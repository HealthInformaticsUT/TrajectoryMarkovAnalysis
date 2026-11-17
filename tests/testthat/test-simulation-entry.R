entry_simulation_estimate <- function(from = c("A", "B"), to = c("B", "D")) {
  history <- data.frame(subject_id = paste0("entry-training-", seq_along(from)),
    state_label = to, state_start_date = as.Date("2025-01-01"),
    state_end_date = as.Date("2025-01-07"))
  estimateEntryTransitions(history,
    data.frame(subject_id = history$subject_id, entry_state = from),
    cycleLength = 7, states = c("A", "B", "D"), absorbingStates = "D")
}

entry_simulation_bundle <- function(a = c(1, 0, 0), b = c(1, 0, 0), costs = NULL) {
  states <- c("A", "B", "D")
  probabilities <- rbind(A = a, B = b, D = c(0, 0, 1))
  colnames(probabilities) <- states
  createModelBundle(list(matrices = list(All = probabilities),
    periods = data.frame(period = "All", start_day = 0, end_day = Inf),
    metadata = list(states = states, absorbingStates = "D", cycleLength = 7)),
    costs = costs)
}

test_that("entry occurs once before the first recorded label and retains explicit baselines", {
  entry <- entry_simulation_estimate()
  bundle <- entry_simulation_bundle()
  population <- data.frame(subject_id = c("source-a", "source-b", "source-d"),
    state_label = c("A", "B", "D"), age = c(40, 60, 80))
  zero <- simulateTrajectories(bundle, population, nSteps = 0, entry = entry,
    terminalStates = "D", seed = 5)
  expect_equal(zero$trajectories$state_label, c("B", "D", "D"))
  expect_equal(zero$trajectories$elapsed_day, rep(0, 3))
  expect_equal(zero$trajectories$cycle, rep(0, 3))
  expect_equal(zero$metadata$steps, 0)
  expect_equal(nrow(zero$transitions), 0)
  expect_equal(zero$entry$population$state_label, population$state_label)
  expect_equal(zero$entry$population$source_subject_id, population$subject_id)
  expect_equal(zero$population$source_subject_id, population$subject_id)
  expect_equal(zero$entry$population$age, population$age)
  expect_identical(zero$entry$matrix, entry$matrix)
  expect_equal(zero$entry$transitions[c("simulation_id", "source_state",
    "destination_state", "transition_probability")],
    data.frame(simulation_id = c("sim_1", "sim_2"), source_state = c("A", "B"),
      destination_state = c("B", "D"), transition_probability = c(1, 1)))
  expect_false(any(entry$subjects$subject_id %in%
    as.character(unlist(zero$entry$metadata, use.names = FALSE))))
  expect_false(any(c("subjects", "eligible_subject_ids") %in% names(zero$entry$metadata)))

  later <- advanceSimulation(zero, 2)
  first <- later$trajectories[later$trajectories$simulation_id == "sim_1", ]
  expect_equal(first$state_label, c("B", "A", "A"))
  expect_equal(first$elapsed_day, c(0, 7, 14))
  expect_equal(first$cycle, c(0, 1, 2))
  expect_equal(later$metadata$steps, 2)
  expect_identical(later$entry, zero$entry)
  drawn <- simulateTrajectories(bundle, n = 1, initial = c(A = 1), nSteps = 0,
    entry = entry, seed = 5)
  expect_equal(drawn$entry$population$state_label, "A")
  expect_equal(drawn$population$state_label, "B")
  expect_error(simulateTrajectories(bundle, entry = entry), "population|initial")
  without <- simulateTrajectories(bundle, population, nSteps = 0, seed = 5)
  expect_identical(without, simulateTrajectories(bundle, population,
    nSteps = 0, seed = 5, entry = NULL))
})

test_that("entry charges post-entry labels and actual changes once, including estimated terminal costs", {
  history <- data.frame(subject_id = c("cost-a", "cost-b"), state_label = c("A", "B"),
    state_start_date = as.Date("2025-01-01"), state_end_date = as.Date("2025-01-07"))
  prepared <- prepareTrajectories(history, cycleLength = 7,
    states = c("A", "B", "D"), absorbingStates = "D")
  events <- data.frame(subject_id = history$subject_id, event_date = as.Date("2025-01-01"),
    amount = c(70, 140), currency_concept_id = "EUR")
  costs <- createCostModel(estimateStateCosts(prepared, events, terminalCostMap = c(D = "B")),
    unit = "cycle", cycleLength = 7, currency = "EUR", priceYear = 2025, terminal = "entry")
  rules <- list(setup = list(type = "once", amount = 10),
    care = list(type = "recurring", amount = 2),
    baseline = list(type = "recurring", states = "A", endDay = 7, amount = 99),
    change = list(type = "transition", from = "A", to = "B", amount = 7),
    terminal = list(type = "transition", from = "B", to = "D", amount = 30))
  zero <- simulateTrajectories(entry_simulation_bundle(costs = costs),
    data.frame(state_label = c("A", "B", "D")), entry = entry_simulation_estimate(),
    nSteps = 0, terminalStates = "D", costRules = rules, seed = 4)
  learned <- subset(zero$costs, component == "healthcare")
  expect_equal(learned$simulation_id, c("sim_1", "sim_2"))
  expect_equal(learned$state_label, c("B", "D"))
  expect_equal(learned$amount, c(140, 140))
  expect_true(all(zero$costs$elapsed_day == 0 & zero$costs$cycle == 0))
  expect_equal(subset(zero$costs, component %in% c("setup", "care"))$simulation_id,
    c("sim_1", "sim_1"))
  expect_equal(subset(zero$costs, component == "change")$amount, 7)
  expect_equal(subset(zero$costs, component == "terminal")$simulation_id, "sim_2")
  expect_false(any(zero$costs$component == "baseline"))
  expect_false(any(zero$costs$simulation_id == "sim_3"))
  resumed <- advanceSimulation(unserialize(serialize(zero, NULL)), 2)
  expect_equal(sum(resumed$costs$component == "setup"), 1)
  expect_equal(sum(resumed$costs$component == "terminal"), 1)
  expect_equal(sum(resumed$costs$component == "change"), 1)
  expect_equal(subset(resumed$costs, component == "care")$elapsed_day, c(0, 7, 14))
  expect_equal(sum(subset(resumed$costs, component == "healthcare")$amount), 420)
  expect_identical(advanceSimulation(zero, 0), zero)
})

test_that("entry leaves step callbacks and effects for adjacent transitions and preserves resumed RNG", {
  bundle <- entry_simulation_bundle()
  fail <- function(...) stop("No step callback may run at entry")
  zero <- simulateTrajectories(bundle, n = 1, initial = c(A = 1), nSteps = 0,
    entry = entry_simulation_estimate(), updateCovariates = fail, seed = 7,
    transitionEffects = list(treatment = list(from = "A", to = "B", multiplier = 0,
      redistributeTo = "A", eligible = fail)))
  expect_equal(zero$population$state_label, "B")
  expect_equal(nrow(zero$effects), 0)
  affected <- simulateTrajectories(bundle, n = 1, initial = c(A = 1), nSteps = 1,
    entry = entry_simulation_estimate(), seed = 7,
    transitionEffects = list(treatment = list(from = "B", to = "A", multiplier = 0,
      redistributeTo = "B")))
  expect_equal(affected$trajectories$state_label, c("B", "B"))
  expect_equal(affected$effects$source_day, 0)

  randomEntry <- entry_simulation_estimate(from = c("A", "A"), to = c("A", "B"))
  randomBundle <- entry_simulation_bundle(a = c(0.7, 0.3, 0), b = c(0.4, 0.6, 0))
  population <- data.frame(subject_id = paste0("source-", 1:12), state_label = "A", score = 0)
  update <- function(current, previous) {
    if (any(current$elapsed_day <= 0)) stop("Covariates updated during entry")
    data.frame(simulation_id = current$simulation_id,
      score = current$score + stats::runif(nrow(current)))
  }
  set.seed(818)
  callerSeed <- .Random.seed
  full <- simulateTrajectories(randomBundle, population, entry = randomEntry,
    nSteps = 4, updateCovariates = update, seed = 19)
  partial <- simulateTrajectories(randomBundle, population, entry = randomEntry,
    nSteps = 0, updateCovariates = update, seed = 19)
  resumed <- advanceSimulation(unserialize(serialize(partial, NULL)), 4)
  for (field in c("entry", "population", "trajectories", "transitions")) {
    expect_identical(resumed[[field]], full[[field]])
  }
  expect_identical(resumed$.state$rng, full$.state$rng)
  expect_identical(.Random.seed, callerSeed)
})

test_that("entry validates zero clocks, compatible matrices and every possible source row before sampling", {
  bundle <- entry_simulation_bundle()
  entry <- entry_simulation_estimate(from = "A", to = "A")
  for (clock in list(c(cycle = 1, elapsed_day = 0), c(cycle = 0, elapsed_day = 7))) {
    population <- data.frame(state_label = "A", cycle = clock[["cycle"]],
      elapsed_day = clock[["elapsed_day"]])
    expect_error(simulateTrajectories(bundle, population, entry = entry, nSteps = 0),
      "zero|cycle|elapsed_day")
  }
  profiles <- data.frame(state_label = c("A", "B"))
  expect_error(simulateTrajectories(bundle, profiles, n = 1, nSteps = 0, entry = entry,
    seed = 1), "[Uu]nestimated|entry.*B|B.*entry")
  expect_error(simulateTrajectories(bundle, n = 1, initial = c(A = 0.999, B = 0.001),
    nSteps = 0, entry = entry, seed = 1), "[Uu]nestimated|entry.*B|B.*entry")
  expect_error(simulateTrajectories(bundle, n = 1,
    initial = data.frame(state_label = c("A", "B"), probability = c(0.999, 0.001)),
    nSteps = 0, entry = entry, seed = 1), "[Uu]nestimated|entry.*B|B.*entry")
  expect_s3_class(simulateTrajectories(bundle, n = 1, initial = c(A = 1, B = 0),
    nSteps = 0, entry = entry, seed = 1), "tma_simulation")
  bad <- entry
  bad$metadata$cycleLength <- 1
  expect_error(simulateTrajectories(bundle, profiles[1, , drop = FALSE], entry = bad),
    "cycleLength|cycle length")
  bad <- entry
  bad$metadata$absorbingStates <- character()
  expect_error(simulateTrajectories(bundle, profiles[1, , drop = FALSE], entry = bad),
    "absorbing|roles")
  bad <- entry
  bad$matrix["A", ] <- c(0.2, 0.2, 0.2)
  expect_error(simulateTrajectories(bundle, profiles[1, , drop = FALSE], entry = bad),
    "sum|probabilit")
  expect_error(simulateTrajectories(bundle, profiles[1, , drop = FALSE], entry = list()),
    "entry|Entry")
})

test_that("entry summaries compare shared pre-entry profiles even when first labels differ", {
  bundle <- entry_simulation_bundle()
  population <- data.frame(subject_id = "source", state_label = "A", age = 60)
  summarise <- function(simulation) summariseSimulation(simulation,
    aliveStates = c("A", "B"), utilities = c(A = 0.5, B = 1, D = 0), daysPerYear = 7)
  referenceSimulation <- simulateTrajectories(bundle, population, nSteps = 0,
    entry = entry_simulation_estimate(from = "A", to = "A"), terminalStates = "D", seed = 3)
  interventionSimulation <- simulateTrajectories(bundle, population, nSteps = 0,
    entry = entry_simulation_estimate(from = c("A", "B"), to = c("B", "B")),
    terminalStates = "D", seed = 3)
  reference <- summarise(referenceSimulation)
  intervention <- summarise(interventionSimulation)
  expect_identical(intervention$initialPopulation, interventionSimulation$entry$population)
  expect_equal(reference$metadata$initial_population_basis, "pre_entry")
  expect_equal(intervention$metadata$initial_population_basis, "pre_entry")
  expect_equal(intervention$byPatient$labelled_cycles, 1)
  expect_equal(intervention$byPatient$qalys, 1)
  comparison <- compareScenarios(reference, intervention)
  expect_equal(subset(comparison$outcomes, metric == "qalys")$incremental_total, 0.5)
  changed <- summarise(simulateTrajectories(bundle, transform(population, state_label = "B"),
    nSteps = 0, entry = entry_simulation_estimate(from = "B", to = "B"),
    terminalStates = "D", seed = 3))
  expect_error(compareScenarios(intervention, changed), "baseline|[Ii]nitial|profile")
  without <- summarise(simulateTrajectories(bundle, population, nSteps = 0,
    terminalStates = "D", seed = 3))
  expect_equal(without$metadata$initial_population_basis, "first_label")
  expect_error(compareScenarios(reference, without), "basis|pre.entry|first.label")
})
