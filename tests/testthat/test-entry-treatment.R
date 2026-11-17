entry_treatment_estimate <- function(to = "B") {
  history <- data.frame(subject_id = paste0("training-", seq_along(to)),
    state_label = to, state_start_date = as.Date("2025-01-01"),
    state_end_date = as.Date("2025-01-07"))
  estimateEntryTransitions(history, entryState = "A", cycleLength = 7,
    states = c("A", "B", "D"), absorbingStates = "D")
}

entry_treatment_bundle <- function(a = c(1, 0, 0), b = c(0, 1, 0)) {
  probabilities <- rbind(A = a, B = b, D = c(0, 0, 1))
  colnames(probabilities) <- c("A", "B", "D")
  createModelBundle(list(matrices = list(All = probabilities),
    periods = data.frame(period = "All", start_day = 0, end_day = Inf),
    metadata = list(states = c("A", "B", "D"), absorbingStates = "D", cycleLength = 7)))
}

entry_treatment_rule <- function(applyAt = "entry", ...) {
  c(list(from = "A", to = "B", multiplier = 0, redistributeTo = "A",
    applyAt = applyAt), list(...))
}

test_that("entry effects preserve the fitted matrix and audit changed sampling probabilities", {
  entry <- entry_treatment_estimate()
  result <- simulateTrajectories(entry_treatment_bundle(), n = 1, initial = c(A = 1),
    nSteps = 0, entry = entry, terminalStates = "D", seed = 4,
    transitionEffects = list(prevention = entry_treatment_rule()))
  expect_equal(result$population$state_label, "A")
  expect_equal(result$entry$transitions$baseline_transition_probability, 0)
  expect_equal(result$entry$transitions$transition_probability, 1)
  expect_equal(result$entry$transitions$effect, "prevention")
  expect_equal(result$entry$effects$source_period, "entry")
  expect_equal(result$entry$effects$baseline_probability, 1)
  expect_equal(result$entry$effects$modified_probability, 0)
  expect_equal(result$entry$effects$modified_redistributed_probability, 1)
  expect_identical(result$entry$matrix, entry$matrix)
  expect_equal(result$entry$effectRules$prevention$applyAt, "entry")
  expect_s3_class(summariseSimulation(result), "tma_simulation_summary")
  altered <- result
  altered$entry$transitions$source_day <- 7
  expect_error(summariseSimulation(altered), "clock|zero")
  altered <- result
  altered$entry$effects$modified_probability <- 0.1
  expect_error(summariseSimulation(altered), "audit|probabilit")

  mixed <- simulateTrajectories(entry_treatment_bundle(), n = 30, initial = c(A = 1),
    nSteps = 0, entry = entry_treatment_estimate(c("A", "B", "D")),
    terminalStates = "D", seed = 14,
    transitionEffects = list(prevention = entry_treatment_rule()))
  deaths <- subset(mixed$entry$transitions, destination_state == "D")
  expect_gt(nrow(deaths), 0)
  expect_equal(deaths$transition_probability, rep(1 / 3, nrow(deaths)))
  expect_equal(deaths$baseline_transition_probability, deaths$transition_probability)
  expect_equal(mixed$entry$effects$modified_probability +
    mixed$entry$effects$modified_redistributed_probability, rep(2 / 3, 30))
  expect_s3_class(summariseSimulation(mixed), "tma_simulation_summary")
})

test_that("entry and transition eligibility are separate phases with source-day windows", {
  entryDays <- stepDays <- numeric()
  entryRule <- entry_treatment_rule(eligible = function(p) {
    entryDays <<- c(entryDays, p$elapsed_day)
    rep(TRUE, nrow(p))
  })
  stepRule <- entry_treatment_rule("transitions", eligible = function(p) {
    stepDays <<- c(stepDays, p$elapsed_day)
    rep(TRUE, nrow(p))
  })
  result <- simulateTrajectories(entry_treatment_bundle(), n = 1, initial = c(A = 1),
    entry = entry_treatment_estimate("A"), nSteps = 2, seed = 3,
    transitionEffects = list(at_entry = entryRule, later = stepRule))
  expect_equal(entryDays, 0)
  expect_equal(stepDays, c(0, 7))
  expect_equal(result$entry$effects$effect, "at_entry")
  expect_equal(result$effects$effect, rep("later", 2))

  bothDays <- numeric()
  both <- simulateTrajectories(entry_treatment_bundle(), n = 1, initial = c(A = 1),
    entry = entry_treatment_estimate("A"), nSteps = 2, seed = 3,
    transitionEffects = list(treatment = entry_treatment_rule("both", endDay = 7,
      eligible = function(p) {
        bothDays <<- c(bothDays, p$elapsed_day)
        rep(TRUE, nrow(p))
      })))
  expect_equal(bothDays, c(0, 0))
  expect_equal(both$entry$effects$source_day, 0)
  expect_equal(both$effects$source_day, 0)
  expect_equal(both$metadata$steps, 2)
})

test_that("prepaid entry fees survive entry death and add no extra health cycle", {
  bundle <- entry_treatment_bundle()
  entry <- entry_treatment_estimate("D")
  population <- data.frame(subject_id = c("eligible", "already-dead"),
    state_label = c("A", "D"), treated = c(TRUE, NA))
  feeCalls <- character()
  fee <- list(type = "entry", amount = 100, states = "A", eligible = function(p) {
    feeCalls <<- c(feeCalls, p$state_label)
    p$treated
  })
  care <- list(type = "recurring", amount = 5)
  run <- function(rules, effects = NULL) simulateTrajectories(bundle, population,
    entry = entry, nSteps = 0, terminalStates = "D", seed = 2,
    costRules = rules, currency = "EUR", priceYear = 2026, transitionEffects = effects)
  summarise <- function(x) summariseSimulation(x, aliveStates = c("A", "B"),
    utilities = c(A = 1, B = 0.5, D = 0), daysPerYear = 7)
  unsuccessful <- run(list(treatment = fee, care = care))
  expect_equal(feeCalls, "A")
  expect_equal(unsuccessful$costs$simulation_id, "sim_1")
  expect_equal(unsuccessful$costs$type, "entry")
  expect_equal(unsuccessful$costs$state_label, "A")
  expect_equal(unsuccessful$costs$elapsed_day, 0)
  expect_equal(summarise(unsuccessful)$byPatient$cost, c(100, 0))
  expect_equal(summarise(unsuccessful)$byPatient$qalys, c(0, 0))

  prevention <- list(from = "A", to = "D", multiplier = 0,
    redistributeTo = "A", applyAt = "entry")
  reference <- summarise(run(list(care = care)))
  successful <- run(list(treatment = fee, care = care), list(prevention = prevention))
  intervention <- summarise(successful)
  expect_equal(intervention$byPatient$labelled_cycles, c(1, 1))
  expect_equal(intervention$byPatient$qalys, c(1, 0))
  comparison <- compareScenarios(reference, intervention)
  expect_equal(subset(comparison$outcomes, metric == "cost")$incremental_total, 105)
  expect_equal(subset(comparison$outcomes, metric == "qalys")$incremental_total, 1)

  altered <- unsuccessful
  altered$costs$state_label <- "D"
  expect_error(summarise(altered), "[Ee]ntry|pre.entry|snapshot|state")
})

test_that("entry treatment and fees are not repeated on resume or mutate fitted inputs", {
  entry <- entry_treatment_estimate(c("A", "B"))
  bundle <- entry_treatment_bundle(a = c(0.7, 0.3, 0), b = c(0.4, 0.6, 0))
  originals <- serialize(list(entry = entry, bundle = bundle), NULL)
  population <- data.frame(subject_id = paste0("source-", 1:8), state_label = "A", score = 0)
  rule <- entry_treatment_rule("both")
  rule$multiplier <- 0.5
  run <- function(steps) simulateTrajectories(bundle, population, entry = entry,
    nSteps = steps, seed = 41, transitionEffects = list(prevention = rule),
    costRules = list(treatment = list(type = "entry", amount = 100)),
    currency = "EUR", priceYear = 2026,
    updateCovariates = function(current, previous) {
      data.frame(simulation_id = current$simulation_id,
        score = current$score + stats::runif(nrow(current)))
    })
  set.seed(901)
  callerSeed <- .Random.seed
  full <- run(4)
  partial <- run(0)
  resumed <- advanceSimulation(unserialize(serialize(partial, NULL)), 4)
  for (field in c("entry", "trajectories", "transitions", "effects", "costs", "totals")) {
    expect_identical(resumed[[field]], full[[field]])
  }
  expect_identical(resumed$.state$rng, full$.state$rng)
  expect_identical(.Random.seed, callerSeed)
  expect_equal(nrow(resumed$costs), 8)
  expect_equal(resumed$costs$elapsed_day, rep(0, 8))
  expect_identical(serialize(list(entry = entry, bundle = bundle), NULL), originals)
})

test_that("entry treatment requires an entry model and rejects ambiguous phase rules", {
  bundle <- entry_treatment_bundle()
  run <- function(effects, entry = NULL) simulateTrajectories(bundle, n = 1,
    initial = c(A = 1), nSteps = 0, entry = entry, transitionEffects = effects, seed = 5)
  for (phase in c("entry", "both")) {
    expect_error(run(list(treatment = entry_treatment_rule(phase))), "[Ee]ntry")
  }
  expect_error(simulateTrajectories(bundle, n = 1, initial = c(A = 1), nSteps = 0,
    costRules = list(treatment = list(type = "entry", amount = 100)),
    currency = "EUR", priceYear = 2026), "[Ee]ntry")
  expect_error(run(list(treatment = entry_treatment_rule("unknown")),
    entry_treatment_estimate()), "applyAt|entry|transitions")
  expect_error(run(list(first = entry_treatment_rule(), second = entry_treatment_rule("both")),
    entry_treatment_estimate()), "[Oo]verlapping")
})
