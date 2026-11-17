extrapolation_model <- function(a = c(1, 0, 0), b = c(0, 1, 0),
                                cycleLength = 7) {
  probabilities <- rbind(A = a, B = b, D = c(0, 0, 1))
  colnames(probabilities) <- c("A", "B", "D")
  list(matrices = list(All = probabilities),
    periods = data.frame(period = "All", start_day = 0, end_day = Inf),
    metadata = list(states = c("A", "B", "D"), absorbingStates = "D",
      cycleLength = cycleLength))
}

extrapolation_strata <- function(models, values, column = "age_group") {
  registry <- data.frame(stratum_id = names(models), stringsAsFactors = FALSE)
  registry[[column]] <- values
  structure(list(models = models, strata = registry,
    metadata = list(strata = column)), class = "tma_stratified_transitions")
}

test_that("extrapolation switches at the source cutoff and uses relative tail periods", {
  base <- createModelBundle(extrapolation_model(a = c(0, 1, 0), b = c(1, 0, 0)))
  first <- extrapolation_model(a = c(0, 1, 0))$matrices$All
  later <- extrapolation_model(b = c(0, 0, 1))$matrices$All
  identity <- extrapolation_model()$matrices$All
  tail <- composeTransitionStages(list(change = list(First = first, Later = later),
    remain = identity), cycleLength = 7, absorbingStates = "D",
    periodBreaks = c(0, 7, Inf), periodLabels = c("First", "Later"))
  bundle <- extendModelBundle(base, fromDay = 14, transitions = tail,
    source = "Specified two-period external fixture")
  result <- simulateTrajectories(bundle, n = 1, initial = c(A = 1),
    nSteps = 4, terminalStates = "D", seed = 6)
  expect_equal(result$trajectories$state_label, c("A", "B", "A", "B", "D"))
  expect_equal(result$trajectories$elapsed_day, c(0, 7, 14, 21, 28))
  expect_equal(result$trajectories$cycle, 0:4)
  expect_equal(result$trajectories$model_phase,
    c("base", "base", "extrapolation", "extrapolation", "extrapolation"))
  expect_equal(result$transitions$source_day, c(0, 7, 14, 21))
  expect_equal(result$transitions$source_period, c("All", "All", "First", "Later"))
  expect_equal(result$transitions$model_phase,
    c("base", "base", "extrapolation", "extrapolation"))
  expect_equal(result$transitions$model_day, c(0, 7, 0, 7))
  expect_equal(result$transitions$model_source[3:4],
    rep("Specified two-period external fixture", 2))
  expect_equal(result$transitions$destination_model_phase[1:3],
    c("base", "extrapolation", "extrapolation"))
  expect_equal(result$transitions$destination_period[1:3], c("All", "First", "Later"))
  expect_identical(bundle$models, base$models)
})

test_that("tail groups use current covariates independently of the base registry", {
  baseGroups <- list(age_group = list(column = "age", breaks = c(0, 65, Inf),
    labels = c("young", "older")))
  base <- createModelBundle(extrapolation_strata(
    list(base_young = extrapolation_model()), "young"), groups = baseGroups)
  tail <- extrapolation_strata(list(tail65 = extrapolation_model(a = c(0, 1, 0)),
    tail66 = extrapolation_model(b = c(0, 0, 1))), c("age65", "age66plus"))
  tailGroups <- list(age_group = list(column = "age", breaks = c(0, 65, 66, Inf),
    labels = c("below65", "age65", "age66plus")))
  bundle <- extendModelBundle(base, 7, tail, groups = tailGroups,
    source = "Specified age-specific external fixture")
  result <- simulateTrajectories(bundle, data.frame(state_label = "A", age = 64),
    nSteps = 3, terminalStates = "D", seed = 3,
    updateCovariates = function(current, previous) data.frame(
      simulation_id = current$simulation_id, age = current$age + 1))
  expect_equal(result$trajectories$state_label, c("A", "A", "B", "D"))
  expect_equal(result$transitions$source_stratum, c("base_young", "tail65", "tail66"))
  expect_equal(result$trajectories$age_group[1:3], c("young", "age65", "age66plus"))
  expect_equal(result$transitions$model_day, c(0, 0, 7))

  directBase <- createModelBundle(extrapolation_strata(
    list(base_young = extrapolation_model()), "young"))
  direct <- extendModelBundle(directBase, 7, tail, groups = tailGroups,
    source = "Specified age-specific external fixture")
  directResult <- simulateTrajectories(direct,
    data.frame(state_label = "A", age_group = "young", age = 65), nSteps = 2, seed = 3)
  expect_equal(directResult$transitions$source_stratum, c("base_young", "tail65"))
  expect_equal(directResult$trajectories$age_group, c("young", "age65", "age65"))
  expect_false("age_group" %in%
    names(summariseSimulation(directResult)$metadata$derived_group_inputs))

  riskTail <- extrapolation_strata(list(high = extrapolation_model()), "high", "risk_group")
  riskGroups <- list(risk_group = list(column = "risk_score", breaks = c(0, 1, Inf),
    labels = c("low", "high")))
  later <- extendModelBundle(createModelBundle(extrapolation_model()), 14,
    riskTail, groups = riskGroups, source = "Specified risk-specific external fixture")
  early <- simulateTrajectories(later, data.frame(state_label = "A"), nSteps = 1, seed = 2)
  expect_equal(early$trajectories$model_phase, c("base", "base"))
  expect_equal(early$trajectories$elapsed_day, c(0, 7))
  atCutoff <- simulateTrajectories(later,
    data.frame(state_label = "A", elapsed_day = 14, risk_score = 2), nSteps = 1, seed = 2)
  expect_equal(atCutoff$transitions$model_phase, "extrapolation")
  expect_equal(atCutoff$transitions$model_day, 0)
  expect_equal(atCutoff$transitions$source_stratum, "high")
  introduced <- simulateTrajectories(later, data.frame(state_label = "A"),
    nSteps = 3, seed = 2, updateCovariates = function(current, previous) {
      updated <- data.frame(simulation_id = current$simulation_id)
      if (current$elapsed_day >= 14) updated$risk_score <- 2
      updated
    })
  expect_equal(introduced$transitions$source_stratum, c("s1", "s1", "high"))
  expect_equal(introduced$trajectories$risk_score, c(NA_real_, NA_real_, 2, 2))
  expect_error(simulateTrajectories(later, data.frame(state_label = "A"),
    nSteps = 3, seed = 2), "risk_score|Grouping|grouping")

  summariseRisk <- function(model, risk = 2) summariseSimulation(simulateTrajectories(model,
    data.frame(state_label = "A", risk_score = risk), nSteps = 3, seed = 2))
  reference <- summariseRisk(createModelBundle(extrapolation_model()))
  intervention <- summariseRisk(later)
  comparison <- compareScenarios(reference, intervention)
  expect_true("risk_score" %in% comparison$metadata$checked_population_columns)
  expect_equal(comparison$metadata$derived_group_columns_ignored, "risk_group")
  changedRisk <- summariseRisk(later, risk = 3)
  expect_error(compareScenarios(reference, changedRisk), "population|profile|baseline|[Ii]nitial")
  expect_s3_class(compareScenarios(reference, changedRisk, scenarioCovariates = "risk_score"),
    "tma_scenario_comparison")
})

test_that("reached missing tail rows and unsupported tail groups never fall back", {
  base <- createModelBundle(extrapolation_model())
  unavailable <- extendModelBundle(base, 7,
    extrapolation_model(a = rep(NA_real_, 3)), source = "Incomplete external fixture")
  boundary <- simulateTrajectories(unavailable, n = 1, initial = c(A = 1),
    nSteps = 1, seed = 9, transitionEffects = list(no_repair = list(
      from = "A", to = "B", multiplier = 0, redistributeTo = "A", startDay = 7,
      eligible = function(source) stop("Eligibility must not run before support validation"))))
  expect_equal(boundary$population$elapsed_day, 7)
  expect_error(advanceSimulation(boundary), "[Uu]nestimated|unavailable|missing")

  tail <- extrapolation_strata(list(supported = extrapolation_model()), "supported", "risk")
  unsupported <- extendModelBundle(base, 7, tail, source = "Restricted external fixture")
  boundary <- simulateTrajectories(unsupported,
    data.frame(state_label = "A", risk = "absent"), nSteps = 1, seed = 9)
  expect_true(is.na(boundary$population$stratum_id))
  expect_equal(boundary$metadata$unsupported_current, 1)
  expect_error(advanceSimulation(boundary), "supports|covariate|strat")
  expect_error(simulateTrajectories(unsupported,
    data.frame(state_label = "A", elapsed_day = 7), nSteps = 1), "covariate|risk")
})

test_that("extension validates compatibility and preserves supplied model inputs", {
  model <- extrapolation_model()
  base <- createModelBundle(model)
  before <- serialize(list(base, model), NULL)
  bundle <- extendModelBundle(base, 14, model, source = "Specified external fixture")
  expect_s3_class(bundle, "tma_model_bundle")
  expect_identical(serialize(list(base, model), NULL), before)
  for (field in c("models", "strata", "groups", "costEstimates", "costModel")) {
    expect_identical(bundle[[field]], base[[field]])
  }
  for (cutoff in c(0, -7, 1, 7.5, Inf, NA_real_)) {
    expect_error(extendModelBundle(base, cutoff, model, source = "Fixture"),
      "fromDay|cutoff|positive|align|finite|integer")
  }
  expect_error(extendModelBundle(base, 7, model), "source|missing")
  expect_error(extendModelBundle(base, 7, model, source = " "), "source")
  expect_error(extendModelBundle(bundle, 21, model, source = "Second fixture"),
    "already|one|nested|extension|extrapolation")
  expect_error(extendModelBundle(base, 7, bundle, source = "Nested fixture"),
    "bundle|nested|transition|model|extrapolation")
  badStates <- model
  badStates$metadata$states <- c("A", "C", "D")
  dimnames(badStates$matrices$All) <- list(c("A", "C", "D"), c("A", "C", "D"))
  expect_error(extendModelBundle(base, 7, badStates, source = "Fixture"), "state|State")
  badAbsorbing <- model
  badAbsorbing$metadata$absorbingStates <- character()
  expect_error(extendModelBundle(base, 7, badAbsorbing, source = "Fixture"),
    "absorbing|Absorbing")
  expect_error(extendModelBundle(base, 7, extrapolation_model(cycleLength = 1),
    source = "Fixture"), "cycleLength|cycle length")

  history <- data.frame(subject_id = c("fixture-young", "fixture-older"), state_label = "A",
    state_start_date = as.Date("2025-01-01"), state_end_date = as.Date("2025-01-07"),
    age_group = c("young", "older"))
  events <- data.frame(subject_id = history$subject_id, event_date = as.Date("2025-01-01"),
    amount = c(7, 70), currency_concept_id = "EUR")
  costs <- createCostModel(estimateStateCosts(prepareTrajectories(history,
    states = c("A", "B", "D"), absorbingStates = "D"), events, strata = "age_group"),
    unit = "day", currency = "EUR", priceYear = 2025, terminal = "none")
  directCost <- createModelBundle(model, costs = costs)
  ageTail <- extrapolation_strata(list(young = model, older = model), c("young", "older"))
  ageGroups <- list(age_group = list(column = "age", breaks = c(0, 65, Inf),
    labels = c("young", "older")))
  expect_error(extendModelBundle(directCost, 7, ageTail, groups = ageGroups,
    source = "Fixture"), "cost|Cost")

  riskModel <- extrapolation_strata(list(high = model), "high", "risk_group")
  riskGroups <- list(risk_group = list(column = "risk_score", breaks = c(0, Inf),
    labels = "high"))
  scoreModel <- extrapolation_strata(list(high = model), "high", "risk_score")
  scoreGroups <- list(risk_score = list(column = "marker", breaks = c(0, Inf),
    labels = "high"))
  expect_error(extendModelBundle(createModelBundle(riskModel, groups = riskGroups),
    7, scoreModel, groups = scoreGroups, source = "Fixture"), "raw|source|input|group")
  expect_error(extendModelBundle(createModelBundle(scoreModel, groups = scoreGroups),
    7, riskModel, groups = riskGroups, source = "Fixture"), "raw|source|input|group")
})

test_that("terminal entry at the cutoff needs no external model covariates", {
  tail <- extrapolation_strata(list(external = extrapolation_model()), "known", "risk")
  bundle <- extendModelBundle(createModelBundle(extrapolation_model(a = c(0, 0, 1))),
    7, tail, source = "External fixture requiring risk")
  result <- simulateTrajectories(bundle, data.frame(state_label = "A"), nSteps = 5,
    terminalStates = "D", seed = 4)
  expect_equal(result$trajectories$state_label, c("A", "D"))
  expect_equal(result$metadata$steps, 1)
  expect_equal(result$metadata$stop_reason, "all_terminal")
  expect_equal(result$transitions$model_phase, "base")
  expect_identical(advanceSimulation(result, 4), result)
  terminal <- simulateTrajectories(bundle,
    data.frame(state_label = "D", elapsed_day = 14), nSteps = 2, terminalStates = "D")
  expect_equal(terminal$metadata$steps, 0)
  expect_equal(nrow(terminal$transitions), 0)
})

test_that("resume preserves entry, absolute effect and cost clocks, and economic audit metadata", {
  base <- createModelBundle(extrapolation_model())
  bundle <- extendModelBundle(base, 14, extrapolation_model(a = c(0, 1, 0)),
    source = "Specified external fixture")
  entry <- estimateEntryTransitions(data.frame(subject_id = "training-fixture",
    state_label = "B", state_start_date = as.Date("2025-01-01"),
    state_end_date = as.Date("2025-01-07")), entryState = "A", cycleLength = 7,
    states = c("A", "B", "D"), absorbingStates = "D")
  population <- data.frame(subject_id = "simulated-fixture", state_label = "A")
  effects <- list(at_entry = list(from = "A", to = "B", multiplier = 0,
    redistributeTo = "A", applyAt = "entry"),
    later = list(from = "A", to = "B", multiplier = 0, redistributeTo = "A",
      startDay = 7, endDay = 21))
  rules <- list(entry_fee = list(type = "entry", amount = 11),
    setup = list(type = "once", amount = 5, startDay = 7),
    care = list(type = "recurring", amount = 3, startDay = 7, endDay = 21),
    change = list(type = "transition", from = "A", to = "B", amount = 13,
      startDay = 21, endDay = 35))
  run <- function(model, steps) simulateTrajectories(model, population, nSteps = steps,
    entry = entry, transitionEffects = effects, costRules = rules,
    currency = "EUR", priceYear = 2025, terminalStates = "D", seed = 81)
  full <- run(bundle, 4)
  partial <- run(bundle, 1)
  atCutoff <- advanceSimulation(partial, 1)
  resumed <- advanceSimulation(unserialize(serialize(atCutoff, NULL)), 2)
  for (field in c("entry", "population", "trajectories", "transitions", "effects",
                  "costs", "totals", "metadata")) {
    expect_identical(resumed[[field]], full[[field]])
  }
  expect_identical(resumed$.state$rng, full$.state$rng)
  originalEntry <- run(base, 0)$entry
  expect_identical(full$entry$population[names(originalEntry$population)], originalEntry$population)
  expect_identical(full$entry[setdiff(names(full$entry), "population")],
    originalEntry[setdiff(names(originalEntry), "population")])
  expect_equal(full$trajectories$state_label, c("A", "A", "A", "A", "B"))
  expect_equal(full$effects$source_day, c(7, 14))
  expect_equal(full$effects$model_day, c(7, 0))
  expect_equal(full$effects$model_phase, c("base", "extrapolation"))
  expect_equal(full$effects$model_source[2], "Specified external fixture")
  expect_equal(subset(full$costs, component == "entry_fee")$elapsed_day, 0)
  expect_equal(subset(full$costs, component == "setup")$elapsed_day, 7)
  expect_equal(subset(full$costs, component == "care")$elapsed_day, c(7, 14))
  expect_equal(subset(full$costs, component == "change")$elapsed_day, 28)
  summarise <- function(x) summariseSimulation(x, aliveStates = c("A", "B"),
    utilities = c(A = 1, B = 0.5, D = 0), daysPerYear = 7, costDiscountRate = 1)
  summary <- summarise(full)
  expect_identical(summarise(resumed), summary)
  expect_equal(summary$byPatient$cost, 35)
  expect_equal(summary$byPatient$discounted_cost, 16.5625)
  expect_equal(summary$byPatient$qalys, 4.5)
  expect_false(is.null(full$metadata$extrapolation))
  expect_identical(summary$metadata$extrapolation, full$metadata$extrapolation)
  comparison <- compareScenarios(summarise(run(base, 4)), summary)
  expect_identical(comparison$metadata$extrapolation,
    list(reference = NULL, intervention = summary$metadata$extrapolation))
})
