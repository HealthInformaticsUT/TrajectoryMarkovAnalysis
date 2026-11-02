effect_simulation_model <- function(a = c(0.6, 0.3, 0.1)) {
  states <- c("A", "B", "D")
  matrix <- rbind(A = a, B = c(0, 1, 0), D = c(0, 0, 1))
  colnames(matrix) <- states
  list(matrices = list(All = matrix),
    periods = data.frame(period = "All", start_day = 0, end_day = Inf),
    metadata = list(states = states, absorbingStates = "D", cycleLength = 7))
}

effect_simulation_rule <- function(multiplier = 0) {
  list(from = "A", to = "B", multiplier = multiplier, redistributeTo = "A")
}

test_that("transition effects use source eligibility and source-day windows before drawing", {
  model <- effect_simulation_model(c(1, 0, 0))
  model$matrices <- list(Early = model$matrices$All,
    Later = effect_simulation_model(c(0, 1, 0))$matrices$All)
  model$periods <- data.frame(period = c("Early", "Later"), start_day = c(0, 7),
    end_day = c(7, Inf))
  bundle <- createModelBundle(model)
  original <- bundle
  rule <- effect_simulation_rule()
  rule$startDay <- 7
  rule$endDay <- 14
  rule$eligible <- function(source) {
    expect_equal(source$elapsed_day, rep(7, nrow(source)))
    source$treated
  }
  population <- data.frame(state_label = c("A", "A", "D"), treated = c(TRUE, FALSE, TRUE))
  result <- simulateTrajectories(bundle, population, nSteps = 3, terminalStates = "D",
    transitionEffects = list(treatment = rule), seed = 37)
  treated <- result$trajectories[result$trajectories$simulation_id == "sim_1", ]
  untreated <- result$trajectories[result$trajectories$simulation_id == "sim_2", ]
  expect_equal(treated$state_label, c("A", "A", "A", "B"))
  expect_equal(untreated$state_label, c("A", "A", "B", "B"))
  expect_equal(nrow(result$effects), 1)
  expect_equal(result$effects$simulation_id, "sim_1")
  expect_equal(result$effects$source_day, 7)
  expect_equal(result$effects$source_period, "Later")
  expect_equal(result$effects$baseline_probability, 1)
  expect_equal(result$effects$modified_probability, 0)
  expect_equal(result$effects$baseline_redistributed_probability, 0)
  expect_equal(result$effects$modified_redistributed_probability, 1)
  draw <- result$transitions[!is.na(result$transitions$effect), ]
  expect_equal(draw$destination_state, "A")
  expect_equal(draw$baseline_transition_probability, 0)
  expect_equal(draw$transition_probability, 1)
  expect_false("sim_3" %in% result$effects$simulation_id)
  expect_identical(bundle, original)
  expect_identical(result$.config$bundle, original)

  rule$eligible <- function(source) stop("Eligibility must not run before the first transition")
  initialOnly <- simulateTrajectories(bundle, population, nSteps = 0,
    transitionEffects = list(treatment = rule), seed = 37)
  expect_equal(nrow(initialOnly$effects), 0)
  expect_equal(nrow(initialOnly$transitions), 0)
  expect_equal(initialOnly$population$state_label, population$state_label)
})

test_that("effects use the probability row of the current age stratum", {
  models <- structure(list(models = list(s1 = effect_simulation_model(c(1, 0, 0)),
    s2 = effect_simulation_model(c(0, 1, 0))),
    strata = data.frame(stratum_id = c("s1", "s2"), age_group = c("young", "older")),
    metadata = list(strata = "age_group")), class = "tma_stratified_transitions")
  groups <- list(age_group = list(column = "age", breaks = c(0, 65, Inf),
    labels = c("young", "older")))
  bundle <- createModelBundle(models, groups = groups)
  result <- simulateTrajectories(bundle, data.frame(state_label = "A", age = 64),
    nSteps = 2, transitionEffects = list(treatment = effect_simulation_rule()), seed = 9,
    updateCovariates = function(current, previous) data.frame(
      simulation_id = current$simulation_id, age = current$age + 1))
  expect_equal(result$trajectories$age_group, c("young", "older", "older"))
  expect_equal(result$trajectories$state_label, rep("A", 3))
  expect_equal(result$effects$source_stratum, c("s1", "s2"))
  expect_equal(result$effects$baseline_probability, c(0, 1))
  expect_equal(result$effects$modified_probability, c(0, 0))
})

test_that("identity effects preserve sampled histories, costs and RNG while empty rules preserve schema", {
  bundle <- createModelBundle(effect_simulation_model())
  population <- data.frame(state_label = rep("A", 40), treated = rep(c(TRUE, FALSE), 20))
  costs <- list(stay = list(type = "recurring", states = "A", amount = 2))
  baseline <- simulateTrajectories(bundle, population, nSteps = 4, terminalStates = "D",
    costRules = costs, currency = "EUR", priceYear = 2025, seed = 28)
  empty <- simulateTrajectories(bundle, population, nSteps = 4, terminalStates = "D",
    costRules = costs, currency = "EUR", priceYear = 2025, seed = 28, transitionEffects = list())
  rule <- effect_simulation_rule(1)
  rule$eligible <- function(source) source$treated
  identity <- simulateTrajectories(bundle, population, nSteps = 4, terminalStates = "D",
    costRules = costs, currency = "EUR", priceYear = 2025, seed = 28,
    transitionEffects = list(identity = rule))
  expect_identical(empty, baseline)
  expect_false("effects" %in% names(baseline))
  expect_false("transitionEffects" %in% names(baseline$.config))
  expect_identical(identity$trajectories, baseline$trajectories)
  expect_identical(identity$population, baseline$population)
  expect_identical(identity$transitions[names(baseline$transitions)], baseline$transitions)
  expect_identical(identity$costs, baseline$costs)
  expect_identical(identity$.state$rng, baseline$.state$rng)
  expect_true(nrow(identity$effects) > 0)
  expect_equal(identity$effects$baseline_probability, identity$effects$modified_probability)
})

test_that("serialized resume retains transition effects, charge memory and private RNG", {
  bundle <- createModelBundle(effect_simulation_model())
  population <- data.frame(state_label = rep("A", 40), score = 0)
  rule <- effect_simulation_rule(0.4)
  rule$eligible <- function(source) source$score >= 1
  update <- function(current, previous) data.frame(simulation_id = current$simulation_id,
    score = current$score + stats::runif(nrow(current)))
  costs <- list(treatment = list(type = "once", amount = 5),
    stay = list(type = "recurring", states = "A", amount = 2))
  full <- simulateTrajectories(bundle, population, nSteps = 5, terminalStates = "D",
    transitionEffects = list(treatment = rule), updateCovariates = update,
    costRules = costs, currency = "EUR", priceYear = 2025, seed = 24)
  first <- simulateTrajectories(bundle, population, nSteps = 2, terminalStates = "D",
    transitionEffects = list(treatment = rule), updateCovariates = update,
    costRules = costs, currency = "EUR", priceYear = 2025, seed = 24)
  restored <- unserialize(serialize(first, NULL))
  resumed <- advanceSimulation(restored, 3)
  expect_identical(resumed$trajectories, full$trajectories)
  expect_identical(resumed$transitions, full$transitions)
  expect_identical(resumed$effects, full$effects)
  expect_identical(resumed$costs, full$costs)
  expect_identical(resumed$totals, full$totals)
  expect_identical(resumed$.state$rng, full$.state$rng)
  expect_identical(first$effects, restored$effects)
  expect_equal(names(resumed$.config$transitionEffects), "treatment")
  restored$.config$transitionEffects$treatment$multiplier <- -1
  expect_error(advanceSimulation(restored), "multiplier")
})

test_that("conflicting and impossible effects fail rather than silently changing the rule", {
  bundle <- createModelBundle(effect_simulation_model())
  population <- data.frame(state_label = "A")
  rule <- effect_simulation_rule()
  expect_error(simulateTrajectories(bundle, population,
    transitionEffects = list(first = rule, second = rule)), "conflict|multiple|one.*rule|More than")
  invalid <- effect_simulation_rule(4)
  expect_error(simulateTrajectories(bundle, population,
    transitionEffects = list(invalid = invalid)), "invalid.*sim_1.*s1.*All.*probabilit")
  invalid$to <- "unknown"
  expect_error(simulateTrajectories(bundle, population, nSteps = 0,
    transitionEffects = list(invalid = invalid)), "state|known")
})

test_that("composed models retain their stages when effects modify the net cycle row", {
  stage <- effect_simulation_model()$matrices$All
  composed <- composeTransitionStages(list(stage = stage), cycleLength = 7, absorbingStates = "D")
  bundle <- createModelBundle(composed)
  result <- simulateTrajectories(bundle, data.frame(state_label = "A"), nSteps = 1,
    transitionEffects = list(treatment = effect_simulation_rule()), seed = 13)
  expect_equal(result$effects$baseline_probability, 0.3)
  expect_equal(result$effects$modified_probability, 0)
  expect_identical(result$.config$bundle$models$s1$stageMatrices, composed$stageMatrices)
})
