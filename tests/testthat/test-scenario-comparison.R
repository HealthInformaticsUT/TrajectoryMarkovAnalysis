comparison_summary <- function(destination = "A", amount = 10, nSteps = 1,
                                 population = data.frame(state_label = "A"),
                                 utilities = c(A = 0.5, B = 1, D = 0),
                                 aliveStates = c("A", "B"), rules = NULL,
                                 costDiscountRate = 0, healthDiscountRate = 0,
                                 daysPerYear = 7, currency = "EUR", priceYear = 2025,
                                 stateOrder = c("A", "B", "D"), stopWhen = NULL,
                                 terminalStates = "D") {
  states <- c("A", "B", "D")
  matrix <- rbind(A = as.numeric(states == destination), B = c(0, 1, 0), D = c(0, 0, 1))
  colnames(matrix) <- states
  matrix <- matrix[stateOrder, stateOrder, drop = FALSE]
  bundle <- createModelBundle(list(matrices = list(All = matrix),
    periods = data.frame(period = "All", start_day = 0, end_day = Inf),
    metadata = list(states = stateOrder, absorbingStates = "D", cycleLength = 7)))
  if (is.null(rules) && !is.null(amount)) rules <- list(care = list(type = "once", amount = amount))
  simulation <- simulateTrajectories(bundle, population, nSteps = nSteps,
    terminalStates = terminalStates, costRules = rules, currency = currency, priceYear = priceYear,
    stopWhen = stopWhen, seed = 11)
  summariseSimulation(simulation, aliveStates = aliveStates, utilities = utilities,
    costDiscountRate = costDiscountRate, healthDiscountRate = healthDiscountRate,
    daysPerYear = daysPerYear)
}

comparison_ce <- function(result, outcome = "qalys") {
  result$costEffectiveness[result$costEffectiveness$outcome == outcome, , drop = FALSE]
}

test_that("comparison reports closed-form increments and discounted per-patient ratios", {
  population <- data.frame(subject_id = c("one", "two"), state_label = "A")
  reference <- comparison_summary("D", nSteps = 2, population = population,
    rules = list(standard_setup = list(type = "once", amount = 10),
                 care = list(type = "recurring", amount = 2)),
    costDiscountRate = 1, healthDiscountRate = 1)
  intervention <- comparison_summary("A", nSteps = 2, population = population,
    rules = list(program = list(type = "once", amount = 20),
                 care = list(type = "recurring", amount = 2)),
    costDiscountRate = 1, healthDiscountRate = 1)
  original <- serialize(list(reference, intervention), NULL)
  set.seed(917)
  seed <- .Random.seed
  kind <- RNGkind()
  result <- compareScenarios(reference, intervention)
  expected <- data.frame(
    metric = c("cost", "discounted_cost", "life_years", "discounted_life_years", "qalys", "discounted_qalys"),
    reference_total = c(24, 24, 2, 2, 1, 1),
    intervention_total = c(52, 47, 6, 3.5, 3, 1.75),
    incremental_total = c(28, 23, 4, 1.5, 2, 0.75),
    reference_per_patient = c(12, 12, 1, 1, 0.5, 0.5),
    intervention_per_patient = c(26, 23.5, 3, 1.75, 1.5, 0.875),
    incremental_per_patient = c(14, 11.5, 2, 0.75, 1, 0.375))
  expect_equal(result$outcomes, expected)
  expect_equal(result$costEffectiveness$outcome, c("life_years", "qalys"))
  expect_equal(result$costEffectiveness$basis, rep("discounted", 2))
  expect_equal(result$costEffectiveness$incremental_cost, c(11.5, 11.5))
  expect_equal(result$costEffectiveness$incremental_effect, c(0.75, 0.375))
  expect_equal(result$costEffectiveness$icer, c(46 / 3, 92 / 3))
  expect_true(all(result$costEffectiveness$status == "higher_cost_more_effective"))
  setup <- subset(result$costs, component == "standard_setup" & metric == "cost")
  expect_equal(setup$reference_total, 20)
  expect_equal(setup$intervention_total, 0)
  program <- subset(result$costs, component == "program" & metric == "discounted_cost")
  expect_equal(program$reference_total, 0)
  expect_equal(program$intervention_total, 40)
  care <- subset(result$costs, component == "care" & metric == "discounted_cost")
  expect_equal(care$incremental_total, 3)
  expect_equal(care$incremental_per_patient, 1.5)
  expect_identical(serialize(list(reference, intervention), NULL), original)
  expect_identical(.Random.seed, seed)
  expect_identical(RNGkind(), kind)
})

test_that("cost-effectiveness signs are classified without misleading quadrant ratios", {
  summaries <- list(A8 = comparison_summary("A", 8), A10 = comparison_summary("A", 10),
    A12 = comparison_summary("A", 12), B8 = comparison_summary("B", 8),
    B10 = comparison_summary("B", 10), B12 = comparison_summary("B", 12))
  cases <- data.frame(
    reference = c("A10", "A10", "B10", "B10", "A10", "B10", "A10", "A10", "A10"),
    intervention = c("B12", "B8", "A12", "A8", "B10", "A10", "A8", "A12", "A10"),
    status = c("higher_cost_more_effective", "dominant", "dominated", "lower_cost_less_effective",
      "equal_cost_more_effective", "equal_cost_less_effective", "equal_effect_lower_cost",
      "equal_effect_higher_cost", "no_difference"))
  results <- lapply(seq_len(nrow(cases)), function(i) comparison_ce(compareScenarios(
    summaries[[cases$reference[i]]], summaries[[cases$intervention[i]]])))
  expect_equal(vapply(results, function(x) x$status, character(1)), cases$status)
  expect_equal(vapply(results, function(x) x$icer, numeric(1)), c(4, NA, NA, NA, 0, NA, NA, NA, NA))
  tiny <- comparison_ce(compareScenarios(comparison_summary(amount = 0),
                                         comparison_summary(amount = 1e-12)))
  expect_equal(tiny$status, "equal_effect_higher_cost")
  expect_equal(tiny$incremental_cost, 1e-12)
})

test_that("comparison checks joint baseline multisets and only explicit scenario exclusions", {
  profiles <- data.frame(state_label = "A", age = c(40, 40, 60, 60),
    risk = c("low", "low", "high", "high"), treatment_active = FALSE)
  reference <- comparison_summary(population = profiles)
  reordered <- comparison_summary(population = profiles[4:1, ])
  expect_true(all(compareScenarios(reference, reordered)$outcomes$incremental_total == 0))
  logicalProfile <- comparison_summary(population = data.frame(state_label = "A", risk = TRUE))
  numericProfile <- comparison_summary(population = data.frame(state_label = "A", risk = 1))
  expect_error(compareScenarios(logicalProfile, numericProfile), "classes differ for risk")
  crossed <- profiles
  crossed$risk <- rev(crossed$risk)
  expect_error(compareScenarios(reference, comparison_summary(population = crossed)),
                 "baseline|[Ii]nitial|profile")
  multiplicity <- profiles[c(1, 1, 2, 3), ]
  expect_error(compareScenarios(reference, comparison_summary(population = multiplicity)),
                 "baseline|[Ii]nitial|profile")
  treated <- profiles
  treated$treatment_active <- TRUE
  intervention <- comparison_summary(population = treated)
  expect_error(compareScenarios(reference, intervention), "baseline|[Ii]nitial|profile")
  expect_true(all(compareScenarios(reference, intervention,
    scenarioCovariates = "treatment_active")$outcomes$incremental_total == 0))
  expect_error(compareScenarios(reference, reordered, scenarioCovariates = "state_label"),
                 "protect|state_label")
  expect_error(compareScenarios(reference, reordered, scenarioCovariates = "elapsed_day"),
                 "protect|elapsed_day")
  named <- data.frame(subject_id = "original", state_label = "A")
  changed <- transform(named, subject_id = "different")
  expect_error(compareScenarios(comparison_summary(population = named),
    comparison_summary(population = changed)), "baseline|initial|profile|source_subject_id")
})

test_that("unequal horizons require actual terminal completion and never accept custom stopping", {
  short <- comparison_summary("A", nSteps = 1)
  long <- comparison_summary("A", nSteps = 3)
  expect_error(compareScenarios(short, long), "horizon|terminal|steps")
  completed <- comparison_summary("D", nSteps = 8)
  expect_equal(completed$metadata$steps, 1)
  expect_true(all(completed$byPatient$final_state == "D"))
  result <- compareScenarios(completed, long)
  life <- subset(result$outcomes, metric == "life_years")
  expect_equal(life$reference_total, 1)
  expect_equal(life$intervention_total, 4)
  expect_equal(life$incremental_total, 3)
  stopped <- comparison_summary(nSteps = 4, stopWhen = function(population, step) step == 1)
  expect_error(compareScenarios(stopped, stopped), "stop_when|custom|stopp")
  forged <- short
  forged$metadata$stop_reason <- "all_terminal"
  forged$metadata$all_terminal <- TRUE
  expect_error(compareScenarios(forged, long), "terminal|final")
})

test_that("economic definitions compare by state name and inconsistent summaries fail", {
  reference <- comparison_summary()
  reordered <- comparison_summary(stateOrder = c("D", "B", "A"),
    utilities = c(D = 0, B = 1, A = 0.5), aliveStates = c("B", "A"))
  expect_true(all(compareScenarios(reference, reordered)$outcomes$incremental_total == 0))
  expect_error(compareScenarios(reference, comparison_summary(terminalStates = character())), "terminalStates")
  expect_error(compareScenarios(reference, comparison_summary(utilities = c(A = 0.6, B = 1, D = 0))),
                 "utilit|definition")
  expect_error(compareScenarios(reference, comparison_summary(currency = "USD")), "currency")
  expect_error(compareScenarios(reference, comparison_summary(priceYear = 2024)), "priceYear|year")
  expect_error(compareScenarios(reference, comparison_summary(costDiscountRate = 0.03)), "discount|Discount")
  expect_error(compareScenarios(reference, comparison_summary(daysPerYear = 365.25)), "daysPerYear|year|clock")
  tampered <- reference
  tampered$totals$total[tampered$totals$metric == "cost"] <- 999
  expect_error(compareScenarios(reference, tampered), "total|patient|consistent")
})

test_that("missing components stay unknown and negative utilities retain their effect direction", {
  unknown <- comparison_summary(amount = NULL)
  known <- comparison_summary(amount = 10)
  result <- compareScenarios(unknown, known)
  expect_true(all(is.na(result$costs$reference_total)))
  expect_true(all(is.na(result$costs$incremental_total)))
  expect_true(all(result$costEffectiveness$status == "unavailable"))
  expect_true(all(is.na(result$costEffectiveness$icer)))
  noUtilities <- comparison_summary(utilities = NULL)
  absent <- comparison_ce(compareScenarios(noUtilities, noUtilities))
  expect_equal(absent$status, "unavailable")
  expect_true(is.na(absent$incremental_effect))
  expect_error(compareScenarios(noUtilities, known), "utilit|definition")

  negative <- c(A = -0.5, B = -1, D = 0)
  reference <- comparison_summary("A", -10, utilities = negative)
  intervention <- comparison_summary("D", -12, utilities = negative)
  comparison <- compareScenarios(reference, intervention)
  expect_equal(comparison_ce(comparison)$incremental_effect, 0.5)
  expect_equal(comparison_ce(comparison)$status, "dominant")
  expect_equal(comparison_ce(comparison, "life_years")$status, "lower_cost_less_effective")
  expect_true(all(is.na(comparison$costEffectiveness$icer)))
})
