reward_update_fixture <- function() {
  createRewardModel(
    stateCosts = data.frame(
      state = rep(c("A", "B", "D"), 4),
      period = rep(c("acute care", "later care"), each = 3, times = 2),
      component = rep(c("healthcare", "rehabilitation"), each = 6),
      cost = c(10, 30, 0, 5, 20, 0, 2, 4, 0, 1, 3, 0)),
    transitionCosts = data.frame(from_state = "A", to_state = "B",
      stage = "onset", component = "onset care", cost = 9),
    upfrontCosts = c(program = 0), aliveStates = c("A", "B"),
    utilities = c(A = 0.8, B = 0.4, D = 0), costTiming = "onset",
    healthTiming = "start", discountTiming = "end",
    costDiscountRate = 0.03, healthDiscountRate = 0.02,
    currency = "EUR", priceYear = 2026, daysPerYear = 365,
    periodBreaks = c(0, 7, Inf), periodLabels = c("acute care", "later care"))
}

test_that("reward updates preserve assumptions and completed-interval results", {
  reference <- reward_update_fixture()
  before <- reference
  expect_identical(updateRewardModel(reference), reference)
  updated <- updateRewardModel(reference, upfrontCosts = c(program = 75),
    costDiscountRate = 0.05, healthDiscountRate = 0.04)
  manual <- reference
  manual$upfrontCosts <- c(program = 75)
  manual$costDiscountRate <- 0.05
  manual$healthDiscountRate <- 0.04
  expect_identical(updated, manual)
  expect_identical(reference, before)

  states <- c("A", "B", "D")
  onset <- diag(3)
  dimnames(onset) <- list(states, states)
  onset["A", ] <- c(0, 1, 0)
  model <- composeTransitionStages(list(onset = onset), 7, "D")
  projection <- projectCohort(model, 2, initial = c(A = 1), cohortSize = 5)
  simulation <- simulateTrajectories(createModelBundle(model), n = 5,
    initial = c(A = 1), nSteps = 2, terminalStates = "D", seed = 8)
  projected <- summariseOutcomes(projection, updated)
  sampled <- summariseOutcomes(simulation, updated)
  expect_identical(projected, summariseOutcomes(projection, manual))
  expect_identical(sampled, summariseOutcomes(simulation, manual))
  expect_equal(projected$totals, sampled$totals)
  expect_equal(projected$metadata$accounted_days, 14)
  expect_equal(projected$costs$cost[projected$costs$component == "program"], 375)
  expect_equal(projected$byCycle$cost, c(590, 115))
  expect_equal(projected$totals$total[projected$totals$metric == "life_years"], 70 / 365)
})

test_that("reward updates remove optional inputs and reuse constructor validation", {
  reference <- reward_update_fixture()
  health <- updateRewardModel(reference, stateCosts = NULL, transitionCosts = NULL,
    upfrontCosts = NULL, currency = NULL, priceYear = NULL)
  expect_identical(health$costs_configured, FALSE)
  expect_null(health$stateCosts)
  expect_null(health$transitionCosts)
  expect_null(health$upfrontCosts)
  expect_identical(health$utilities, reference$utilities)
  costs <- updateRewardModel(reference, utilities = NULL, aliveStates = NULL)
  expect_null(costs$utilities)
  expect_null(costs$aliveStates)
  expect_identical(costs$stateCosts, reference$stateCosts)
  expect_error(updateRewardModel(reference, costDiscountRate = -1), "costDiscountRate")
  expect_error(updateRewardModel(reference, currency = NULL), "currency")
  expect_error(updateRewardModel(reference, aliveStates = NULL), "aliveStates")
  expect_error(updateRewardModel(reference, utilities = c(A = 0.8, B = 0.4, D = 0.1)),
    "zero for nonliving")
  expect_error(updateRewardModel(list()), "createRewardModel")
  expect_identical(reference, reward_update_fixture())
})

test_that("reward updates reject ambiguous argument names", {
  reference <- reward_update_fixture()
  expect_error(updateRewardModel(reference, 0.05), "named")
  expect_error(updateRewardModel(reference, upfrontCosts = NULL, 0.05), "named")
  expect_error(updateRewardModel(reference, costDiscountRate = 0.05, costDiscountRate = 0.04),
    "unique")
  expect_error(updateRewardModel(reference, costDiscount = 0.05), "Unknown.*costDiscount")
  expect_error(updateRewardModel(reference, costs_configured = FALSE), "Unknown.*costs_configured")
  expect_error(updateRewardModel(reference, periods = reference$periods), "Unknown.*periods")
})

test_that("period updates retain omitted choices and require coherent replacements", {
  reference <- createRewardModel(stateCosts = c(A = 10, D = 0),
    currency = "EUR", priceYear = 2026, periodBreaks = c(0, 7, Inf),
    periodLabels = c("acute", "later"))
  shifted <- updateRewardModel(reference, periodBreaks = c(0, 14, Inf))
  expect_identical(shifted$periods$period, c("acute", "later"))
  expect_equal(shifted$periods$start_day, c(0, 14))
  renamed <- updateRewardModel(reference, periodLabels = c("first", "second"))
  expect_identical(renamed$periods$period, c("first", "second"))
  expect_identical(renamed$periods$start_day, reference$periods$start_day)
  expect_error(updateRewardModel(reference, periodBreaks = c(0, 7, 14, Inf)), "periodLabels")
  resized <- updateRewardModel(reference, periodBreaks = c(0, 7, 14, Inf), periodLabels = NULL)
  expect_equal(nrow(resized$periods), 3)
  reset <- updateRewardModel(reference, periodBreaks = NULL)
  expect_identical(reset$periods,
    createRewardModel(stateCosts = c(A = 10, D = 0), currency = "EUR", priceYear = 2026)$periods)
  named <- updateRewardModel(reference, periodBreaks = NULL, periodLabels = "whole horizon")
  expect_identical(named$periods$period, "whole horizon")
  expect_equal(named$periods$start_day, 0)
  expect_equal(named$periods$end_day, Inf)
  expect_identical(updateRewardModel(reference, periodLabels = NULL)$periods,
    createRewardModel(periodBreaks = c(0, 7, Inf))$periods)
})
