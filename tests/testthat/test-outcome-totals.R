test_that("outcomeTotals selects stored discounted or undiscounted totals and scale", {
  matrix <- diag(2)
  dimnames(matrix) <- list(c("A", "D"), c("A", "D"))
  fit <- composeTransitionStages(list(unchanged = matrix), 7, "D")
  projection <- projectCohort(fit, 2, initial = c(A = 1), cohortSize = 10)
  rewards <- createRewardModel(stateCosts = c(A = 30, D = 0),
    upfrontCosts = c(program = 100), aliveStates = "A", utilities = c(A = .8, D = 0),
    currency = "EUR", priceYear = 2026, costDiscountRate = .1, healthDiscountRate = .1)
  outcomes <- summariseOutcomes(projection, rewards)
  before <- serialize(outcomes, NULL)
  weights <- 1.1^(-c(3.5, 10.5) / 365.25)
  discounted <- c(cost = 1000 + 300 * sum(weights),
    life_years = 70 / 365.25 * sum(weights), qalys = 56 / 365.25 * sum(weights))
  undiscounted <- c(cost = 1600, life_years = 140 / 365.25, qalys = 112 / 365.25)
  expect_equal(outcomeTotals(outcomes), discounted)
  expect_equal(outcomeTotals(outcomes, discounted = FALSE), undiscounted)
  expect_equal(outcomeTotals(outcomes, perPatient = TRUE), discounted / 10)
  expect_equal(outcomeTotals(outcomes, discounted = FALSE, perPatient = TRUE), undiscounted / 10)
  expect_identical(serialize(outcomes, NULL), before)
  reordered <- outcomes
  reordered$totals <- reordered$totals[rev(seq_len(nrow(reordered$totals))), ]
  expect_identical(outcomeTotals(reordered), outcomeTotals(outcomes))

  comparison <- compareScenarios(outcomes, outcomes)
  printed <- capture.output(returned <- withVisible(print(comparison)))
  expect_true(any(grepl("Discounted cohort totals", printed, fixed = TRUE)))
  expect_true(any(grepl("reference_total", printed, fixed = TRUE)))
  expect_true(any(grepl("intervention_total", printed, fixed = TRUE)))
  expect_true(any(grepl("incremental_total", printed, fixed = TRUE)))
  expect_true(any(grepl("discounted_qalys", printed, fixed = TRUE)))
  expect_true(any(grepl("per patient", printed, fixed = TRUE)))
  expect_identical(returned$value, comparison)
  expect_false(returned$visible)
  expect_error(outcomeTotals(comparison), "summary from")
  expect_error(outcomeTotals(outcomes, discounted = NA), "discounted must be TRUE or FALSE")
  expect_error(outcomeTotals(outcomes, perPatient = 1), "perPatient must be TRUE or FALSE")
})

test_that("outcomeTotals preserves labelled-cycle accounting and uncomputed values", {
  matrix <- diag(2)
  dimnames(matrix) <- list(c("A", "D"), c("A", "D"))
  fit <- composeTransitionStages(list(unchanged = matrix), 7, "D")
  simulation <- simulateTrajectories(createModelBundle(fit), n = 10,
    initial = c(A = 1), nSteps = 2, terminalStates = "D", seed = 4)
  labelled <- summariseSimulation(simulation, aliveStates = "A",
    utilities = c(A = .8, D = 0), healthDiscountRate = .1)
  weights <- 1.1^(-c(0, 7, 14) / 365.25)
  expect_equal(outcomeTotals(labelled), c(cost = NA_real_,
    life_years = 70 / 365.25 * sum(weights), qalys = 56 / 365.25 * sum(weights)))
  expect_equal(outcomeTotals(labelled, discounted = FALSE, perPatient = TRUE),
    c(cost = NA_real_, life_years = 21 / 365.25, qalys = 16.8 / 365.25))

  projection <- projectCohort(fit, 0, initial = c(A = 1), cohortSize = 10)
  fees <- summariseOutcomes(projection, createRewardModel(upfrontCosts = c(program = 100),
    aliveStates = "A", currency = "EUR", priceYear = 2026))
  expect_identical(outcomeTotals(fees), c(cost = 1000, life_years = 0, qalys = NA_real_))
  missing <- summariseOutcomes(projection, createRewardModel())
  expect_identical(outcomeTotals(missing),
    c(cost = NA_real_, life_years = NA_real_, qalys = NA_real_))
  expect_error(outcomeTotals(list()), "summary from")
})
