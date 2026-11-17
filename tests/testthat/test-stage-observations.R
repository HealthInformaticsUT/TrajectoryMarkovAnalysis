stage_observation_fixture <- function() {
  x <- data.frame(subject_id = rep(c("001", "002", "003", "004", "005"), each = 2),
    source_day = 0, stage = rep(c("onset", "mortality"), 5),
    from_state = c("N", "P", "N", "N", "P", "P", "N", "N", "N", "N"),
    to_state = c("P", "DP", "N", "N", "P", "P", "N", "DN", NA, NA),
    observed = c(rep(TRUE, 8), FALSE, FALSE),
    index_date = as.Date("2025-01-01"), birth_date = as.Date("1960-01-01"))
  fit <- estimateStageTransitions(x, c("onset", "mortality"), 7,
    states = c("N", "P", "DN", "DP"), absorbingStates = c("DN", "DP"),
    identityStates = list(onset = "P"), periodLabels = "Reference")
  list(x = x, fit = fit)
}

test_that("stage fitting includes early events and excludes unobserved outcomes", {
  fixture <- stage_observation_fixture()
  fit <- fixture$fit
  expect_equal(fit$stageMatrices$onset$Reference["N", "P"], 1 / 3)
  expect_equal(fit$stageMatrices$mortality$Reference["P", "DP"], 1 / 2)
  expect_equal(fit$matrix["N", "DP"], 1 / 6)
  expect_equal(fit$coverage$excluded_outcomes, c(1, 1))
  expect_true("001" %in% fit$observations$subject_id)
  e <- subset(fit$estimates, stage == "mortality" & from_state == "P" & to_state == "DP")
  expect_equal(unname(unlist(e[c("count", "denominator", "patients", "source_patients")])), c(1, 2, 1, 2))
  projected <- projectCohort(fit, nCycles = 1, initial = c(N = 1), cohortSize = 6)
  expect_equal(subset(projected$occupancy, cycle == 1 & state == "DP")$expected_count, 1)
  bundle <- createModelBundle(fit)
  expect_null(bundle$models[[1]]$observations)
  expect_error(estimateStageTransitions(rbind(fixture$x, fixture$x[1, ]),
    c("onset", "mortality"), 7), "once")
  broken <- fixture$x
  broken$from_state[2] <- "N"
  expect_error(estimateStageTransitions(broken, c("onset", "mortality"), 7), "join")
})

test_that("stage support and calibration agree for first-cycle mortality", {
  fixture <- stage_observation_fixture()
  background <- createModelBundle(createMortalityTransitions(
    data.frame(death_probability = .1), states = c("Alive", "Dead"),
    deathState = "Dead", cycleLength = 7, probabilityDays = 7, source = "Test table"))
  expect_error(calibrateExcessMortality(fixture$fit, background, c("DN", "DP")), "stage")
  calibration <- calibrateExcessMortality(fixture$fit, background,
    c("DN", "DP"), stage = "mortality")
  expect_equal(calibration$estimates$cycles, c(2, 2))
  expect_equal(calibration$estimates$deaths, c(1, 1))
  expect_equal(calibration$estimates$reconstructed_probability, c(.5, .5))
  expect_equal(calibration$metadata$mortality_stage, "mortality")
  relabelled <- fixture$fit
  relabelled$observations$stratum_id <- "stale-derived-id"
  restored <- calibrateExcessMortality(relabelled, background,
    c("DN", "DP"), stage = "mortality")
  expect_equal(restored$estimates, calibration$estimates)
  allDeaths <- fixture$x
  allDeaths$to_state[allDeaths$stage == "mortality" & allDeaths$observed] <-
    ifelse(allDeaths$from_state[allDeaths$stage == "mortality" & allDeaths$observed] == "N", "DN", "DP")
  allDeathFit <- estimateStageTransitions(allDeaths, c("onset", "mortality"), 7,
    states = c("N", "P", "DN", "DP"), absorbingStates = c("DN", "DP"),
    identityStates = list(onset = "P"), periodLabels = "Reference")
  expect_error(calibrateExcessMortality(allDeathFit, background,
    c("DN", "DP"), stage = "mortality"), "Every eligible")
  capped <- calibrateExcessMortality(allDeathFit, background, c("DN", "DP"),
    stage = "mortality", probabilityCeiling = .999999)
  expect_equal(capped$estimates$observed_probability, c(1, 1))
  expect_equal(capped$estimates$reconstructed_probability, c(.999999, .999999))
  expect_equal(capped$estimates$calibration_difference, rep(-.000001, 2))
  expect_error(extrapolateMortality(createModelBundle(allDeathFit), capped,
    fromDay = 7, referencePeriod = "Reference", excess = "constant",
    deathAllocation = c(N = "DN", P = "DP"), source = "Test"), "support")
  model <- extrapolateMortality(createModelBundle(fixture$fit), calibration,
    fromDay = 7, referencePeriod = "Reference", excess = "constant",
    deathAllocation = c(N = "DN", P = "DP"), source = "Test extension")
  simulated <- simulateTrajectories(model,
    population = data.frame(state_label = rep("N", 30), elapsed_day = 7),
    nSteps = 1, terminalStates = c("DN", "DP"), seed = 79)
  expected <- fixture$fit$matrix[cbind(rep("N", nrow(simulated$transitions)),
    simulated$transitions$destination_state)]
  expect_equal(simulated$transitions$transition_probability, unname(expected))
  expect_equal(simulated$transitions$mortality_probability, rep(.5, nrow(simulated$transitions)))
})

test_that("stage fitting retains unsupported periods and time-varying strata", {
  x <- stage_observation_fixture()$x
  x$age_group <- "younger"
  y <- x
  y$source_day <- 7
  y$age_group <- "older"
  fit <- estimateStageTransitions(rbind(x, y), c("onset", "mortality"), 7,
    states = c("N", "P", "DN", "DP"), absorbingStates = c("DN", "DP"),
    identityStates = list(onset = "P"), periodBreaks = c(0, 7, Inf),
    periodLabels = c("early", "later"), strata = "age_group")
  expect_s3_class(fit, "tma_stratified_transitions")
  expect_equal(nrow(fit$strata), 2)
  expect_equal(sum(fit$coverage$observed_outcomes), 16)
  expect_true(all(is.na(subset(fit$estimates,
    age_group == "younger" & period == "later" & stage == "mortality" & from_state == "N")$probability)))
  expect_equal(nrow(createModelBundle(fit)$strata), 2)
  background <- createModelBundle(createMortalityTransitions(
    data.frame(death_probability = .1), states = c("Alive", "Dead"),
    deathState = "Dead", cycleLength = 7, probabilityDays = 7, source = "Test table"))
  calibration <- calibrateExcessMortality(fit, background, c("DN", "DP"), stage = "mortality")
  expect_equal(sum(calibration$estimates$cycles), 8)
  expect_equal(sum(calibration$estimates$status == "unestimated"), 4)
  expect_error(calibrateExcessMortality(fit$models[[1]], background,
    c("DN", "DP"), stage = "mortality"), "complete stratified")
  wrong <- x
  wrong$age_group[2] <- "older"
  expect_error(estimateStageTransitions(wrong, c("onset", "mortality"), 7,
    strata = "age_group"), "agree across stages")
})
