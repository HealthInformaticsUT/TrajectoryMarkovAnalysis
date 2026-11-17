future_mortality_fixture <- function(deathStates = "D", ageGroups = FALSE) {
  index <- as.Date("2025-01-01")
  destinations <- list(A = c(rep("A", 4), rep("B", 4), rep(deathStates[1], 2)),
    B = c(rep("A", 2), rep("B", 6), rep(tail(deathStates, 1), 2)))
  history <- do.call(rbind, lapply(names(destinations), function(state) {
    do.call(rbind, lapply(seq_along(destinations[[state]]), function(i) {
      data.frame(subject_id = paste0(state, i),
        state_label = c(state, destinations[[state]][i]),
        state_start_date = index + c(0, 7), state_end_date = index + c(6, 13),
        birth_date = as.Date("1960-01-10"), age_group = "study")
    }))
  }))
  prepared <- prepareTrajectories(history, 7,
    states = c("A", "B", deathStates), absorbingStates = deathStates)
  clinicalGroups <- if (ageGroups) list(age_group = list(column = "age",
    breaks = c(0, Inf), labels = "study")) else NULL
  background <- createModelBundle(createMortalityTransitions(
    if (ageGroups) data.frame(age_group = c("under65", "65plus"),
      death_probability = c(0.02, 0.08)) else data.frame(death_probability = 0.05),
    states = c("Living", "Death"), deathState = "Death", cycleLength = 7,
    probabilityDays = 7, strata = if (ageGroups) "age_group" else NULL,
    source = "Synthetic weekly background mortality"),
    groups = if (ageGroups) list(age_group = list(column = "age",
      breaks = c(0, 65, Inf), labels = c("under65", "65plus"))) else NULL)
  strata <- if (ageGroups) "age_group" else NULL
  fitted <- estimateTransitions(prepared, strata = strata, periodLabels = "Reference")
  list(bundle = createModelBundle(fitted, groups = clinicalGroups),
    calibration = calibrateExcessMortality(prepared, background,
      deathStates = deathStates, strata = strata, periodLabels = "Reference"), prepared = prepared)
}

future_mortality_extend <- function(fixture, ...) {
  extrapolateMortality(fixture$bundle, fixture$calibration, fromDay = 7,
    referencePeriod = "Reference", source = "Synthetic future mortality", ...)
}

test_that("constant excess replaces mortality and preserves conditional destinations", {
  fixture <- future_mortality_fixture()
  original <- serialize(fixture, NULL)
  model <- future_mortality_extend(fixture, excess = "constant")
  result <- simulateTrajectories(model, n = 40, initial = c(A = 1),
    nSteps = 2, terminalStates = "D", seed = 34)
  audit <- subset(result$transitions, model_phase == "extrapolation")
  excess <- (log(0.95) - log(0.8)) / 7
  expect_identical(serialize(fixture, NULL), original)
  expect_identical(model$models, fixture$bundle$models)
  expect_equal(audit$background_death_probability, rep(0.05, nrow(audit)))
  expect_equal(audit$excess_hazard_per_day, rep(excess, nrow(audit)))
  expect_equal(audit$integrated_excess_hazard, rep(excess * 7, nrow(audit)))
  expect_equal(audit$mortality_probability, rep(0.2, nrow(audit)))
  reference <- fixture$bundle$models[[1]]$matrices$Reference
  expected <- reference[cbind(match(audit$source_state, rownames(reference)),
    match(audit$destination_state, colnames(reference)))]
  expect_equal(audit$transition_probability, unname(expected))
  expect_true(all(is.na(subset(result$transitions, model_phase == "base")$mortality_probability)))
  empty <- simulateTrajectories(model, n = 1, initial = c(A = 1), nSteps = 0)
  expect_true(all(c("background_stratum_id", "background_death_probability",
    "excess_hazard_per_day", "integrated_excess_hazard", "mortality_probability") %in%
      names(empty$transitions)))
})

test_that("linear excess is integrated across cycles including the convergence point", {
  fixture <- future_mortality_fixture()
  model <- future_mortality_extend(fixture, excess = "linear", declineDays = 10)
  result <- simulateTrajectories(model,
    data.frame(state_label = rep("A", 40), elapsed_day = 7),
    nSteps = 3, terminalStates = "D", seed = 35)
  audit <- result$transitions
  excess <- (log(0.95) - log(0.8)) / 7
  # The decline ends within the second seven-day cycle: areas are 4.55 and 0.45 days.
  area <- c(4.55, 0.45, 0)[match(audit$model_day, c(0, 7, 14))]
  expect_setequal(audit$model_day, c(0, 7, 14))
  expect_equal(audit$integrated_excess_hazard, excess * area)
  expect_equal(audit$mortality_probability, 1 - 0.95 * exp(-excess * area))
  expect_equal(subset(audit, model_day == 14)$mortality_probability,
    rep(0.05, sum(audit$model_day == 14)))
  reference <- fixture$bundle$models[[1]]$matrices$Reference
  conditional <- reference[cbind(match(audit$source_state, rownames(reference)),
    match(audit$destination_state, colnames(reference)))] / 0.8
  expected <- ifelse(audit$destination_state == "D", audit$mortality_probability,
    (1 - audit$mortality_probability) * conditional)
  expect_equal(audit$transition_probability, unname(expected))
})

test_that("background birthdays are private and resume preserves global effect and cost clocks", {
  fixture <- future_mortality_fixture(ageGroups = TRUE)
  # Local registry identifiers have no clinical meaning and need not agree.
  fixture$calibration$strata$stratum_id <- "calibration_group"
  fixture$calibration$estimates$stratum_id <- "calibration_group"
  fixture$calibration$reference$stratum_id <- "calibration_group"
  model <- future_mortality_extend(fixture, excess = "constant")
  population <- data.frame(state_label = rep("A", 5),
    birth_date = as.Date("1960-01-10"), index_date = as.Date("2025-01-01"))
  effects <- list(protect_A = list(from = "A", to = "D", multiplier = 0,
    redistributeTo = "A", startDay = 0, endDay = 28),
    protect_B = list(from = "B", to = "D", multiplier = 0,
      redistributeTo = "B", startDay = 0, endDay = 28))
  rules <- list(entry_fee = list(type = "entry", amount = 11),
    setup = list(type = "once", amount = 5, startDay = 7),
    care = list(type = "recurring", amount = 3, startDay = 7, endDay = 21))
  entry <- estimateEntryTransitions(fixture$prepared, entryState = "A")
  run <- function(steps) simulateTrajectories(model, population, nSteps = steps,
    entry = entry, transitionEffects = effects, costRules = rules,
    currency = "EUR", priceYear = 2025, terminalStates = "D", seed = 36)
  full <- run(4)
  resumed <- advanceSimulation(unserialize(serialize(run(1), NULL)), 3)
  for (field in c("population", "trajectories", "transitions", "effects", "costs", "totals", "metadata")) {
    expect_identical(resumed[[field]], full[[field]])
  }
  expect_identical(resumed$.state$rng, full$.state$rng)
  expect_equal(unique(full$trajectories$age_group), "study")
  audit <- subset(full$transitions, model_phase == "extrapolation")
  expectedBackground <- ifelse(audit$source_day == 7, 0.02, 0.08)
  expect_equal(audit$background_death_probability, expectedBackground)
  expect_equal(audit$mortality_probability,
    1 - (1 - expectedBackground) * 0.8 / 0.98)
  expect_equal(length(unique(audit$background_stratum_id)), 2)
  expect_true(all(full$transitions$destination_state != "D"))
  expect_setequal(full$effects$source_day, c(0, 7, 14, 21))
  expect_setequal(subset(full$costs, component == "entry_fee")$elapsed_day, 0)
  expect_setequal(subset(full$costs, component == "setup")$elapsed_day, 7)
  expect_setequal(subset(full$costs, component == "care")$elapsed_day, c(7, 14))
  expect_identical(summariseSimulation(resumed), summariseSimulation(full))
})

test_that("multiple death outcomes require explicit allocation and preserve absorption", {
  fixture <- future_mortality_fixture(c("D0", "D1"))
  expect_error(future_mortality_extend(fixture, excess = "constant"), "deathAllocation|death|allocation")
  model <- future_mortality_extend(fixture, excess = "constant",
    deathAllocation = c(A = "D0", B = "D1"))
  result <- simulateTrajectories(model,
    data.frame(state_label = rep(c("A", "B"), each = 40), elapsed_day = 7),
    nSteps = 1, terminalStates = c("D0", "D1"), seed = 37)
  deaths <- subset(result$transitions, destination_state %in% c("D0", "D1"))
  expect_gt(nrow(deaths), 0)
  expect_equal(deaths$destination_state, ifelse(deaths$source_state == "A", "D0", "D1"))
  weights <- rbind(A = c(D0 = 0.25, D1 = 0.75), B = c(D0 = 0.75, D1 = 0.25))
  weighted <- future_mortality_extend(fixture, excess = "constant", deathAllocation = weights)
  result <- simulateTrajectories(weighted,
    data.frame(state_label = rep(c("A", "B"), each = 40), elapsed_day = 7),
    nSteps = 1, terminalStates = c("D0", "D1"), seed = 38)
  deaths <- subset(result$transitions, destination_state %in% c("D0", "D1"))
  expect_equal(deaths$transition_probability, unname(deaths$mortality_probability *
    weights[cbind(match(deaths$source_state, rownames(weights)),
      match(deaths$destination_state, colnames(weights)))]))
  terminal <- simulateTrajectories(weighted,
    data.frame(state_label = c("D0", "D1"), elapsed_day = 7), nSteps = 2,
    terminalStates = c("D0", "D1"))
  expect_equal(nrow(terminal$transitions), 0)
  expect_equal(terminal$population$state_label, c("D0", "D1"))
})

test_that("mortality extrapolation rejects inconsistent support and unsupported use", {
  fixture <- future_mortality_fixture()
  expect_error(extrapolateMortality(fixture$bundle, fixture$calibration, 7,
    referencePeriod = "missing", excess = "constant", source = "Fixture"), "period|reference")
  expect_error(extrapolateMortality(fixture$bundle, fixture$calibration, 8,
    referencePeriod = "Reference", excess = "constant", source = "Fixture"), "align|fromDay|cycle")
  expect_error(future_mortality_extend(fixture, excess = "linear"), "declineDays|decline")
  expect_error(future_mortality_extend(fixture, excess = "constant", declineDays = 10),
    "declineDays|constant|linear")
  inconsistent <- fixture
  inconsistent$calibration$estimates$deaths[1] <- inconsistent$calibration$estimates$deaths[1] + 1
  expect_error(future_mortality_extend(inconsistent, excess = "constant"),
    "calibrat|death|count|probab|match|reference")
  unavailable <- fixture
  unavailable$calibration$estimates$status[1] <- "unestimated"
  expect_error(future_mortality_extend(unavailable, excess = "constant"),
    "calibrat|support|unestimated|status|reference")
  model <- future_mortality_extend(fixture, excess = "constant")
  expect_error(projectCohort(model, 2, initial = c(A = 1)),
    "simulateTrajectories|patient|profile|mortality")
  expect_error(projectCohort(model$extrapolation$bundle, 2, initial = c(A = 1)),
    "simulateTrajectories|patient|profile|mortality")
  ageFixture <- future_mortality_fixture(ageGroups = TRUE)
  ageModel <- future_mortality_extend(ageFixture, excess = "constant")
  expect_error(simulateTrajectories(ageModel,
    data.frame(state_label = "A", elapsed_day = 7), nSteps = 1),
    "age|covariate|group|match|support")
  restrictedBackground <- createModelBundle(createMortalityTransitions(
    data.frame(age_group = "under65", death_probability = 0.02),
    states = c("Living", "Death"), deathState = "Death", cycleLength = 7,
    probabilityDays = 7, strata = "age_group", source = "Young ages only"),
    groups = ageFixture$calibration$background$groups)
  ageFixture$calibration <- calibrateExcessMortality(ageFixture$prepared,
    restrictedBackground, deathStates = "D", strata = "age_group", periodLabels = "Reference")
  restricted <- future_mortality_extend(ageFixture, excess = "constant")
  expect_error(simulateTrajectories(restricted,
    data.frame(state_label = "A", elapsed_day = 7, age = 65), nSteps = 1),
    "background|support|strat|covariate|match")
})
