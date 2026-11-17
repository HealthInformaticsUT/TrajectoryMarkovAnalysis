test_that("excess mortality uses the same eligible source cycles as transition estimation", {
  history <- data.frame(
    subject_id = c(rep("repeated", 3), rep("later-death", 2), rep("survivor", 2),
      "first-death", "censored", rep("gap", 2), rep("unknown", 3)),
    state_label = c("Alive", "Alive", "Death1", "Alive", "Death2", "Alive", "Alive",
      "Death1", "Alive", "Alive", "Alive", "Alive", "OUT OF COHORT", "Alive"),
    state_start_date = as.Date("2020-01-01") + c(0, 7, 14, 0, 7, 0, 7, 0, 0, 0, 14, 0, 7, 14),
    state_end_date = as.Date("2020-01-01") + c(6, 13, 15, 6, 13, 6, 9, 1, 6, 6, 20, 6, 13, 20),
    risk = c("low", "low", NA, "high", NA, "high", NA, NA, NA, NA, NA, NA, NA, NA))
  prepared <- prepareTrajectories(history, 7, absorbingStates = c("Death1", "Death2"),
    states = c("Alive", "Unobserved", "Death1", "Death2"))
  background <- createModelBundle(createMortalityTransitions(
    data.frame(risk = c("low", "high"), death_probability = c(0.1, 0.3)),
    states = c("Living", "Dead"), deathState = "Dead", cycleLength = 7,
    probabilityDays = 7, strata = "risk", source = "Synthetic weekly mortality"))
  result <- calibrateExcessMortality(prepared, background, deathStates = c("Death1", "Death2"))
  alive <- subset(result$estimates, from_state == "Alive")
  expect_equal(unname(unlist(alive[c("cycles", "patients", "deaths", "observed_probability",
    "expected_background_deaths", "background_probability")])), c(4, 3, 2, 0.5, 0.8, 0.2))
  expected <- (log(0.8) - log(0.5)) / 7
  expect_equal(alive$excess_hazard_per_day, expected)
  expect_equal(alive$reconstructed_probability, 0.5)
  expect_equal(alive$calibration_difference, 0, tolerance = 1e-14)
  reference <- result$reference[order(result$reference$background_probability), ]
  expect_equal(reference$cycles, c(2, 2))
  expect_equal(reference$patients, c(1, 2))
  expect_equal(reference$weight, c(0.5, 0.5))
  expect_equal(sum(reference$weight *
    (1 - (1 - reference$background_probability) * exp(-expected * 7))), 0.5)
  fitted <- subset(estimateTransitions(prepared)$estimates, from_state == "Alive")
  expect_equal(unique(fitted$denominator), alive$cycles)
  missing <- subset(result$estimates, from_state == "Unobserved")
  expect_equal(missing$status, "unestimated")
  expect_true(is.na(missing$excess_hazard_per_day))
  expect_false(any(c("subject_id", "source_subject_id", ".row_id") %in%
    unlist(lapply(result, names), use.names = FALSE)))
})

test_that("period reference populations use source birthdays without overwriting estimation strata", {
  history <- data.frame(subject_id = c(rep("ages", 4), rep("dies", 2), rep("recovers", 2), "early"),
    state_label = c("Alive", "Alive", "Alive", "Death1", "Alive", "Death2", "Alive", "Recovered", "Death1"),
    state_start_date = as.Date("2025-01-01") + c(0, 7, 14, 21, 0, 7, 0, 7, 0),
    state_end_date = as.Date("2025-01-01") + c(6, 13, 20, 22, 6, 8, 6, 13, 1),
    birth_date = as.Date(c(rep("1960-01-08", 3), NA, "1960-01-08", NA, "1960-01-08", NA, NA)),
    age_group = "study group")
  prepared <- prepareTrajectories(history, 7,
    absorbingStates = c("Death1", "Death2", "Recovered"))
  background <- createModelBundle(createMortalityTransitions(
    data.frame(age_group = c("under65", "65plus"), death_probability = c(0.1, 0.3)),
    states = c("Living", "Dead"), deathState = "Dead", cycleLength = 7,
    probabilityDays = 7, strata = "age_group", source = "Synthetic age-specific mortality"),
    groups = list(age_group = list(column = "age", breaks = c(0, 65, Inf), labels = c("under65", "65plus"))))
  result <- calibrateExcessMortality(prepared, background, deathStates = c("Death1", "Death2"),
    periodBreaks = c(0, 7, Inf), periodLabels = c("early", "late"), strata = "age_group")
  rows <- result$estimates[match(c("early", "late"), result$estimates$period), ]
  expect_equal(result$strata$age_group, "study group")
  expect_equal(rows$cycles, c(3, 2))
  expect_equal(rows$deaths, c(1, 1))
  expect_equal(rows$background_probability, c(0.1, 0.3))
  expect_equal(rows$reconstructed_probability, c(1 / 3, 1 / 2))
  expect_setequal(result$estimates$from_state, "Alive")
  expect_identical(prepared$data$age_group, rep("study group", nrow(prepared$data)))
})

test_that("negative excess requires an explicit floor and exposes its reconstruction difference", {
  history <- data.frame(subject_id = rep(c("dead", "alive"), each = 2),
    state_label = c("Alive", "Dead", "Alive", "Alive"),
    state_start_date = as.Date("2020-01-01") + rep(c(0, 7), 2),
    state_end_date = as.Date("2020-01-01") + rep(c(6, 13), 2))
  prepared <- prepareTrajectories(history, 7, absorbingStates = "Dead")
  background <- createModelBundle(createMortalityTransitions(data.frame(death_probability = 0.6),
    states = c("Alive", "Dead"), deathState = "Dead", cycleLength = 7,
    probabilityDays = 7, source = "Synthetic weekly mortality"))
  expect_error(calibrateExcessMortality(prepared, background, deathStates = "Dead"),
    "negative|below|background")
  expect_warning(result <- calibrateExcessMortality(prepared, background, deathStates = "Dead",
    negativeExcess = "zero"), "negative|floor|zero")
  row <- result$estimates
  expect_equal(row$status, "floored")
  expect_equal(row$raw_excess_hazard_per_day, (log(0.4) - log(0.5)) / 7)
  expect_equal(row$excess_hazard_per_day, 0)
  expect_equal(row$reconstructed_probability, 0.6)
  expect_equal(row$calibration_difference, 0.1)
  expect_error(calibrateExcessMortality(prepared, background, deathStates = "Dead",
    negativeExcess = "automatic"), "negativeExcess|error|zero")
})

test_that("calibration rejects unsupported reference inputs and infinite hazards", {
  history <- data.frame(subject_id = rep(c("dead", "alive"), each = 2),
    state_label = c("Alive", "Dead", "Alive", "Alive"),
    state_start_date = as.Date("2020-01-01") + rep(c(0, 7), 2),
    state_end_date = as.Date("2020-01-01") + rep(c(6, 13), 2), sex = "female")
  prepared <- prepareTrajectories(history, 7, absorbingStates = "Dead")
  makeBackground <- function(probability = 0.1, cycleLength = 7, sex = "female") {
    createModelBundle(createMortalityTransitions(data.frame(sex = sex, death_probability = probability),
      states = c("Alive", "Dead"), deathState = "Dead", cycleLength = cycleLength,
      probabilityDays = 7, strata = "sex", source = "Synthetic weekly mortality"))
  }
  expect_error(calibrateExcessMortality(prepared, makeBackground(sex = "male"), deathStates = "Dead"),
    "support|match|strat|covariate")
  expect_error(calibrateExcessMortality(prepared, makeBackground(cycleLength = 14), deathStates = "Dead"),
    "cycle|Cycle")
  stale <- makeBackground()
  stale$models[[1]]$metadata$mortality$cycle_probability <- 0.2
  expect_error(calibrateExcessMortality(prepared, stale, deathStates = "Dead"),
    "mortality|probab|consistent|match")
  expect_error(calibrateExcessMortality(prepared, makeBackground(1), deathStates = "Dead"),
    "one|finite|certain|death|probab")
  onlyDeaths <- prepareTrajectories(history[history$subject_id == "dead", ], 7, absorbingStates = "Dead")
  expect_error(calibrateExcessMortality(onlyDeaths, makeBackground(), deathStates = "Dead"),
    "one|finite|certain|death|probab")
  expect_error(calibrateExcessMortality(prepared, makeBackground(), deathStates = "Alive"),
    "deathStates|absorbing|terminal")
})
