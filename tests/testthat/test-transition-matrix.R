matrix_accessor_history <- function() {
  data.frame(subject_id = c(rep("one", 3), rep("two", 2)),
    state_label = c("A", "A", "D", "A", "D"),
    state_start_date = as.Date("2020-01-01") + c(0, 7, 14, 0, 7),
    state_end_date = as.Date("2020-01-07") + c(0, 7, 14, 0, 7),
    group = c(rep("low", 3), rep("high", 2)))
}

matrix_accessor_stages <- function() {
  states <- c("A", "B", "D")
  onset <- mortality <- diag(3)
  dimnames(onset) <- dimnames(mortality) <- list(states, states)
  onset["A", ] <- c(.8, .2, 0)
  mortality["B", ] <- c(0, .5, .5)
  composeTransitionStages(list(onset = onset, mortality = mortality),
    cycleLength = 7, absorbingStates = "D")
}

test_that("fit access preserves original probabilities and requires an exact period", {
  fit <- estimateTransitions(matrix_accessor_history(), 7,
    states = c("A", "D", "Unobserved"), absorbingStates = "D")
  expect_identical(transitionMatrix(fit), fit$matrix)
  expect_true(all(is.na(transitionMatrix(fit)["Unobserved", ])))
  periods <- estimateTransitions(matrix_accessor_history(), 7, absorbingStates = "D",
    periodBreaks = c(0, 7, Inf), periodLabels = c("Early", "Late"))
  expect_error(transitionMatrix(periods), "Supply period")
  expect_identical(transitionMatrix(periods, period = "Late"), periods$matrices$Late)
  expect_error(transitionMatrix(periods, period = "La"), "Unknown period")
  expect_error(transitionMatrix(fit, cycle = 0), "only for a cohort projection")
  expect_error(transitionMatrix(fit, stratum = "s1"), "stratum is supported only")
  expect_error(transitionMatrix(fit, stage = "onset"), "stage definitions")
  expect_error(transitionMatrix(fit, period = 1), "exact name")
  expect_error(transitionMatrix(fit, period = c("Early", "Late")), "exact name")
})

test_that("stratified fits and bundles require an exact registry ID", {
  fit <- estimateTransitions(matrix_accessor_history(), 7, absorbingStates = "D",
    strata = "group")
  id <- fit$strata$stratum_id[fit$strata$group == "low"]
  bundle <- createModelBundle(fit)
  for (x in list(fit, bundle)) {
    expect_error(transitionMatrix(x), "Supply stratum")
    expect_identical(transitionMatrix(x, stratum = id), fit$models[[id]]$matrix)
    expect_error(transitionMatrix(x, stratum = "low"), "Unknown stratum")
    expect_error(transitionMatrix(x, stratum = "s"), "Unknown stratum")
  }
  single <- createModelBundle(matrix_accessor_stages())
  expect_identical(transitionMatrix(single), single$models$s1$matrix)
  expect_identical(transitionMatrix(single, stage = "onset"),
    single$models$s1$stageMatrices$onset[[1]])
})

test_that("stage access distinguishes original fits from effective projection cycles", {
  fit <- matrix_accessor_stages()
  original <- fit
  projection <- projectCohort(fit, 3, initial = c(A = 1), transitionEffects = list(
    prevention = list(stage = "onset", from = "A", to = "B", redistributeTo = "A",
      multiplier = .5, startDay = 7, endDay = 14)))
  expect_identical(transitionMatrix(fit), fit$matrix)
  expect_identical(transitionMatrix(fit, stage = "onset"), fit$stageMatrices$onset[[1]])
  expect_equal(transitionMatrix(fit, stage = "onset")["A", "B"], .2)
  expect_equal(transitionMatrix(projection, cycle = 0, stage = "onset")["A", "B"], .2)
  expect_equal(transitionMatrix(projection, cycle = 1, stage = "onset")["A", "B"], .1)
  expect_equal(transitionMatrix(projection, cycle = 2, stage = "onset")["A", "B"], .2)
  expect_identical(transitionMatrix(projection, cycle = 1), projection$cycleMatrices[[2]])
  expect_equal(transitionMatrix(projection, cycle = 1)["A", "D"], .05)
  expect_error(transitionMatrix(fit, stage = "on"), "Unknown stage")
  expect_error(transitionMatrix(projection, cycle = 1, stage = "on"), "Unknown stage")
  expect_identical(fit, original)
})

test_that("projection selection follows source cycles and rejects unsupported selectors", {
  fit <- estimateTransitions(matrix_accessor_history(), 7, absorbingStates = "D",
    periodBreaks = c(0, 7, Inf), periodLabels = c("Early", "Late"))
  projection <- projectCohort(fit, 2, initial = c(A = 1))
  expect_identical(transitionMatrix(projection, cycle = 0), fit$matrices$Early)
  expect_identical(transitionMatrix(projection, cycle = 1), fit$matrices$Late)
  expect_error(transitionMatrix(projection), "zero-based source cycle")
  expect_error(transitionMatrix(projection, cycle = 2), "Unknown source cycle")
  expect_error(transitionMatrix(projection, cycle = .5), "exact finite integer")
  expect_error(transitionMatrix(projection, cycle = -1), "exact finite integer")
  expect_error(transitionMatrix(projection, cycle = 0, period = "Early"), "do not supply")
  expect_error(transitionMatrix(projection, cycle = 0, stratum = "s1"), "do not supply")
  empty <- projectCohort(fit, 0, initial = c(A = 1))
  expect_error(transitionMatrix(empty, cycle = 0), "no completed transition steps")
})

test_that("net-cycle effects never fall back to original stage definitions", {
  fit <- matrix_accessor_stages()
  projection <- projectCohort(fit, 1, initial = c(A = 1), transitionEffects = list(
    prevention = list(from = "A", to = "B", redistributeTo = "A", multiplier = .5)))
  expect_equal(transitionMatrix(projection, cycle = 0)["A", "B"], .05)
  expect_identical(transitionMatrix(projection, cycle = 0), projection$cycleMatrices[[1]])
  expect_error(transitionMatrix(projection, cycle = 0, stage = "onset"),
    "Effective stage matrices are unavailable")
  expect_error(transitionMatrix(projection, stage = "onset"), "zero-based source cycle")
})

test_that("simulations and switched bundles require explicit source-model selection", {
  fit <- matrix_accessor_stages()
  bundle <- createModelBundle(fit)
  simulation <- simulateTrajectories(bundle, n = 1, initial = c(A = 1),
    nSteps = 1, terminalStates = "D", seed = 24)
  expect_error(transitionMatrix(simulation, cycle = 0), "Simulation results are not supported")
  switched <- extendModelBundle(bundle, 7, fit, source = "test assumption")
  expect_error(transitionMatrix(switched), "late-model switch")
})
