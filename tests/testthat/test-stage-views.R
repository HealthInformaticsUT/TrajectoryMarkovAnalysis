stage_view_fixture <- function() {
  # The first and fourth patients die in shortened cycles. The final patient
  # has an observed onset but is censored before the weekly outcome is known.
  history <- data.frame(subject_id = c("001", "002", "002", "003", "003", "004", "9007199254740993123"),
    state_label = c("P", "N", "N", "P", "P", "N", "P"),
    state_start_date = as.Date("2025-01-01") + c(0, 0, 7, 0, 7, 0, 0),
    state_end_date = as.Date("2025-01-01") + c(2, 6, 13, 6, 13, 4, 2))
  outcome <- history
  outcome$state_label[c(1, 6)] <- c("DP", "DN")
  attr(history, "trajectory_definition") <- list(version = 1L, trajectoryType = "Discrete",
    cycleLength = 7L, states = c("N", "P"), absorbingStates = character())
  attr(outcome, "trajectory_definition") <- list(version = 1L, trajectoryType = "Discrete",
    cycleLength = 7L, states = c("N", "P", "DN", "DP"), absorbingStates = c("DN", "DP"))
  list(onset = history, mortality = outcome)
}

test_that("aligned views retain first-cycle onset and terminal-week history", {
  views <- stage_view_fixture()
  original <- views
  views$onset <- views$onset[rev(seq_len(nrow(views$onset))), ]
  views$mortality <- prepareTrajectories(views$mortality)
  supplied <- views
  observations <- prepareStageObservations(views, "N")
  expect_identical(views, supplied)
  expect_identical(original, stage_view_fixture())
  expect_identical(observations$subject_id[1:2], c("001", "001"))
  expect_identical(observations$from_state[1:2], c("N", "P"))
  expect_identical(observations$to_state[1:2], c("P", "DP"))
  expect_true(all(observations$observed[1:2]))
  expect_equal(observations$source_day[1:2], c(0, 0))
  expect_identical(attr(observations, "stage_order"), c("onset", "mortality"))
  expect_identical(attr(observations, "trajectory_definition"),
    attr(original$mortality, "trajectory_definition"))
  expect_equal(subset(observations, subject_id == "003" & source_day == 7)$from_state,
    c("P", "P"))
  entries <- data.frame(subject_id = rev(unique(original$onset$subject_id)), entry_state = "N")
  expect_identical(prepareStageObservations(original, entries), observations)
  entries$entry_state[entries$subject_id == "003"] <- "P"
  baseline <- prepareStageObservations(original, entries)
  expect_identical(subset(baseline, subject_id == "003" & source_day == 0)$from_state, c("P", "P"))
  expect_true("9007199254740993123" %in% observations$subject_id)
  three <- prepareStageObservations(list(onset = original$onset,
    unchanged = original$onset, mortality = original$mortality), "N")
  expect_identical(three$from_state[1:3], c("N", "P", "P"))
  expect_identical(three$to_state[1:3], c("P", "P", "DP"))
  plain <- lapply(original, function(x) {
    definition <- attr(x, "trajectory_definition")
    attr(x, "trajectory_definition") <- NULL
    prepareTrajectories(x, cycleLength = definition$cycleLength,
      states = definition$states, absorbingStates = definition$absorbingStates)
  })
  expect_identical(prepareStageObservations(plain, "N"), observations)
})

test_that("all stages share explicit complete-cycle and next-cycle eligibility", {
  views <- stage_view_fixture()
  complete <- prepareStageObservations(views, "N")
  nextCycle <- prepareStageObservations(views, "N", requireNextCycle = TRUE)
  expect_equal(sum(complete$observed), 12)
  expect_equal(sum(nextCycle$observed), 8)
  censored <- subset(complete, subject_id == "9007199254740993123")
  expect_identical(censored$from_state, c("N", "P"))
  expect_true(all(is.na(censored$to_state)))
  expect_false(any(censored$observed))
  expect_true(all(subset(complete, subject_id %in% c("002", "003") & source_day == 7)$observed))
  expect_false(any(subset(nextCycle, subject_id %in% c("002", "003") & source_day == 7)$observed))
  expect_true(all(subset(nextCycle, subject_id %in% c("001", "004"))$observed))
})

test_that("paired stage estimates and projections equal handwritten observations", {
  observations <- prepareStageObservations(stage_view_fixture(), "N", requireNextCycle = TRUE)
  expected <- data.frame(subject_id = rep(c("001", "002", "002", "003", "003", "004", "9007199254740993123"), each = 2),
    source_day = rep(c(0, 0, 7, 0, 7, 0, 0), each = 2),
    stage = rep(c("onset", "mortality"), 7),
    from_state = c("N", "P", "N", "N", "N", "N", "N", "P", "P", "P", "N", "N", "N", "P"),
    to_state = c("P", "DP", "N", "N", NA, NA, "P", "P", NA, NA, "N", "DN", NA, NA),
    observed = rep(c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), each = 2),
    index_date = as.Date("2025-01-01"))
  expect_equal(observations[names(expected)], expected, ignore_attr = TRUE)
  fit <- estimateStageTransitions(observations, c("onset", "mortality"),
    definition = observations, identityStates = list(onset = "P"))
  hand <- estimateStageTransitions(expected, c("onset", "mortality"), 7,
    states = c("N", "P", "DN", "DP"), absorbingStates = c("DN", "DP"),
    identityStates = list(onset = "P"))
  expect_equal(fit$estimates, hand$estimates)
  inherited <- estimateStageTransitions(observations, identityStates = list(onset = "P"))
  expect_equal(inherited$estimates, hand$estimates)
  equivalent <- stage_view_fixture()$mortality
  attr(equivalent, "trajectory_definition")$cycleLength <- 7
  attr(equivalent, "trajectory_definition")$absorbingStates <- c("DP", "DN")
  expect_equal(estimateStageTransitions(observations, definition = equivalent,
    identityStates = list(onset = "P"))$estimates, hand$estimates)
  mortality <- estimateStageTransitions(observations[observations$stage == "mortality", , drop = FALSE])
  expect_equal(mortality$stageMatrices$mortality, fit$stageMatrices$mortality)
  expect_error(estimateStageTransitions(observations, c("mortality", "onset")), "stageOrder conflicts")
  expect_equal(fit$stageMatrices, hand$stageMatrices)
  expect_equal(unname(fit$matrix["N", ]), rep(.25, 4))
  projection <- projectCohort(fit, nCycles = 2, initial = c(N = 1), cohortSize = 4)
  expect_equal(projection$occupancy,
    projectCohort(hand, nCycles = 2, initial = c(N = 1), cohortSize = 4)$occupancy)
  expect_equal(subset(projection$occupancy, cycle == 1)$expected_count, rep(1, 4))
})

test_that("misaligned and unresolved trajectory views are rejected", {
  views <- stage_view_fixture()
  missingPatient <- views
  missingPatient$onset <- missingPatient$onset[-1, ]
  expect_error(prepareStageObservations(missingPatient, "N"), "align")
  wrongEnd <- views
  wrongEnd$onset$state_end_date[1] <- wrongEnd$onset$state_end_date[1] + 1
  expect_error(prepareStageObservations(wrongEnd, "N"), "align")
  shifted <- views
  shifted$onset$state_start_date <- shifted$onset$state_start_date + 7
  shifted$onset$state_end_date <- shifted$onset$state_end_date + 7
  expect_error(prepareStageObservations(shifted, "N"), "align")
  gap <- views
  for (i in seq_along(gap)) {
    gap[[i]]$state_start_date[3] <- gap[[i]]$state_start_date[3] + 7
    gap[[i]]$state_end_date[3] <- gap[[i]]$state_end_date[3] + 7
  }
  expect_error(prepareStageObservations(gap, "N"), "gaps")
  unresolved <- views
  unresolved$onset$state_label[2] <- "OUT OF COHORT"
  expect_error(prepareStageObservations(unresolved, "N"), "unresolved")
  expect_error(prepareStageObservations(views, "N", NA), "TRUE/FALSE")
  expect_error(prepareStageObservations(views, "N", 1), "TRUE/FALSE")
  expect_error(prepareStageObservations(unname(views), "N"), "named list")
  expect_error(prepareStageObservations(views), "entryState explicitly")
  expect_error(prepareStageObservations(views, data.frame(subject_id = "001", entry_state = "N")),
    "every trajectory subject")
})

test_that("absorbing entry and intermediate states cannot be left", {
  views <- stage_view_fixture()
  expect_error(prepareStageObservations(views, "DP"), "absorbing")
  intermediate <- views
  attr(intermediate$onset, "trajectory_definition") <- attr(views$mortality, "trajectory_definition")
  intermediate$onset$state_label[1] <- "DN"
  expect_error(prepareStageObservations(intermediate, "N"), "cannot leave an absorbing")
  intermediate$onset$state_label[1] <- "DP"
  result <- prepareStageObservations(intermediate, "N")
  expect_identical(result$from_state[1:2], c("N", "DP"))
  expect_identical(result$to_state[1:2], c("DP", "DP"))
  postTerminal <- views
  postTerminal$mortality$state_label[2] <- "DN"
  expect_error(prepareStageObservations(postTerminal, "N"), "final observed")
})
