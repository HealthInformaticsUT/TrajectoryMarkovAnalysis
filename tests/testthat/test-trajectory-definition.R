definition_fixture <- function(type = "Discrete") {
  x <- data.frame(subject_id = c(rep("001", 4), "002"),
    state_label = c("START", "Alive", "Dead", "EXIT", "Alive"),
    state_start_date = c("2020-01-01", "2020-01-01", "2020-01-08", "2020-01-15", "2020-01-01"),
    state_end_date = c("2020-01-01", "2020-01-07", "2020-01-14", "2020-01-15", "2020-01-07"))
  attr(x, "trajectory_definition") <- list(version = 1L, trajectoryType = type,
    cycleLength = if (type == "Discrete") 7L else NULL,
    states = c("Unobserved", "Alive", "Dead", "Other death"),
    absorbingStates = c("Dead", "Other death"))
  x
}

definition_stage_fixture <- function() {
  data.frame(subject_id = c("001", "001", "002", "002", "003"),
    source_day = 0, stage = c("onset", "mortality", "onset", "mortality", "onset"),
    from_state = "Alive", to_state = c("Alive", "Dead", "Alive", "Alive", NA),
    observed = c(TRUE, TRUE, TRUE, TRUE, FALSE))
}

test_that("trajectory definitions supply the complete dictionary and cycle without dependencies", {
  x <- definition_fixture()
  definition <- attr(x, "trajectory_definition")
  prepared <- prepareTrajectories(x)
  expect_identical(prepared$cycleLength, 7L)
  expect_identical(prepared$states, definition$states)
  expect_identical(prepared$absorbingStates, definition$absorbingStates)
  expect_identical(attr(prepared, "trajectory_definition"), definition)
  expect_null(attr(prepared$data, "trajectory_definition"))
  expect_identical(attr(x, "trajectory_definition"), definition)
  expect_equal(prepared$diagnostics$boundary_markers_removed, 2)
  expect_identical(prepareTrajectories(prepared), prepared)
  fit <- estimateTransitions(x)
  expect_identical(rownames(fit$matrix), definition$states)
  expect_equal(fit$matrix["Alive", "Dead"], 1)
  expect_equal(fit$matrix["Other death", "Other death"], 1)
  expect_true(all(is.na(fit$matrix["Unobserved", ])))
})

test_that("trajectory definitions survive row selection, RDS and re-preparation", {
  x <- definition_fixture()
  selected <- x[x$subject_id == "002", , drop = FALSE]
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(selected, path)
  prepared <- prepareTrajectories(readRDS(path))
  expect_identical(prepared$states, attr(x, "trajectory_definition")$states)
  expect_identical(prepared$absorbingStates, c("Dead", "Other death"))
  saveRDS(prepared, path)
  restored <- readRDS(path)
  expect_identical(prepareTrajectories(restored), prepared)
  expect_error(prepareTrajectories(restored, cycleLength = 14), "cycleLength conflicts")
  expect_error(prepareTrajectories(restored, states = rev(prepared$states)), "states conflicts")
  expect_error(prepareTrajectories(restored, absorbingStates = character()), "absorbingStates conflicts")
})

test_that("explicit trajectory settings must agree with the attached definition", {
  x <- definition_fixture()
  definition <- attr(x, "trajectory_definition")
  agreed <- prepareTrajectories(x, cycleLength = 7, states = definition$states,
    absorbingStates = rev(definition$absorbingStates))
  expect_equal(agreed$cycleLength, 7)
  expect_setequal(agreed$absorbingStates, definition$absorbingStates)
  expect_error(prepareTrajectories(x, cycleLength = NULL), "cycleLength conflicts")
  expect_error(prepareTrajectories(x, cycleLength = 14), "cycleLength conflicts")
  expect_error(prepareTrajectories(x, states = NULL), "states conflicts")
  expect_error(prepareTrajectories(x, states = rev(definition$states)), "declared order")
  expect_error(prepareTrajectories(x, states = definition$states[-1]), "states conflicts")
  expect_error(prepareTrajectories(x, absorbingStates = "Dead"), "absorbingStates conflicts")
  expect_error(estimateTransitions(x, cycleLength = 1), "cycleLength conflicts")
  stale <- prepareTrajectories(x)
  stale$states <- rev(stale$states)
  expect_error(prepareTrajectories(stale), "states conflicts")
  unknown <- x
  unknown$state_label[2] <- "Undeclared"
  expect_error(prepareTrajectories(unknown), "include every observed")
})

test_that("malformed and unsupported trajectory definitions fail clearly", {
  x <- definition_fixture()
  original <- attr(x, "trajectory_definition")
  invalid <- list(
    list(value = "invalid", pattern = "versioned list"),
    list(value = c(original, list(patient_data = x)), pattern = "versioned list"),
    list(value = within(original, rm(cycleLength)), pattern = "versioned list"),
    list(value = modifyList(original, list(version = 2L)), pattern = "Unsupported.*version"),
    list(value = modifyList(original, list(version = "1")), pattern = "version must"),
    list(value = modifyList(original, list(trajectoryType = "Other")), pattern = "trajectoryType"),
    list(value = modifyList(original, list(cycleLength = 0)), pattern = "cycleLength"),
    list(value = modifyList(original, list(cycleLength = 7.5)), pattern = "cycleLength"),
    list(value = modifyList(original, list(trajectoryType = "Continuous")), pattern = "must be NULL"),
    list(value = modifyList(original, list(states = c("Alive", "Alive"))), pattern = "states"),
    list(value = modifyList(original, list(states = c("Alive", "EXIT"))), pattern = "boundary markers"),
    list(value = modifyList(original, list(states = c("0", original$states))), pattern = "boundary markers"),
    list(value = modifyList(original, list(absorbingStates = "Undeclared")), pattern = "belong to states"),
    list(value = modifyList(original, list(absorbingStates = c("Dead", NA_character_))), pattern = "absorbingStates"))
  for (case in invalid) {
    attr(x, "trajectory_definition") <- case$value
    expect_error(prepareTrajectories(x), case$pattern)
  }
  attr(original, "environment") <- new.env(parent = emptyenv())
  attr(x, "trajectory_definition") <- original
  expect_error(prepareTrajectories(x), "versioned list")
})

test_that("continuous definitions support intervals without relabelling them as cycles", {
  x <- definition_fixture("Continuous")
  prepared <- prepareTrajectories(x)
  expect_null(prepared$cycleLength)
  expect_identical(prepared$states, attr(x, "trajectory_definition")$states)
  expect_identical(prepareTrajectories(x, cycleLength = NULL), prepared)
  expect_error(prepareTrajectories(x, cycleLength = 7), "cycleLength conflicts")
  expect_error(prepareTrajectories(prepared, cycleLength = 7), "cycleLength conflicts")
  expect_error(estimateTransitions(x), "cycleLength must be supplied")
})

test_that("plain data frames retain explicit arguments and flexible re-preparation", {
  x <- definition_fixture()
  attr(x, "trajectory_definition") <- NULL
  prepared <- prepareTrajectories(x)
  expect_null(prepared$cycleLength)
  expect_identical(prepared$states, c("Alive", "Dead"))
  expect_identical(prepared$absorbingStates, character())
  expect_null(attr(prepared, "trajectory_definition"))
  updated <- prepareTrajectories(prepared, cycleLength = 7,
    states = c("Dead", "Alive", "Additional"), absorbingStates = "Dead")
  expect_equal(updated$cycleLength, 7)
  expect_identical(updated$states, c("Dead", "Alive", "Additional"))
  expect_null(attr(updated, "trajectory_definition"))
  x$state_label[x$state_label == "Alive"] <- "0"
  expect_true("0" %in% prepareTrajectories(x)$states)
})

test_that("stage estimation inherits settings while stage observations stay explicit", {
  x <- definition_fixture()
  observations <- definition_stage_fixture()
  order <- c("onset", "mortality")
  fit <- estimateStageTransitions(observations, order, definition = x)
  explicit <- estimateStageTransitions(observations, order, 7,
    states = attr(x, "trajectory_definition")$states,
    absorbingStates = attr(x, "trajectory_definition")$absorbingStates)
  expect_equal(fit, explicit)
  expect_equal(estimateStageTransitions(observations, order,
    definition = prepareTrajectories(x)), explicit)
  expect_equal(fit$coverage$excluded_outcomes, c(1, 0))
  expect_equal(fit$stageMatrices$mortality[[1]]["Alive", "Dead"], .5)
  expect_true(all(is.na(fit$stageMatrices$onset[[1]]["Unobserved", ])))
  expect_equal(fit$stageMatrices$onset[[1]]["Other death", "Other death"], 1)
  identity <- estimateStageTransitions(observations, order, definition = x,
    identityStates = list(onset = "Unobserved"))
  expect_equal(identity$stageMatrices$onset[[1]]["Unobserved", "Unobserved"], 1)
  expect_error(estimateStageTransitions(x, order, definition = x), "observations must")
  expect_error(estimateStageTransitions(observations, rev(order), definition = x), "join")
})

test_that("stage definition conflicts and invalid public sources fail clearly", {
  x <- definition_fixture()
  observations <- definition_stage_fixture()
  order <- c("onset", "mortality")
  expect_error(estimateStageTransitions(observations, order, 14, definition = x), "cycleLength conflicts")
  expect_error(estimateStageTransitions(observations, order, NULL, definition = x), "cycleLength conflicts")
  expect_error(estimateStageTransitions(observations, order, definition = x,
    states = c("Alive", "Dead")), "states conflicts")
  expect_error(estimateStageTransitions(observations, order, definition = x,
    absorbingStates = character()), "absorbingStates conflicts")
  expect_error(estimateStageTransitions(observations, order,
    definition = attr(x, "trajectory_definition")), "definition must be prepared")
  attr(x, "trajectory_definition") <- NULL
  expect_error(estimateStageTransitions(observations, order, 7, definition = x), "must carry")
  expect_error(estimateStageTransitions(observations, order), "cycleLength must be supplied")
  expect_error(estimateStageTransitions(observations, order, definition = NULL), "cycleLength must be supplied")
})

test_that("stage settings can be derived from plain-source prepared histories", {
  x <- definition_fixture()
  definition <- attr(x, "trajectory_definition")
  attr(x, "trajectory_definition") <- NULL
  prepared <- prepareTrajectories(x, 7, states = definition$states,
    absorbingStates = definition$absorbingStates)
  observations <- definition_stage_fixture()
  order <- c("onset", "mortality")
  fit <- estimateStageTransitions(observations, order, definition = prepared)
  expect_identical(fit$metadata$states, definition$states)
  expect_equal(fit$metadata$cycleLength, 7)
  expect_setequal(fit$metadata$absorbingStates, definition$absorbingStates)
  expect_error(estimateStageTransitions(observations, order, 14,
    definition = prepared), "cycleLength conflicts")
  continuous <- prepareTrajectories(x, states = definition$states,
    absorbingStates = definition$absorbingStates)
  expect_error(estimateStageTransitions(observations, order,
    definition = continuous), "cycleLength must be supplied")
  expect_equal(estimateStageTransitions(observations, order, 7,
    definition = continuous), fit)
  expect_equal(estimateStageTransitions(observations, order, 7,
    definition = definition_fixture("Continuous")), fit)
})


test_that("unresolved-only histories retain their empty clinical definition", {
  x <- data.frame(subject_id = c("001", "001"), state_label = "OUT OF COHORT",
    state_start_date = c("2020-01-01", "2020-01-08"),
    state_end_date = c("2020-01-07", "2020-01-14"))
  definition <- list(version = 1L, trajectoryType = "Discrete", cycleLength = 7L,
    states = character(), absorbingStates = character())
  attr(x, "trajectory_definition") <- definition
  prepared <- prepareTrajectories(x)
  expect_identical(prepared$states, character())
  expect_identical(prepared$absorbingStates, character())
  expect_equal(prepared$diagnostics$unresolved_intervals, 2)
  expect_identical(attr(prepared, "trajectory_definition"), definition)
})
