episode_fixture <- function(gapDays = 0L) {
  starts <- as.Date("2026-01-01") + c(0, 7 + gapDays)
  data.frame(subject_id = "p1", episode_id = 1:2,
    state_label = c("Stable", "Complication"),
    state_start_date = starts, state_end_date = starts + 6)
}

episode_definition <- function(type = "Discrete") {
  list(version = 2L, trajectoryType = type,
    cycleLength = if (type == "Discrete") 7L else NULL,
    states = c("Stable", "Complication"), absorbingStates = character(),
    identityColumns = c("subject_id", "episode_id"), timeOrigin = "episode_start",
    targetEpisodeHandling = "All", stopAfterAbsorbingEpisode = FALSE)
}

test_that("First-period C2T definitions remain supported", {
  x <- episode_fixture()[1, , drop = FALSE]
  x$episode_id <- NULL
  definition <- episode_definition()[c("version", "trajectoryType", "cycleLength",
                                      "states", "absorbingStates")]
  definition$version <- 1L
  attr(x, "trajectory_definition") <- definition
  prepared <- prepareTrajectories(x)
  expect_identical(prepared$cycleLength, 7L)
  expect_identical(prepared$states, definition$states)
  expect_identical(prepared$data$subject_id, "p1")
  expect_identical(prepared$data$state_label, "Stable")
  expect_identical(attr(prepared, "trajectory_definition"), definition)
})

test_that("C2T All-period definitions explain the supported alternative", {
  for (type in c("Discrete", "Continuous")) {
    x <- episode_fixture()
    attr(x, "trajectory_definition") <- episode_definition(type)
    expect_error(prepareTrajectories(x),
      'Multiple target episodes.*targetEpisodeHandling = "First"')
  }
  x <- episode_fixture()
  definition <- episode_definition()
  definition$version <- 3L
  attr(x, "trajectory_definition") <- definition
  expect_error(prepareTrajectories(x), "versioned list")
  definition <- episode_definition()
  definition$timeOrigin <- NULL
  attr(x, "trajectory_definition") <- definition
  expect_error(prepareTrajectories(x), "versioned list")
  attr(x, "trajectory_definition") <- c(episode_definition(), list(unknown = TRUE))
  expect_error(prepareTrajectories(x), "versioned list")
})

test_that("attribute-free clinical rows cannot join target episodes", {
  for (gapDays in c(0L, 7L)) {
    x <- episode_fixture(gapDays)
    expect_error(prepareTrajectories(x, cycleLength = 7), "Multiple target episodes")
    expect_error(estimateTransitions(x, cycleLength = 7), "Multiple target episodes")
    expect_error(estimateEntryTransitions(x, entryState = "Stable", cycleLength = 7),
      "Multiple target episodes")
    expect_error(prepareTrajectories(x), "Multiple target episodes")
  }
})

test_that("prepared histories are checked before their early return", {
  x <- episode_fixture()
  x$episode_id <- NULL
  prepared <- prepareTrajectories(x, cycleLength = 7)
  prepared$data$episode_id <- 1:2
  expect_error(prepareTrajectories(prepared), "Multiple target episodes")
  expect_error(prepareTrajectories(prepared, cycleLength = 7), "Multiple target episodes")
  expect_error(estimateTransitions(prepared), "Multiple target episodes")
})

test_that("one episode per exact patient identifier remains ordinary metadata", {
  x <- do.call(rbind, lapply(c("001", "1", "9007199254740993123"), function(id) {
    rows <- episode_fixture()
    rows$subject_id <- id
    rows$episode_id <- if (id == "1") "readmission" else "first admission"
    rows
  }))
  prepared <- prepareTrajectories(x, cycleLength = 7)
  expect_identical(prepared$patients$subject_id, c("001", "1", "9007199254740993123"))
  expect_identical(prepared$data$episode_id, x$episode_id)
  expect_identical(prepareTrajectories(prepared), prepared)
  fit <- estimateTransitions(prepared)
  expect_equal(sum(fit$estimates$count), 3)
  expect_equal(fit$matrix["Stable", "Complication"], 1)
  x$episode_id[1] <- NA_character_
  expect_error(prepareTrajectories(x, cycleLength = 7), "episode_id.*missing")
})
