projection_fixture <- function() {
  states <- c("A", "D")
  early <- matrix(c(0.5, 0, 0.5, 1), 2, dimnames = list(states, states))
  later <- matrix(c(0.8, 0, 0.2, 1), 2, dimnames = list(states, states))
  list(matrices = list(Later = later[c("D", "A"), c("D", "A")], Early = early),
       periods = data.frame(period = c("Early", "Later"), start_day = c(0, 7),
                            end_day = c(7, Inf)),
       metadata = list(states = states, absorbingStates = "D", cycleLength = 7),
       entryCounts = data.frame(state = states, count = c(10, 0), denominator = 10,
                                probability = c(1, 0)))
}

test_that("projection uses source periods and conserves a hand-calculated cohort", {
  result <- projectCohort(projection_fixture(), nCycles = 3, initial = c(A = 1), cohortSize = 100)
  a <- result$occupancy[result$occupancy$state == "A", ]
  d <- result$occupancy[result$occupancy$state == "D", ]
  expect_equal(a$probability, c(1, 0.5, 0.4, 0.32))
  expect_equal(d$probability, c(0, 0.5, 0.6, 0.68))
  expect_equal(a$expected_count, c(100, 50, 40, 32))
  expect_equal(a$elapsed_day, c(0, 7, 14, 21))
  expect_equal(result$steps$period, c("Early", "Later", "Later"))
  expect_equal(result$steps$source_day, c(0, 7, 14))
  expect_equal(a$probability + d$probability, rep(1, 4))
  initialOnly <- projectCohort(projection_fixture(), nCycles = 0)
  expect_equal(nrow(initialOnly$steps), 0)
  expect_equal(initialOnly$occupancy$probability, c(1, 0))
})

test_that("first-cycle deaths persist and unestimated rows are never invented", {
  input <- data.frame(
    subject_id = c("first", "later", "later", "unknown"),
    state_label = c("D", "A", "D", "OUT OF COHORT"),
    state_start_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-08", "2020-01-01")),
    state_end_date = as.Date(c("2020-01-01", "2020-01-07", "2020-01-08", "2020-01-07"))
  )
  model <- estimateTransitions(input, cycleLength = 7, absorbingStates = "D", states = c("A", "U", "D"))
  expect_warning(result <- projectCohort(model, 2), "known|entry|exclude")
  expect_equal(result$occupancy$probability[result$occupancy$state == "D"], c(0.5, 1, 1))
  expect_equal(result$occupancy$probability[result$occupancy$state == "U"], c(0, 0, 0))
  expect_error(projectCohort(model, 1, initial = c(U = 1)), "U")
  model <- projection_fixture()
  model$metadata$absorbingStates <- character()
  model$matrices$Later["D", ] <- NA_real_
  expect_error(projectCohort(model, 2, initial = c(A = 1)), "Later")
})

test_that("projection rejects invalid probabilities, states, horizons and period boundaries", {
  model <- projection_fixture()
  expect_error(projectCohort(model, 1.5), "nCycles")
  expect_error(projectCohort(model, 1, initial = c(A = 0.9)), "sum")
  expect_error(projectCohort(model, 1, initial = c(Unknown = 1)), "state|Unknown")
  bad <- model
  bad$matrices$Early["A", ] <- c(0.4, 0.4)
  expect_error(projectCohort(bad, 1), "sum")
  bad$matrices$Early["A", ] <- c(NA_real_, 0.5)
  expect_error(projectCohort(bad, 1), "missing|NA")
  bad <- model
  bad$matrices$Early["D", ] <- c(1e-12, 1 - 1e-12)
  expect_error(projectCohort(bad, 1), "absorbing|identity")
  bad <- model
  bad$periods$start_day[2] <- bad$periods$end_day[1] <- 8
  expect_error(projectCohort(bad, 1), "align")
})
