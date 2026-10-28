test_that("costs use inclusive observed days and remain independent of transitions", {
  history <- data.frame(
    subject_id = rep("09007199254740993", 3),
    state_label = c("A", "A", "Death"),
    state_start_date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15")),
    state_end_date = as.Date(c("2020-01-07", "2020-01-14", "2020-01-17"))
  )
  events <- data.frame(
    subject_id = rep("09007199254740993", 3),
    event_date = as.Date(c("2020-01-07", "2020-01-15", "2020-01-17")),
    amount = c(70, 20, 10),
    currency_concept_id = "EUR", measure = "total_charge"
  )
  result <- estimateStateCosts(history, events)
  expect_equal(result$estimates$total_cost, c(70, 30))
  expect_equal(result$estimates$observed_days, c(14L, 3L))
  expect_equal(result$estimates$mean_cost_per_day, c(5, 10))
  expect_equal(result$estimates$intervals_without_records, c(1L, 0L))
  expect_identical(result$intervalCosts$subject_id, history$subject_id)
  wrapper <- TrajectoryMarkovAnalysis(history, analyses = "costs", costData = events)
  expect_null(wrapper$transitions)
  expect_equal(wrapper$costs$estimates, result$estimates)
})

test_that("missing amounts and currencies are not pooled or treated as zero", {
  history <- data.frame(subject_id = "001", state_label = "A",
                        state_start_date = as.Date("2020-01-01"),
                        state_end_date = as.Date("2020-01-07"))
  events <- data.frame(subject_id = rep("001", 3),
                      event_date = rep(as.Date("2020-01-02"), 3),
                      amount = c(7, NA_real_, 14),
                      currency_concept_id = c("EUR", "EUR", "USD"))
  result <- estimateStateCosts(history, events)
  expect_equal(result$estimates$currency_concept_id, c("EUR", "USD"))
  expect_equal(result$estimates$known_recorded_cost, c(7, 14))
  expect_true(is.na(result$estimates$total_cost[1]))
  expect_true(is.na(result$estimates$mean_cost_per_day[1]))
  expect_equal(result$estimates$total_cost[2], 14)
  events$cost_id <- c("1", "1", "2")
  expect_error(estimateStateCosts(history, events), "cost_id must be unique")
  events$cost_id <- c("1", "2", "3")
  events$event_date[1] <- events$event_date[1] + 0.5
  expect_error(estimateStateCosts(history, events), "invalid dates")
})

test_that("costs in gaps, unknown states and other patients remain visible", {
  history <- data.frame(subject_id = rep("001", 2),
                        state_label = c("A", "OUT OF COHORT"),
                        state_start_date = as.Date(c("2020-01-01", "2020-01-08")),
                        state_end_date = as.Date(c("2020-01-07", "2020-01-14")))
  events <- data.frame(subject_id = c("001", "001", "002"),
                      event_date = as.Date(c("2020-01-09", "2020-01-15", "2020-01-03")),
                      amount = c(10, 20, 30))
  result <- estimateStateCosts(history, events)
  expect_equal(result$estimates$total_cost, 0)
  expect_equal(result$diagnostics$unallocated_cost_records, 3L)
  expect_setequal(result$unallocatedCosts$reason, c("Unresolved trajectory interval",
                    "Outside a supplied interval", "Patient not in trajectories"))
  empty <- estimateStateCosts(history, events[FALSE, ])
  expect_equal(empty$estimates$total_cost, 0)
})
