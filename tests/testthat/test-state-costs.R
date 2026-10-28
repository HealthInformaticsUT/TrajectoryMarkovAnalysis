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

test_that("period costs split days and events at patient-relative boundaries without loss", {
  history <- data.frame(
    subject_id = c("001", "002"), state_label = "A",
    state_start_date = as.Date(c("2020-01-01", "2020-02-01")),
    state_end_date = as.Date(c("2020-01-12", "2020-02-04"))
  )
  events <- data.frame(
    subject_id = c(rep("001", 7), "002"),
    event_date = as.Date(c("2020-01-01", "2020-01-03", "2020-01-04", "2020-01-08",
                          "2020-01-09", "2020-01-12", "2020-01-13", "2020-02-04")),
    amount = c(1:6, 100, 40), currency_concept_id = "EUR"
  )
  result <- estimateStateCosts(history, events, periodBreaks = c(0, 3, 8, 30, Inf),
                               periodLabels = c("Early", "Middle", "Late", "Unobserved"))
  pooled <- estimateStateCosts(history, events)
  expect_equal(result$estimates$period, c("Early", "Middle", "Late"))
  expect_equal(result$estimates$observed_days, c(6, 6, 4))
  expect_equal(result$estimates$total_cost, c(3, 47, 11))
  expect_equal(sum(result$estimates$total_cost), pooled$estimates$total_cost)
  expect_equal(sum(result$estimates$observed_days), pooled$estimates$observed_days)
  expect_equal(sum(result$estimates$cost_records), pooled$diagnostics$allocated_cost_records)
  firstSubject <- result$intervalCosts[result$intervalCosts$subject_id == "001", ]
  expect_equal(firstSubject$.row_id, rep(1L, 3))
  expect_equal(firstSubject$state_start_date, as.Date(c("2020-01-01", "2020-01-04", "2020-01-09")))
  expect_equal(firstSubject$state_end_date, as.Date(c("2020-01-03", "2020-01-08", "2020-01-12")))
  expect_equal(firstSubject$observed_days, c(3, 5, 4))
  expect_equal(result$unallocatedCosts$amount, 100)
  expect_equal(result$unallocatedCosts$reason, "Outside a supplied interval")
  expect_equal(result$metadata$periods, result$periods)
  expect_equal(nrow(result$periods), 4L)
})

test_that("missing amounts stay within their period and currency after splitting", {
  history <- data.frame(subject_id = "001", state_label = "A",
                        state_start_date = as.Date("2020-01-01"),
                        state_end_date = as.Date("2020-01-10"))
  events <- data.frame(subject_id = "001",
                      event_date = as.Date(c("2020-01-05", "2020-01-06", "2020-01-06")),
                      amount = c(5, NA_real_, 10), currency_concept_id = c("EUR", "EUR", "USD"))
  result <- estimateStateCosts(history, events, periodBreaks = c(0, 5, Inf),
                               periodLabels = c("First", "Second"))
  eur <- result$estimates[result$estimates$currency_concept_id == "EUR", ]
  usd <- result$estimates[result$estimates$currency_concept_id == "USD", ]
  expect_equal(eur$period, c("First", "Second"))
  expect_equal(eur$total_cost, c(5, NA_real_))
  expect_equal(eur$missing_amounts, c(0, 1))
  expect_equal(eur$known_recorded_cost, c(5, 0))
  expect_equal(usd$total_cost, c(0, 10))
  expect_equal(eur$observed_days, c(5, 5))
  expect_equal(result$diagnostics$allocated_cost_records, 3L)
  expect_equal(result$diagnostics$unallocated_cost_records, 0L)
})
