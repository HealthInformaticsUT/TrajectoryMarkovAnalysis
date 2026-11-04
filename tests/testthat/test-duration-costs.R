test_that("duration charges conserve full-event shares across states, periods and missing follow-up", {
  history <- data.frame(
    subject_id = "one", state_label = c("A", "B", "OUT OF COHORT", "Death"),
    state_start_date = as.Date("2020-01-01") + c(1, 3, 5, 7),
    state_end_date = as.Date("2020-01-01") + c(2, 4, 5, 8),
    age_band = c("young", "old", NA, "old")
  )
  prepared <- prepareTrajectories(history, absorbingStates = "Death")
  before <- prepared
  event <- data.frame(subject_id = "one", cost_id = "visit1",
    event_date = as.Date("2020-01-01"), event_end_date = as.Date("2020-01-10"),
    amount = 100, currency_concept_id = "EUR")
  result <- estimateStateCosts(prepared, event, allocation = "duration",
    periodBreaks = c(0, 3, Inf), periodLabels = c("Early", "Late"),
    terminalCostMap = c(Death = "B"), strata = "age_band")

  expect_identical(prepared, before)
  cells <- result$estimates
  expect_equal(cells$total_cost[cells$cost_state == "A"], 20)
  expect_equal(cells$total_cost[cells$cost_state == "B"], c(10, 30))
  expect_equal(sum(cells$observed_days), 6)
  expect_equal(sum(cells$mapped_terminal_days), 2)
  expect_equal(unique(cells$age_band[cells$cost_state == "A"]), "young")
  expect_equal(unique(cells$age_band[cells$cost_state == "B"]), "old")
  expect_equal(result$unallocatedCosts$amount[
    result$unallocatedCosts$reason == "Unresolved trajectory interval"], 10)
  expect_equal(result$unallocatedCosts$amount[
    result$unallocatedCosts$reason == "Outside a supplied interval"], 30)
  expect_equal(sum(cells$known_recorded_cost) + sum(result$unallocatedCosts$amount), 100)
  expect_equal(sum(result$allocatedCosts$allocation_fraction), 0.6)
  expect_equal(unique(result$allocatedCosts$original_amount), 100)
  expect_equal(unique(result$allocatedCosts$.cost_row_id), 1L)
  expect_equal(unlist(result$diagnostics[c("input_cost_records", "allocated_cost_records",
    "unallocated_cost_records", "partially_allocated_cost_records")], use.names = FALSE),
    rep(1L, 4))
  expect_equal(result$metadata$allocation, "duration")
  expect_equal(estimateStateCosts(prepared, event)$diagnostics$allocated_cost_records, 0)
})

test_that("unknown duration amounts affect only overlapping states in their own currency", {
  history <- data.frame(subject_id = "one", state_label = c("A", "B"),
    state_start_date = as.Date(c("2020-01-01", "2020-01-04")),
    state_end_date = as.Date(c("2020-01-03", "2020-01-07")))
  events <- data.frame(subject_id = "one", cost_id = c("1", "2", "3"),
    event_date = as.Date(c("2020-01-01", "2020-01-04", "2020-01-04")),
    event_end_date = as.Date("2020-01-07"), amount = c(700, NA, 40),
    currency_concept_id = c("EUR", "EUR", "USD"))
  result <- estimateStateCosts(history, events, allocation = "duration")
  eur <- subset(result$estimates, currency_concept_id == "EUR")
  usd <- subset(result$estimates, currency_concept_id == "USD")
  expect_equal(eur$total_cost, c(300, NA_real_))
  expect_equal(eur$known_recorded_cost, c(300, 400))
  expect_equal(eur$missing_amounts, c(0, 1))
  expect_equal(usd$total_cost, c(0, 40))
  expect_equal(eur$observed_days, c(3, 4))
  expect_equal(usd$observed_days, c(3, 4))
  expect_equal(result$diagnostics$allocated_cost_records, 3L)
})

test_that("duration requires complete original dates and supports point and empty records", {
  history <- data.frame(subject_id = "one", state_label = "A",
    state_start_date = as.Date("2020-01-01"), state_end_date = as.Date("2020-01-07"))
  event <- data.frame(subject_id = "one", event_date = as.Date("2020-01-04"),
    event_end_date = as.Date("2020-01-04"), amount = 70)
  expect_equal(estimateStateCosts(history, event, allocation = "duration")$estimates,
               estimateStateCosts(history, event)$estimates)
  empty <- estimateStateCosts(history, event[FALSE, ], allocation = "duration")
  expect_equal(empty$estimates$total_cost, 0)
  expect_equal(nrow(empty$allocatedCosts), 0L)
  expect_error(estimateStateCosts(history, event[c("subject_id", "event_date", "amount")],
                                  allocation = "duration"), "requires event_end_date")
  invalid <- event
  invalid$event_end_date <- as.Date(NA)
  expect_error(estimateStateCosts(history, invalid, allocation = "duration"), "invalid dates")
  invalid$event_end_date <- event$event_date - 1
  expect_error(estimateStateCosts(history, invalid, allocation = "duration"), "cannot precede")
  attr(event, "allocation") <- "start"
  expect_error(estimateStateCosts(history, event, allocation = "duration"), "Extract costs again")
})
