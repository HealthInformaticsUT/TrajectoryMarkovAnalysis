test_that("terminal costs and days move without changing clinical labels or transitions", {
  input <- data.frame(
    subject_id = c("later", "later", "first", "living", "unmapped"),
    state_label = c("Alive", "Death history", "Death history", "History", "Other death"),
    state_start_date = as.Date(c("2020-01-01", "2020-01-08", "2020-02-01",
                                 "2020-03-01", "2020-04-01")),
    state_end_date = as.Date(c("2020-01-07", "2020-01-10", "2020-02-02",
                               "2020-03-07", "2020-04-01"))
  )
  histories <- prepareTrajectories(input, 7,
                                   absorbingStates = c("Death history", "Other death"))
  before <- histories
  events <- data.frame(
    subject_id = c("later", "later", "later", "later", "first", "living", "unmapped"),
    event_date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-09", "2020-01-10",
                           "2020-02-02", "2020-03-01", "2020-04-01")),
    amount = c(70, 30, NA, 5, 20, 70, 9),
    currency_concept_id = c("EUR", "EUR", "EUR", "USD", "EUR", "EUR", "EUR")
  )
  map <- c("Death history" = "History")
  result <- TrajectoryMarkovAnalysis(histories, analyses = c("transitions", "costs"),
    costData = events, periodBreaks = c(0, 7, Inf), periodLabels = c("First", "Later"),
    terminalCostMap = map)
  unchanged <- TrajectoryMarkovAnalysis(histories, analyses = c("transitions", "costs"),
    costData = events, periodBreaks = c(0, 7, Inf), periodLabels = c("First", "Later"))
  expect_identical(histories, before)
  expect_identical(result$trajectories, unchanged$trajectories)
  expect_identical(result$transitions, unchanged$transitions)
  rows <- result$costs$intervalCosts
  expect_identical(rows$state_label, unchanged$costs$intervalCosts$state_label)
  expect_identical(rows$.row_id, unchanged$costs$intervalCosts$.row_id)
  expect_true(all(rows$cost_state[rows$state_label == "Death history"] == "History"))
  estimates <- result$costs$estimates
  eur <- estimates[estimates$cost_state == "History" & estimates$currency_concept_id == "EUR", ]
  expect_equal(eur$patients, c(2, 1))
  expect_equal(eur$observed_days, c(9, 3))
  expect_equal(eur$mapped_terminal_days, c(2, 3))
  expect_equal(eur$total_cost, c(90, NA_real_))
  expect_equal(eur$known_recorded_cost, c(90, 30))
  expect_equal(eur$mean_cost_per_day, c(10, NA_real_))
  expect_true("Other death" %in% estimates$cost_state)
  groupTotals <- function(x) aggregate(cbind(observed_days, known_recorded_cost, missing_amounts) ~
                                        period + currency_concept_id, x$estimates, sum)
  expect_equal(groupTotals(result$costs), groupTotals(unchanged$costs))
  expect_equal(unlist(result$costs$diagnostics[c("mapped_terminal_patients",
    "mapped_terminal_intervals", "mapped_terminal_days")], use.names = FALSE), c(2, 2, 5))
  expect_identical(result$costs$metadata$terminal_cost_map, map)
})

test_that("mapping works for first-cycle death and defined unobserved destinations", {
  input <- data.frame(subject_id = c("first", "second"), state_label = c("D1", "D2"),
    state_start_date = as.Date(c("2020-01-01", "2020-02-01")),
    state_end_date = as.Date(c("2020-01-03", "2020-02-02")))
  history <- prepareTrajectories(input, absorbingStates = c("D1", "D2"),
                                 states = c("Living", "D1", "D2"))
  events <- data.frame(subject_id = c("first", "first", "second"),
    event_date = as.Date(c("2020-01-01", "2020-01-03", "2020-02-02")), amount = c(4, 6, 8))
  map <- c(D1 = "Living", D2 = "Living")
  result <- estimateStateCosts(history, events, periodBreaks = c(0, 2, Inf),
                               periodLabels = c("Early", "Late"), terminalCostMap = map)
  expect_equal(result$estimates$cost_state, c("Living", "Living"))
  expect_equal(result$estimates$observed_days, c(4, 1))
  expect_equal(result$estimates$total_cost, c(12, 6))
  expect_equal(result$diagnostics$mapped_terminal_intervals, 2)
  expect_equal(result$diagnostics$mapped_terminal_days, 5)
  expect_equal(sort(result$intervalCosts$.row_id), c(1, 1, 2))
  expect_error(estimateStateCosts(input, events, terminalCostMap = map), "declared absorbingStates")
  expect_error(estimateStateCosts(history, events, terminalCostMap = c("Living")), "source-state names")
  expect_error(estimateStateCosts(history, events, terminalCostMap = c(D1 = "Living", D1 = "Living")), "unique")
  expect_error(estimateStateCosts(history, events, terminalCostMap = c(D1 = "Unknown")), "defined nonabsorbing")
  expect_error(estimateStateCosts(history, events, terminalCostMap = c(D1 = "D2")), "defined nonabsorbing")
  expect_error(TrajectoryMarkovAnalysis(history, terminalCostMap = map), "Include 'costs'")
})
