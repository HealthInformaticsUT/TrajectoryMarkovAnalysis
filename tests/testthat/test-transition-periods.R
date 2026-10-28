test_that("period estimates use index-relative source cycles and preserve empty periods", {
  input <- data.frame(
    subject_id = c("one", "one", "one", "two", "two", "first-death"),
    state_label = c("A", "B", "Dead", "A", "A", "Dead"),
    state_start_date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15",
                                 "2021-03-01", "2021-03-08", "2020-05-01")),
    state_end_date = as.Date(c("2020-01-07", "2020-01-14", "2020-01-17",
                               "2021-03-07", "2021-03-10", "2020-05-02"))
  )
  history <- prepareTrajectories(input, 7, absorbingStates = "Dead",
                                 states = c("A", "B", "Dead", "Unobserved"))
  model <- estimateTransitions(history, periodBreaks = c(0, 7, 14, Inf),
                                periodLabels = c("First week", "Second week", "Later"))
  expect_null(model$matrix)
  expect_identical(names(model$matrices), c("First week", "Second week", "Later"))
  expect_equal(model$matrices[[1]]["A", c("A", "B")], c(A = 0.5, B = 0.5))
  expect_equal(model$matrices[[2]]["B", "Dead"], 1)
  expect_true(all(is.na(model$matrices[[3]][c("A", "B", "Unobserved"), ])))
  expect_equal(model$matrices[[3]]["Dead", "Dead"], 1)
  expect_equal(model$diagnostics$pairs_by_period$observed_transition_pairs, c(2, 1, 0))
  expect_equal(model$entryCounts$count, c(2, 0, 1, 0))
  pooled <- estimateTransitions(history)
  sums <- aggregate(count ~ from_state + to_state, model$estimates, sum)
  expected <- aggregate(count ~ from_state + to_state, pooled$estimates, sum)
  expect_equal(sums, expected)
  wrapped <- TrajectoryMarkovAnalysis(history, periodBreaks = c(0, 7, 14, Inf),
                                       periodLabels = c("First week", "Second week", "Later"))
  expect_equal(wrapped$transitions, model)
})

test_that("period boundaries cannot silently omit follow-up or split transition cycles", {
  expect_error(.tmaPeriods(c(7, Inf)), "starting at 0")
  expect_error(.tmaPeriods(c(0, 7)), "ending at Inf")
  expect_error(.tmaPeriods(c(0, 7, 7, Inf)), "strictly increasing")
  expect_error(.tmaPeriods(c(0, NA, Inf)), "strictly increasing")
  expect_error(.tmaPeriods(c(0, 1.5, Inf)), "whole numbers")
  expect_error(.tmaPeriods(c(0, 7, Inf), c("Same", "Same")), "unique")
  expect_error(.tmaPeriods(c(0, 7, Inf), "One"), "one unique")
  expect_error(.tmaPeriods(c(0, 8, Inf), cycleLength = 7), "align")
  expect_equal(.tmaPeriodIndex(c(0, 6, 7, 1000), .tmaPeriods(c(0, 7, Inf))),
               c(1L, 1L, 2L, 2L))
})
