test_that("the combined interface supports explicit and extracted duration allocation", {
  histories <- prepareTrajectories(data.frame(
    subject_id = c("p1", "p1"),
    state_label = c("Alive", "History"),
    state_start_date = as.Date(c("2020-01-01", "2020-01-04")),
    state_end_date = as.Date(c("2020-01-03", "2020-01-07"))
  ))
  events <- data.frame(
    subject_id = "p1", event_date = as.Date("2020-01-01"),
    event_end_date = as.Date("2020-01-07"), amount = 700
  )
  explicit <- TrajectoryMarkovAnalysis(
    histories, analyses = "costs", costData = events, costAllocation = "duration"
  )
  direct <- estimateStateCosts(histories, events, allocation = "duration")
  expect_identical(explicit$costs, direct)
  expect_equal(explicit$costs$estimates$total_cost, c(300, 400))
  attr(events, "allocation") <- "duration"
  inferred <- TrajectoryMarkovAnalysis(histories, analyses = "costs", costData = events)
  expect_equal(inferred$costs$estimates, direct$estimates)
  expect_error(TrajectoryMarkovAnalysis(histories, costAllocation = "duration"),
               "Include 'costs'.*costAllocation")
})
