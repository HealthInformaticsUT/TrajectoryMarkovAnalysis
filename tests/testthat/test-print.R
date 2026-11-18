test_that("compact printing returns unchanged objects without exposing internals", {
  histories <- prepareTrajectories(data.frame(
    subject_id = c("private-id", "private-id"), state_label = c("A", "D"),
    state_start_date = as.Date(c("2020-01-01", "2020-01-08")),
    state_end_date = as.Date(c("2020-01-07", "2020-01-08"))),
    cycleLength = 7, states = c("A", "D"), absorbingStates = "D")
  fit <- estimateTransitions(histories)
  bundle <- createModelBundle(fit)
  projection <- projectCohort(fit, 1, initial = c(A = 1), cohortSize = 10)
  simulation <- simulateTrajectories(bundle, n = 10, initial = c(A = 1),
    nSteps = 1, terminalStates = "D", seed = 4)
  costs <- estimateStateCosts(histories, data.frame(subject_id = "private-id",
    event_date = as.Date("2020-01-01"), amount = 70, currency_concept_id = "EUR"))
  costModel <- createCostModel(costs, unit = "day", currency = "EUR", priceYear = 2026, terminal = "none")
  rewards <- createRewardModel(stateCosts = c(A = 70, D = 0),
    aliveStates = "A", utilities = c(A = .8, D = 0), currency = "EUR", priceYear = 2026)
  outcomes <- summariseOutcomes(projection, rewards)
  comparison <- compareScenarios(outcomes, outcomes)
  labelSummary <- summariseSimulation(simulation, aliveStates = "A", utilities = c(A = .8, D = 0))
  objects <- list(histories, fit, bundle, projection, simulation, costs,
    costModel, rewards, outcomes, comparison, labelSummary)
  for (object in objects) {
    before <- serialize(object, NULL)
    output <- capture.output(returned <- withVisible(print(object)))
    expect_identical(returned$value, object)
    expect_false(returned$visible)
    expect_identical(serialize(object, NULL), before)
    expect_lt(length(output), 25)
    expect_false(any(grepl("private-id|\\$\\.state|\\$\\.config", output)))
  }
  expect_output(print(outcomes), "Completed-interval")
  expect_output(print(labelSummary), "Labelled-cycle")
  expect_output(print(comparison), "per patient")
})

test_that("model printing distinguishes stages, strata and unsupported rows", {
  input <- data.frame(subject_id = c("p1", "p1", "p2", "p2"),
    state_label = c("A", "D", "A", "D"), group = c("young", "young", "old", "old"),
    state_start_date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-01", "2020-01-08")),
    state_end_date = as.Date(c("2020-01-07", "2020-01-08", "2020-01-07", "2020-01-08")))
  stratified <- estimateTransitions(input, cycleLength = 7, states = c("A", "B", "D"),
    absorbingStates = "D", strata = "group")
  expect_output(print(stratified), "2 stratum/strata")
  expect_output(print(stratified), "2 unsupported source row")
  grouped_costs <- estimateStateCosts(prepareTrajectories(input, cycleLength = 7, absorbingStates = "D"),
    data.frame(subject_id = c("p1", "p2"), event_date = as.Date("2020-01-01"),
      amount = c(70, 140), currency_concept_id = "EUR"), strata = "group")
  grouped_model <- createCostModel(grouped_costs, unit = "day", currency = "EUR", priceYear = 2026, terminal = "none")
  expect_output(print(grouped_costs), "group")
  expect_output(print(grouped_model), "group")
  stage <- diag(2); dimnames(stage) <- list(c("A", "D"), c("A", "D"))
  model <- composeTransitionStages(list(onset = stage, mortality = stage), 7, "D")
  expect_output(print(model), "Stage order: onset -> mortality")
  expect_output(print(projectCohort(model, 0, initial = c(A = 1))), "0 transition steps")
})
