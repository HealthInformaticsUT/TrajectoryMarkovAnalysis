estimated_simulation_fixture <- function(terminal = "none", mapped = FALSE,
                                        omitOlder = FALSE, die = FALSE) {
  history <- data.frame(subject_id = c("young", "older"), state_label = "A",
    state_start_date = as.Date("2024-01-01"), state_end_date = as.Date("2024-01-14"),
    age_group = c("young", "older"))
  events <- data.frame(subject_id = rep(c("young", "older"), each = 2),
    event_date = rep(as.Date(c("2024-01-01", "2024-01-04")), 2),
    amount = c(30, 220, 90, 440), currency_concept_id = "EUR")
  if (omitOlder) {
    history <- history[history$subject_id == "young", , drop = FALSE]
    events <- events[events$subject_id == "young", , drop = FALSE]
  }
  prepared <- prepareTrajectories(history, states = c("A", "D"), absorbingStates = "D")
  estimates <- estimateStateCosts(prepared, events, strata = "age_group",
    periodBreaks = c(0, 3, Inf), periodLabels = c("Early cost", "Later cost"),
    terminalCostMap = if (mapped) c(D = "A") else NULL)
  costModel <- createCostModel(estimates, unit = "day", currency = "EUR", priceYear = 2025,
    terminal = terminal)
  states <- c("A", "D")
  matrix <- if (die) matrix(c(0, 0, 1, 1), 2) else diag(2)
  dimnames(matrix) <- list(states, states)
  transitions <- list(matrices = list(All = matrix),
    periods = data.frame(period = "All", start_day = 0, end_day = Inf),
    metadata = list(states = states, absorbingStates = "D", cycleLength = 7))
  grouping <- list(age_group = list(column = "age", breaks = c(0, 65, Inf),
    labels = c("young", "older")))
  list(bundle = createModelBundle(transitions, costs = costModel, groups = grouping),
       transitions = transitions, estimates = estimates, grouping = grouping)
}

test_that("learned costs follow updated groups and split daily cost periods independently", {
  fixture <- estimated_simulation_fixture()
  population <- data.frame(state_label = "A", index_date = as.Date("2025-01-01"),
    birth_date = as.Date("1960-01-08"))
  result <- simulateTrajectories(fixture$bundle, population, nSteps = 2, seed = 12)
  expect_equal(result$costs$component, rep("healthcare", 4))
  expect_equal(result$costs$type, rep("estimated", 4))
  expect_equal(result$costs$cost_period, c("Early cost", rep("Later cost", 3)))
  expect_equal(result$costs$age_group, c("young", "young", "older", "older"))
  expect_equal(result$costs$unit_cost, c(10, 20, 40, 40))
  expect_equal(result$costs$quantity, c(3, 4, 7, 7))
  expect_equal(result$costs$amount, c(30, 80, 280, 280))
  expect_equal(result$totals, data.frame(component = "healthcare", amount = 670))
  expect_equal(result$metadata$currency, "EUR")
  expect_equal(result$metadata$priceYear, 2025)
  expect_equal(result$metadata$cost_model$unit, "day")
  expect_equal(result$transitions$source_period, rep("All", 2))
})

test_that("estimated healthcare and explicit rules retain separate totals across resume", {
  fixture <- estimated_simulation_fixture()
  population <- data.frame(state_label = "A", age = 64)
  update <- function(current, previous) data.frame(simulation_id = current$simulation_id,
    age = current$age + 1)
  rules <- list(treatment = list(type = "once", amount = 5))
  full <- simulateTrajectories(fixture$bundle, population, nSteps = 2,
    updateCovariates = update, costRules = rules, seed = 74)
  first <- simulateTrajectories(fixture$bundle, population, nSteps = 1,
    updateCovariates = update, costRules = rules, seed = 74)
  first <- unserialize(serialize(first, NULL))
  resumed <- advanceSimulation(first, 1)
  expect_identical(resumed$costs, full$costs)
  expect_identical(resumed$totals, full$totals)
  expect_identical(resumed$.state$rng, full$.state$rng)
  expect_equal(full$totals, data.frame(component = c("healthcare", "treatment"),
    amount = c(670, 5)))
  expect_true(all(is.na(full$costs$unit_cost[full$costs$component == "treatment"])))
  expect_equal(first$totals$amount, c(390, 5))

  noCost <- createModelBundle(fixture$transitions)
  withoutCosts <- simulateTrajectories(noCost, population, nSteps = 2,
    updateCovariates = update, costRules = rules, currency = "EUR", priceYear = 2025,
    seed = 74)
  expect_identical(full$transitions, withoutCosts$transitions)
  expect_identical(full$.state$rng, withoutCosts$.state$rng)
})

test_that("mapped terminal entry uses retained source covariates and is charged once", {
  fixture <- estimated_simulation_fixture(terminal = "entry", mapped = TRUE, die = TRUE)
  population <- data.frame(state_label = c("A", "D"), age = c(64, 80))
  result <- simulateTrajectories(fixture$bundle, population, nSteps = 4,
    terminalStates = "D", seed = 7)
  expect_equal(result$metadata$steps, 1)
  expect_equal(result$costs$simulation_id, rep("sim_1", 3))
  expect_equal(result$costs$state_label, c("A", "A", "D"))
  expect_equal(result$costs$cost_state, rep("A", 3))
  expect_equal(result$costs$age_group, rep("young", 3))
  expect_equal(result$costs$elapsed_day, c(0, 0, 7))
  expect_equal(result$totals$amount, 250)
  expect_identical(advanceSimulation(result, 2), result)
  initialTerminal <- simulateTrajectories(fixture$bundle, data.frame(state_label = "D"),
    nSteps = 2, terminalStates = "D", seed = 7)
  expect_equal(nrow(initialTerminal$costs), 0)
  expect_equal(initialTerminal$totals, data.frame(component = "healthcare", amount = 0))
  suppressed <- estimated_simulation_fixture(terminal = "none", mapped = TRUE, die = TRUE)
  noEntry <- simulateTrajectories(suppressed$bundle, population, nSteps = 4,
    terminalStates = "D", seed = 7)
  expect_equal(noEntry$totals$amount, 110)
})

test_that("model labels, reserved components and reachable missing cost cells fail clearly", {
  fixture <- estimated_simulation_fixture()
  population <- data.frame(state_label = "A", age = 64)
  expect_error(simulateTrajectories(fixture$bundle, population, currency = "USD"),
    "must match")
  expect_error(simulateTrajectories(fixture$bundle, population, priceYear = 2024),
    "must match")
  expect_error(simulateTrajectories(fixture$bundle, population,
    costRules = list(healthcare = list(type = "recurring", amount = 3))), "reserved")
  expect_error(simulateTrajectories(fixture$bundle, n = 1, initial = c(A = 1)),
    "covariates|joint")
  missing <- estimated_simulation_fixture(omitOlder = TRUE)
  expect_error(simulateTrajectories(missing$bundle, data.frame(state_label = "A", age = 65),
    nSteps = 0), "cost|Cost")
  expect_error(simulateTrajectories(missing$bundle, population, nSteps = 1,
    updateCovariates = function(current, previous) data.frame(
      simulation_id = current$simulation_id, age = 65)), "cost|Cost")
  mapped <- estimated_simulation_fixture(terminal = "entry", mapped = TRUE)
  expect_error(simulateTrajectories(mapped$bundle, population), "terminalStates")

  descriptive <- createModelBundle(fixture$transitions, costs = fixture$estimates)
  unchanged <- simulateTrajectories(descriptive, population, nSteps = 1, seed = 2)
  expect_equal(nrow(unchanged$costs), 0)
  expect_equal(nrow(unchanged$totals), 0)
  expect_null(unchanged$metadata$cost_model)
})
