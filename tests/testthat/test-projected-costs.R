projected_cost_fixture <- function() {
  history <- data.frame(
    subject_id = c("first-death", "later-death", "later-death"),
    state_label = c("D", "A", "D"),
    state_start_date = as.Date("2020-01-01") + c(0, 0, 7),
    state_end_date = as.Date("2020-01-01") + c(1, 6, 8)
  )
  prepared <- prepareTrajectories(history, cycleLength = 7,
                                   absorbingStates = "D", states = c("A", "D"))
  list(transitions = estimateTransitions(prepared, periodBreaks = c(0, 7, Inf),
                                         periodLabels = c("First", "Later")),
       entry = estimateEntryTransitions(prepared, entryState = "A"))
}

test_that("first-cycle and later death charges occur once alongside cycle and startup costs", {
  model <- projected_cost_fixture()
  projection <- projectCohort(model$transitions, nCycles = 2, cohortSize = 100, entry = model$entry)
  changeCosts <- data.frame(from_state = "A", to_state = "D", cost = 100)
  costs <- projectCosts(projection, stateCosts = c(A = 10, D = 0),
                         transitionCosts = changeCosts, entryCost = 5,
                         currency = "EUR", priceYear = 2025)
  expect_equal(costs$byCycle$cycle, 0:2)
  expect_equal(costs$byCycle$state_cost, c(500, 0, 0))
  expect_equal(costs$byCycle$transition_cost, c(5000, 5000, 0))
  expect_equal(costs$byCycle$entry_cost, c(500, 0, 0))
  expect_equal(costs$byCycle$total_cost, c(6000, 5000, 0))
  expect_equal(costs$byCycle$cumulative_cost, c(6000, 11000, 11000))
  expect_equal(costs$totals$component, c("state", "transition", "entry", "total"))
  expect_equal(costs$totals$cost, c(500, 10000, 500, 11000))
  expect_equal(costs$totals$cost_per_person, costs$totals$cost / 100)
})

test_that("cost periods use destination-cycle labels rather than probability source periods", {
  states <- c("A", "D")
  transitionMatrix <- matrix(c(0.5, 0, 0.5, 1), 2,
                              dimnames = list(states, states))
  transitions <- list(
    matrices = list("Probability first" = transitionMatrix, "Probability later" = transitionMatrix),
    periods = data.frame(period = c("Probability first", "Probability later"),
                          start_day = c(0, 7), end_day = c(7, Inf)),
    metadata = list(states = states, absorbingStates = "D", cycleLength = 7)
  )
  projection <- projectCohort(transitions, nCycles = 1, initial = c(A = 1), cohortSize = 100)
  stateCosts <- data.frame(state = rep(states, 2), cost = c(2, 0, 3, 0),
                            period = rep(c("Early", "Late"), each = 2))
  changeCosts <- data.frame(from_state = "A", to_state = "D", cost = c(10, 100),
                             period = c("Early", "Late"))
  costs <- projectCosts(projection, stateCosts = stateCosts, transitionCosts = changeCosts,
                         currency = "EUR", priceYear = 2025,
                         periodBreaks = c(0, 7, Inf), periodLabels = c("Early", "Late"))
  expect_equal(projection$steps$period, "Probability first")
  expect_equal(costs$byCycle$elapsed_day, c(0, 7))
  expect_equal(costs$byCycle$period, c("Early", "Late"))
  expect_equal(costs$byCycle$state_cost, c(200, 150))
  expect_equal(costs$byCycle$transition_cost, c(0, 5000))
  expect_equal(costs$byCycle$total_cost, c(200, 5150))
  death <- subset(costs$details$transition, from_state == "A" & to_state == "D" & expected_count > 0)
  expect_equal(death$probability_period, "Probability first")
  expect_equal(death$cost_period, "Late")
  expect_equal(death$unit_cost, 100)
})

test_that("zero-step projections cost cycle zero and only explicitly modelled entry changes", {
  model <- projected_cost_fixture()
  withEntry <- projectCohort(model$transitions, nCycles = 0, cohortSize = 10, entry = model$entry)
  withoutEntry <- projectCohort(model$transitions, nCycles = 0, cohortSize = 10)
  changeCosts <- data.frame(from_state = "A", to_state = "D", cost = 100)
  entryCosts <- projectCosts(withEntry, stateCosts = c(A = 2, D = 0),
                              transitionCosts = changeCosts, entryCost = 3,
                              currency = "EUR", priceYear = 2025)
  ordinaryCosts <- projectCosts(withoutEntry, stateCosts = c(A = 2, D = 0),
                                 transitionCosts = changeCosts,
                                 currency = "EUR", priceYear = 2025)
  expect_equal(entryCosts$byCycle$state_cost, 10)
  expect_equal(entryCosts$byCycle$transition_cost, 500)
  expect_equal(entryCosts$byCycle$entry_cost, 30)
  expect_equal(entryCosts$byCycle$total_cost, 540)
  expect_equal(ordinaryCosts$byCycle$state_cost, 10)
  expect_equal(ordinaryCosts$byCycle$transition_cost, 0)
  expect_equal(ordinaryCosts$byCycle$entry_cost, 0)
  expect_equal(ordinaryCosts$byCycle$total_cost, 10)
})

test_that("cost schedules require complete states, valid unique changes, and explicit entry", {
  model <- projected_cost_fixture()
  projection <- projectCohort(model$transitions, nCycles = 0, cohortSize = 10)
  expect_error(projectCosts(projection, stateCosts = c(A = 1), currency = "EUR", priceYear = 2025),
                "cover|complete|stateCosts|state")
  duplicate <- data.frame(state = c("A", "A", "D"), cost = c(1, 2, 0))
  expect_error(projectCosts(projection, stateCosts = duplicate, currency = "EUR", priceYear = 2025),
                "duplicate|unique|once")
  incompletePeriods <- data.frame(state = c("A", "D"), cost = c(1, 0), period = "Early")
  expect_error(projectCosts(projection, stateCosts = incompletePeriods, currency = "EUR", priceYear = 2025,
                              periodBreaks = c(0, 7, Inf), periodLabels = c("Early", "Late")),
                "cover|complete|period|state")
  selfCost <- data.frame(from_state = "D", to_state = "D", cost = 100)
  expect_error(projectCosts(projection, transitionCosts = selfCost, currency = "EUR", priceYear = 2025),
                "self|change|same")
  duplicateChanges <- data.frame(from_state = c("A", "A"), to_state = c("D", "D"), cost = c(1, 2))
  expect_error(projectCosts(projection, transitionCosts = duplicateChanges, currency = "EUR", priceYear = 2025),
                "duplicate|unique|once")
  expect_error(projectCosts(projection, stateCosts = c(A = Inf, D = 0), currency = "EUR", priceYear = 2025),
                "finite")
  expect_error(projectCosts(projection, entryCost = 5, currency = "EUR", priceYear = 2025), "entry")
})
