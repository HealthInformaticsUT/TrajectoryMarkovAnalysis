test_that("one transition stage agrees with ordinary projection without adding time", {
  states <- c("A", "B")
  kernel <- matrix(c(0.8, 0.2, 0.3, 0.7), 2, byrow = TRUE,
                     dimnames = list(states, states))
  model <- composeTransitionStages(list(change = kernel), cycleLength = 7)
  ordinary <- list(matrices = model$matrices, periods = model$periods,
                    metadata = list(states = states, absorbingStates = character(),
                                      cycleLength = 7))
  staged <- projectCohort(model, 2, initial = c(A = 1), cohortSize = 100)
  expected <- projectCohort(ordinary, 2, initial = c(A = 1), cohortSize = 100)
  expect_equal(staged$occupancy, expected$occupancy)
  expect_equal(staged$steps, expected$steps)
  expect_equal(staged$flows, expected$flows)
  expect_null(expected$stageFlows)
  expect_equal(staged$stageFlows$probability_mass, staged$flows$probability_mass)
  expect_equal(unique(staged$stageFlows$stage), "change")
  expect_equal(unique(staged$stageFlows$stage_index), 1L)
  empty <- projectCohort(model, 0, initial = c(A = 1))
  expect_equal(nrow(empty$stageFlows), 0)
  expect_identical(names(empty$stageFlows), names(staged$stageFlows))
  expect_error(projectCohort(model, 1), "initial|entry|initialization")
})

test_that("ordered reversible stages preserve hidden paths without adding cycle charges", {
  states <- c("A", "B")
  toB <- matrix(c(0, 1, 0, 1), 2, byrow = TRUE,
                  dimnames = list(states, states))
  toA <- matrix(c(1, 0, 1, 0), 2, byrow = TRUE,
                  dimnames = list(states, states))
  model <- composeTransitionStages(list(outward = toB, return = toA), cycleLength = 7)
  result <- projectCohort(model, 1, initial = c(A = 1), cohortSize = 100)
  expect_equal(result$occupancy$expected_count[result$occupancy$state == "A"], c(100, 100))
  expect_equal(unique(result$occupancy$elapsed_day), c(0, 7))
  positive <- subset(result$stageFlows, expected_count > 0)
  expect_equal(positive$stage, c("outward", "return"))
  expect_equal(positive$from_state, c("A", "B"))
  expect_equal(positive$to_state, c("B", "A"))
  expect_equal(positive$expected_count, c(100, 100))
  reverse <- composeTransitionStages(list(return = toA, outward = toB), cycleLength = 7)
  reversed <- projectCohort(reverse, 1, initial = c(A = 1))
  expect_equal(reversed$occupancy$probability[reversed$occupancy$state == "B"], c(0, 1))
  costs <- projectCosts(result, stateCosts = c(A = 2, B = 10),
                         transitionCosts = data.frame(from_state = c("A", "B"),
                                                       to_state = c("B", "A"), cost = c(7, 11)),
                         currency = "EUR", priceYear = 2026)
  expect_equal(costs$byCycle$state_cost, c(200, 200))
  expect_equal(costs$byCycle$transition_cost, c(0, 0))
})

test_that("stage models align named periods and state axes", {
  states <- c("A", "B")
  early <- matrix(c(0.75, 0.25, 0.4, 0.6), 2, byrow = TRUE,
                    dimnames = list(states, states))
  late <- matrix(c(0.2, 0.8, 0.1, 0.9), 2, byrow = TRUE,
                   dimnames = list(states, states))
  unchanged <- diag(2)
  dimnames(unchanged) <- list(states, states)
  model <- composeTransitionStages(
    list(change = list(Late = late[c("B", "A"), c("B", "A")], Early = early),
           remain = unchanged[c("B", "A"), c("B", "A")]),
    cycleLength = 7, periodBreaks = c(0, 7, Inf), periodLabels = c("Early", "Late")
  )
  expect_equal(model$matrices$Early[states, states], early)
  expect_equal(model$matrices$Late[states, states], late)
  expect_equal(names(model$stageMatrices), c("change", "remain"))
  result <- projectCohort(model, 2, initial = c(A = 1), cohortSize = 100)
  expect_equal(result$occupancy$probability[result$occupancy$state == "A"], c(1, 0.75, 0.175))
  expect_equal(result$steps$period, c("Early", "Late"))
  totals <- aggregate(expected_count ~ destination_cycle + stage, result$stageFlows, sum)
  expect_equal(totals$expected_count, rep(100, 4))
})

test_that("unestimated stage rows remain unavailable only when reachable", {
  states <- c("A", "B")
  unchanged <- diag(2)
  dimnames(unchanged) <- list(states, states)
  incomplete <- unchanged
  incomplete["B", ] <- NA_real_
  model <- composeTransitionStages(list(first = unchanged, second = incomplete), cycleLength = 7)
  result <- projectCohort(model, 2, initial = c(A = 1))
  expect_equal(result$occupancy$probability[result$occupancy$state == "A"], c(1, 1, 1))
  absent <- subset(result$stageFlows, stage == "second" & from_state == "B")
  expect_true(all(is.na(absent$transition_probability)))
  expect_true(all(absent$probability_mass == 0))
  expect_error(projectCohort(model, 1, initial = c(B = 1)), "[Uu]nestimated|unavailable|missing")
  toB <- unchanged
  toB["A", ] <- c(0, 1)
  internallyReached <- composeTransitionStages(list(first = toB, second = incomplete), cycleLength = 7)
  expect_error(projectCohort(internallyReached, 1, initial = c(A = 1)),
                 "[Uu]nestimated|unavailable|missing")
})

test_that("absorption and stage products are checked rather than silently repaired", {
  states <- c("A", "D")
  first <- matrix(c(0.8, 0.2, 0, 1), 2, byrow = TRUE,
                    dimnames = list(states, states))
  second <- matrix(c(0.7, 0.3, 0, 1), 2, byrow = TRUE,
                     dimnames = list(states, states))
  model <- composeTransitionStages(list(first = first, second = second),
                                    cycleLength = 7, absorbingStates = "D")
  result <- projectCohort(model, 2, initial = c(A = 1))
  expect_equal(result$occupancy$probability[result$occupancy$state == "D"], c(0, 0.44, 0.6864))
  invalid <- second
  invalid["D", ] <- c(0.1, 0.9)
  expect_error(composeTransitionStages(list(first = first, second = invalid),
                                       cycleLength = 7, absorbingStates = "D"),
                 "[Aa]bsorbing|identity")
  modified <- model
  modified$stageMatrices$first[[1]]["A", ] <- c(0.9, 0.1)
  expect_error(projectCohort(modified, 1, initial = c(A = 1)), "stage|product|compos|match")
  modified <- model
  modified$matrices[[1]]["A", ] <- c(0.6, 0.4)
  expect_error(projectCohort(modified, 1, initial = c(A = 1)), "stage|product|compos|match")
})

test_that("stage names cannot shadow interval accounting meanings", {
  states <- c("Alive", "Dead")
  matrix <- diag(2); dimnames(matrix) <- list(states, states)
  for (name in c("start", "end", "transition")) {
    expect_error(composeTransitionStages(stats::setNames(list(matrix), name), 7),
                 "reserved")
  }
})
