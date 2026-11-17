summary_bundle <- function(a = c(1, 0, 0), b = c(0, 1, 0),
                            absorbing = c("B", "D")) {
  states <- c("A", "B", "D")
  probabilities <- rbind(A = a, B = b, D = c(0, 0, 1))
  colnames(probabilities) <- states
  createModelBundle(list(matrices = list(All = probabilities),
    periods = data.frame(period = "All", start_day = 0, end_day = Inf),
    metadata = list(states = states, absorbingStates = absorbing, cycleLength = 7)))
}

summary_outcome <- function(result, metric, field = "total") {
  result$totals[[field]][match(metric, result$totals$metric)]
}

test_that("summary discounts full labels and explicit charges from a nonzero origin", {
  simulation <- simulateTrajectories(summary_bundle(),
    data.frame(subject_id = "original", state_label = "A", cycle = 2, elapsed_day = 14),
    nSteps = 2, terminalStates = "D", seed = 3,
    costRules = list(setup = list(type = "once", amount = 10),
                     care = list(type = "recurring", amount = 2)),
    currency = "EUR", priceYear = 2025)
  result <- summariseSimulation(simulation, aliveStates = c("A", "B"),
    utilities = c(A = 0.5, B = 1, D = 0), costDiscountRate = 1, healthDiscountRate = 3,
    daysPerYear = 7, discountOriginDay = 7)
  expected <- data.frame(simulation_id = "sim_1", source_subject_id = "original",
    first_day = 14, last_label_day = 28, labelled_cycles = 3, labelled_days = 21,
    cost = 16, discounted_cost = 6.75, life_years = 3,
    discounted_life_years = 1 / 4 + 1 / 16 + 1 / 64,
    qalys = 1.5, discounted_qalys = (1 / 4 + 1 / 16 + 1 / 64) / 2)
  expect_equal(result$byPatient[names(expected)], expected)
  metrics <- c("cost", "discounted_cost", "life_years", "discounted_life_years",
                 "qalys", "discounted_qalys")
  expect_equal(result$totals$metric, metrics)
  expect_equal(result$totals$total, unname(unlist(expected[metrics])))
  expect_equal(result$totals$per_patient, result$totals$total)
  components <- result$costs[match(c("setup", "care"), result$costs$component), ]
  expect_equal(components$cost, c(10, 6))
  expect_equal(components$discounted_cost, c(5, 1.75))
  expect_equal(components$cost_per_patient, components$cost)
  expect_equal(components$discounted_cost_per_patient, components$discounted_cost)
  expect_equal(result$stateTime, data.frame(state_label = c("A", "B", "D"),
    labelled_cycles = c(3, 0, 0), person_days = c(21, 0, 0)))
})

test_that("terminal residency adds no life years and living absorbing states do", {
  simulation <- simulateTrajectories(summary_bundle(a = c(0, 0, 1)),
    data.frame(subject_id = c("initial-terminal", "entry", "living"), state_label = c("D", "A", "B")),
    nSteps = 3, terminalStates = "D", seed = 7,
    costRules = list(care = list(type = "recurring", amount = 2),
      terminal = list(type = "transition", from = "A", to = "D", amount = 100)),
    currency = "EUR", priceYear = 2025)
  result <- summariseSimulation(simulation, aliveStates = c("A", "B"),
    utilities = c(A = 0.5, B = 1, D = 0))
  expect_equal(result$byPatient$source_subject_id, c("initial-terminal", "entry", "living"))
  expect_equal(result$byPatient$labelled_cycles, rep(4, 3))
  expect_equal(result$byPatient$labelled_days, rep(28, 3))
  expect_equal(result$byPatient$life_years, c(0, 7, 28) / 365.25)
  expect_equal(result$byPatient$qalys, c(0, 3.5, 28) / 365.25)
  expect_equal(result$byPatient$cost, c(0, 102, 8))
  expect_equal(summary_outcome(result, "cost"), 110)
  expect_equal(summary_outcome(result, "cost", "per_patient"), 110 / 3)
  expect_equal(summary_outcome(result, "life_years"), 35 / 365.25)
  expect_equal(summary_outcome(result, "life_years", "per_patient"), 35 / (3 * 365.25))
  expect_equal(result$stateTime, data.frame(state_label = c("A", "B", "D"),
    labelled_cycles = c(1, 4, 7), person_days = c(7, 28, 49)))
})

test_that("omitted costs differ from zero charges and repeated source draws remain separate", {
  population <- data.frame(subject_id = "same-source", state_label = "A")
  unconfigured <- simulateTrajectories(summary_bundle(), population, n = 3, replace = TRUE,
    nSteps = 0, terminalStates = "D", seed = 2)
  unknown <- summariseSimulation(unconfigured)
  health <- c("life_years", "discounted_life_years", "qalys", "discounted_qalys")
  expect_true(all(is.na(as.matrix(unknown$byPatient[health]))))
  expect_true(all(is.na(unknown$byPatient$cost)))
  expect_true(is.na(summary_outcome(unknown, "cost")))
  expect_equal(nrow(unknown$costs), 0)
  result <- summariseSimulation(unconfigured, aliveStates = "A",
    utilities = c(A = -0.5, B = 0, D = 0), daysPerYear = 7)
  expect_equal(result$byPatient$source_subject_id, rep("same-source", 3))
  expect_equal(length(unique(result$byPatient$simulation_id)), 3)
  expect_equal(result$byPatient$labelled_cycles, rep(1, 3))
  expect_equal(result$byPatient$labelled_days, rep(7, 3))
  expect_equal(result$byPatient$life_years, rep(1, 3))
  expect_equal(result$byPatient$qalys, rep(-0.5, 3))
  expect_equal(summary_outcome(result, "qalys"), -1.5)
  expect_equal(summary_outcome(result, "qalys", "per_patient"), -0.5)

  zero <- simulateTrajectories(summary_bundle(), population, nSteps = 0,
    costRules = list(dormant = list(type = "recurring", states = "B", amount = 9)),
    currency = "EUR", priceYear = 2025, seed = 2)
  zeroSummary <- summariseSimulation(zero)
  expect_equal(zeroSummary$byPatient$cost, 0)
  expect_equal(summary_outcome(zeroSummary, "cost"), 0)
  expect_equal(zeroSummary$costs, data.frame(component = "dormant", cost = 0,
    discounted_cost = 0, cost_per_patient = 0, discounted_cost_per_patient = 0))

  credit <- simulateTrajectories(summary_bundle(), population, n = 3, replace = TRUE,
    nSteps = 0, costRules = list(credit = list(type = "once", amount = -10)),
    currency = "EUR", priceYear = 2025, seed = 2)
  creditSummary <- summariseSimulation(credit, aliveStates = "A", utilities = c(A = -0.5, B = 0, D = 0))
  expect_equal(summary_outcome(creditSummary, "cost"), -30)
  expect_equal(summary_outcome(creditSummary, "cost", "per_patient"), -10)
  expect_equal(creditSummary$costs$cost_per_patient, -10)
})

test_that("summaries agree across resume without changing input or caller randomness", {
  bundle <- summary_bundle(a = c(0.6, 0.3, 0.1), b = c(0.1, 0.7, 0.2), absorbing = "D")
  population <- data.frame(subject_id = seq_len(12), state_label = "A")
  rules <- list(setup = list(type = "once", amount = 5), care = list(type = "recurring", amount = 2))
  full <- simulateTrajectories(bundle, population, nSteps = 4, terminalStates = "D", seed = 77,
    costRules = rules, currency = "EUR", priceYear = 2025)
  partial <- simulateTrajectories(bundle, population, nSteps = 1, terminalStates = "D", seed = 77,
    costRules = rules, currency = "EUR", priceYear = 2025)
  resumed <- advanceSimulation(unserialize(serialize(partial, NULL)), 3)
  # Summarisation must use saved records without executing callbacks.
  fail <- function(...) stop("A summary must not execute simulation callbacks.")
  environment(fail) <- baseenv()
  full$.config$stopWhen <- resumed$.config$stopWhen <- fail
  full$.config$updateCovariates <- resumed$.config$updateCovariates <- fail
  full$.config$costRules$care$eligible <- resumed$.config$costRules$care$eligible <- fail
  original <- serialize(full, NULL)
  set.seed(818)
  callerSeed <- .Random.seed
  callerKind <- RNGkind()
  first <- summariseSimulation(full, aliveStates = c("A", "B"),
    utilities = c(A = 1, B = 0.5, D = 0), costDiscountRate = 0.03, healthDiscountRate = 0.015)
  second <- summariseSimulation(resumed, aliveStates = c("A", "B"),
    utilities = c(A = 1, B = 0.5, D = 0), costDiscountRate = 0.03, healthDiscountRate = 0.015)
  expect_identical(second, first)
  expect_identical(serialize(full, NULL), original)
  expect_identical(.Random.seed, callerSeed)
  expect_identical(RNGkind(), callerKind)
})

test_that("invalid utility mappings, discount origins and charge ledgers are rejected", {
  simulation <- simulateTrajectories(summary_bundle(), data.frame(state_label = "A"),
    nSteps = 1, terminalStates = "D", seed = 4,
    costRules = list(care = list(type = "recurring", amount = 1)),
    currency = "EUR", priceYear = 2025)
  expect_error(summariseSimulation(simulation, utilities = c(A = 1, B = 0, D = 0)), "aliveStates")
  expect_error(summariseSimulation(simulation, aliveStates = c("A", "D")), "terminal")
  expect_error(summariseSimulation(simulation, aliveStates = "A", utilities = c(A = 0.5)),
                 "state|complete|every")
  expect_error(summariseSimulation(simulation, aliveStates = "A", utilities = c(A = 1.1, B = 0, D = 0)),
                 "utilit")
  expect_error(summariseSimulation(simulation, aliveStates = "A", utilities = c(A = 0.5, B = 0.1, D = 0)),
                 "zero|outside")
  expect_error(summariseSimulation(simulation, discountOriginDay = 1), "origin|Origin")
  bad <- simulation
  bad$costs$simulation_id[1] <- "not-a-simulated-patient"
  expect_error(summariseSimulation(bad), "ID|id|patient|snapshot|ledger")
  bad <- simulation
  bad$costs$elapsed_day[1] <- 1
  expect_error(summariseSimulation(bad), "time|day|label|snapshot|ledger|align")
})
