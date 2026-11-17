test_that("interval rewards retain stage costs, first-cycle deaths and the exact horizon", {
  states <- c("A", "B", "D")
  onset <- mortality <- diag(3)
  dimnames(onset) <- dimnames(mortality) <- list(states, states)
  onset["A", ] <- c(0, 1, 0)
  mortality["B", ] <- c(0, 0, 1)
  model <- composeTransitionStages(list(onset = onset, mortality = mortality), 7, "D")
  rewards <- createRewardModel(stateCosts = c(A = 10, B = 30, D = 0),
    upfrontCosts = c(program = 100), aliveStates = c("A", "B"),
    utilities = c(A = .8, B = .4, D = 0), costTiming = "onset",
    currency = "EUR", priceYear = 2026, costDiscountRate = .1, healthDiscountRate = .1)
  projection <- projectCohort(model, 1, initial = c(A = 1), cohortSize = 10)
  out <- summariseOutcomes(projection, rewards)
  expect_equal(out$totals$total[out$totals$metric == "cost"], 1300)
  expect_equal(out$totals$total[out$totals$metric == "life_years"], 70 / 365.25)
  expect_equal(out$totals$total[out$totals$metric == "discounted_cost"], 1000 + 300 / 1.1^(3.5 / 365.25))
  sim <- simulateTrajectories(createModelBundle(model), n = 10, initial = c(A = 1),
    nSteps = 1, terminalStates = "D", seed = 8)
  sampled <- summariseOutcomes(sim, rewards)
  expect_equal(sampled$totals, out$totals)
  expect_equal(compareScenarios(out, out)$costEffectiveness$status, rep("no_difference", 2))
  expect_error(summariseOutcomes(sim, createRewardModel(stateCosts = c(A=1,B=1,D=0),
    costTiming = "typo", currency="EUR", priceYear=2026)), "Unknown reward stage")

  idle <- simulateTrajectories(createModelBundle(model), n = 10, initial = c(A = 1), nSteps = 0)
  fees <- summariseOutcomes(idle, rewards)
  expect_equal(fees$totals$total[fees$totals$metric == "cost"], 1000)
  expect_equal(fees$totals$total[fees$totals$metric == "life_years"], 0)
  identity <- diag(3); dimnames(identity) <- list(states, states)
  stable <- projectCohort(composeTransitionStages(list(unchanged=identity), 7, "D"),
    52, initial=c(A=1), cohortSize=10)
  year <- summariseOutcomes(stable, createRewardModel(aliveStates=c("A","B")))
  expect_equal(year$metadata$accounted_days, 364)
  expect_equal(year$totals$total[year$totals$metric == "life_years"], 3640 / 365.25)
})

test_that("explicit weekly rewards are not silently converted from daily means", {
  expect_error(createRewardModel(stateCosts=data.frame(state=character(),cost=numeric())), "empty")
  states <- c("A", "D"); m <- diag(2); dimnames(m) <- list(states, states)
  p <- projectCohort(composeTransitionStages(list(unchanged=m),7,"D"), 2,
    initial=c(A=1), cohortSize=2)
  r <- createRewardModel(stateCosts=data.frame(state=rep(states,2),
    period=rep(c("early","late"),each=2),cost=c(10,0,3,0)),
    periodBreaks=c(0,7,Inf),periodLabels=c("early","late"),currency="EUR",priceYear=2026)
  out <- summariseOutcomes(p,r)
  expect_equal(out$byCycle$cost,c(20,6))
  expect_equal(out$totals$total[out$totals$metric=="cost"],26)
})

test_that("sampled stage accounting agrees with expected outcomes under the same rules", {
  s <- c("A", "B", "D0", "D1")
  onset <- mortality <- diag(4); dimnames(onset) <- dimnames(mortality) <- list(s,s)
  onset["A",] <- c(.8,.2,0,0)
  mortality["A",] <- c(.9,0,.1,0)
  mortality["B",] <- c(0,.5,0,.5)
  fit <- composeTransitionStages(list(onset=onset,mortality=mortality),7,c("D0","D1"))
  effects <- list(prevent=list(stage="onset",from="A",to="B",multiplier=.5,
    redistributeTo="A",endDay=28))
  rewards <- createRewardModel(stateCosts=c(A=10,B=30,D0=0,D1=0),
    upfrontCosts=c(program=15),aliveStates=c("A","B"),utilities=c(A=.8,B=.5,D0=0,D1=0),
    costTiming="onset",currency="EUR",priceYear=2026,costDiscountRate=.035,healthDiscountRate=.035)
  expected <- summariseOutcomes(projectCohort(fit,6,initial=c(A=1),cohortSize=1000,
    transitionEffects=effects),rewards)
  sampled <- summariseOutcomes(simulateTrajectories(createModelBundle(fit),n=1000,
    initial=c(A=1),nSteps=6,terminalStates=c("D0","D1"),transitionEffects=effects,seed=36),rewards)
  for (metric in c("discounted_cost","discounted_life_years","discounted_qalys")) {
    target <- expected$totals$per_patient[expected$totals$metric==metric]
    values <- sampled$byPatient[[metric]]
    expect_lt(abs(mean(values)-target),5*stats::sd(values)/sqrt(length(values)))
  }
  net <- simulateTrajectories(createModelBundle(fit),n=5,initial=c(A=1),nSteps=1,
    terminalStates=c("D0","D1"),seed=3,
    transitionEffects=list(net=list(from="A",to="B",multiplier=.5,redistributeTo="A")))
  eventCost <- createRewardModel(transitionCosts=data.frame(stage="onset",from_state="A",to_state="B",cost=5),
    currency="EUR",priceYear=2026)
  expect_error(summariseOutcomes(net,eventCost), "no retained event")
})


test_that("projection comparisons retain the full initial distribution", {
  states <- c("A", "B")
  matrix <- diag(2); dimnames(matrix) <- list(states, states)
  fit <- composeTransitionStages(list(unchanged = matrix), 7)
  rewards <- createRewardModel(aliveStates = states)
  left <- summariseOutcomes(projectCohort(fit, 1, initial = c(A = 1)), rewards)
  right <- summariseOutcomes(projectCohort(fit, 1, initial = c(B = 1)), rewards)
  expect_error(compareScenarios(left, right), "population")
  expect_error(compareScenarios(left, right, scenarioCovariates = "weight"), "distribution weight")
})


test_that("an explicit horizon can extend verified absorbing terminal snapshots", {
  states <- c("A", "D")
  care <- diag(2); dimnames(care) <- list(states, states)
  death <- care; death["A", ] <- c(0, 1)
  fit <- composeTransitionStages(list(care = care, mortality = death), 7, "D")
  sim <- simulateTrajectories(createModelBundle(fit), n = 3, initial = c(A = 1),
    nSteps = 4, terminalStates = "D", seed = 1)
  rewards <- createRewardModel(stateCosts = c(A = 10, D = 2), upfrontCosts = c(program = 5),
    transitionCosts = data.frame(from_state = "A", to_state = "D", stage = "mortality", cost = 100),
    aliveStates = "A", utilities = c(A = .8, D = 0), costTiming = "care",
    currency = "EUR", priceYear = 2026)
  projected <- summariseOutcomes(projectCohort(fit, 4, initial = c(A = 1), cohortSize = 3), rewards)
  extended <- summariseOutcomes(sim, rewards, nCycles = 4)
  expect_equal(extended$totals, projected$totals)
  expect_equal(extended$metadata$recorded_steps, 1)
  expect_equal(extended$metadata$terminal_extension, 3)
  expect_equal(nrow(sim$trajectories), 6)
  truncated <- simulateTrajectories(createModelBundle(fit), n = 3, initial = c(A = 1), nSteps = 0)
  expect_error(summariseOutcomes(truncated, rewards, nCycles = 4), "all_terminal")
  altered <- sim
  altered$.config$bundle$models$s1$matrices[[1]]["D", ] <- c(.2, .8)
  expect_error(summariseOutcomes(altered, rewards, nCycles = 4), "[Aa]bsorbing|identity")
})

test_that("reward keys keep state and period labels separate", {
  states <- c("A B", "A")
  move <- matrix(c(0, 1, 0, 1), 2, byrow = TRUE,
                 dimnames = list(states, states))
  fit <- composeTransitionStages(list(move = move), 7)
  projection <- projectCohort(fit, 2, initial = c("A B" = 1))
  rewards <- createRewardModel(
    stateCosts = data.frame(state = rep(states, 2),
      period = rep(c("C", "B C"), each = 2), cost = c(10, 20, 30, 40)),
    periodBreaks = c(0, 7, Inf), periodLabels = c("C", "B C"),
    currency = "EUR", priceYear = 2026)
  expect_equal(summariseOutcomes(projection, rewards)$byCycle$cost, c(10, 40))

  states <- c("A B", "C", "A", "B C")
  move <- diag(4); dimnames(move) <- list(states, states)
  move["A B", ] <- c(0, 1, 0, 0)
  move["A", ] <- c(0, 0, 0, 1)
  fit <- composeTransitionStages(list(move = move), 7)
  projection <- projectCohort(fit, 1, initial = c("A B" = .5, "A" = .5), cohortSize = 2)
  rewards <- createRewardModel(transitionCosts = data.frame(
    from_state = c("A B", "A"), to_state = c("C", "B C"), cost = c(10, 40)),
    currency = "EUR", priceYear = 2026)
  expect_equal(summariseOutcomes(projection, rewards)$byCycle$cost, 50)
})
