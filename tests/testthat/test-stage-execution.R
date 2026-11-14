stage_execution_fixture <- function() {
  states <- c("N", "P", "DN", "DP")
  onset <- mortality <- diag(4)
  dimnames(onset) <- dimnames(mortality) <- list(states, states)
  onset["N", ] <- c(.8, .2, 0, 0)
  mortality["N", ] <- c(.9, 0, .1, 0)
  mortality["P", ] <- c(0, .5, 0, .5)
  composeTransitionStages(list(onset = onset, mortality = mortality), 7,
    absorbingStates = c("DN", "DP"))
}

test_that("stage prevention changes downstream outcomes before projection", {
  model <- stage_execution_fixture()
  effect <- list(prevention = list(stage = "onset", from = "N", to = "P",
    redistributeTo = "N", multiplier = .5, endDay = 7))
  projection <- projectCohort(model, 2, initial = c(N = 1), transitionEffects = effect)
  expect_equal(subset(projection$occupancy, cycle == 1)$probability, c(.81, .05, .09, .05))
  expect_equal(subset(projection$stageFlows, source_cycle == 0 & stage == "mortality" &
    from_state == "P" & to_state == "DP")$probability_mass, .05)
  expect_equal(projection$cycleMatrices[[2]], model$matrix)
  expect_equal(model$stageMatrices$onset[[1]]["N", "P"], .2)
  two_effects <- c(effect, list(survival = list(stage = "mortality", from = "P",
    to = "DP", redistributeTo = "P", multiplier = .5)))
  combined <- projectCohort(model, 1, initial = c(N = 1), transitionEffects = two_effects)
  expect_equal(subset(combined$occupancy, cycle == 1)$probability, c(.81, .075, .09, .025))
  expect_error(projectCohort(model, 1, initial = c(N = 1),
    transitionEffects = c(effect, list(duplicate = effect$prevention))), "Overlapping")
  expect_error(projectCohort(model, 1, initial = c(N = 1), transitionEffects = list(
    prevention = c(effect$prevention, list(eligible = function(p) TRUE)))), "profiles")
  expect_error(projectCohort(model, 1, initial = c(N = 1), transitionEffects = list(
    prevention = effect$prevention, net = list(from = "N", to = "P",
      multiplier = .5, redistributeTo = "N"))), "mix")
})

test_that("sampled stages retain within-cycle events and resume exactly", {
  model <- stage_execution_fixture()
  effect <- list(prevention = list(stage = "onset", from = "N", to = "P",
    redistributeTo = "N", multiplier = .5, endDay = 7))
  bundle <- createModelBundle(model)
  full <- simulateTrajectories(bundle, n = 1000, initial = c(N = 1), nSteps = 2,
    terminalStates = c("DN", "DP"), transitionEffects = effect, seed = 114)
  first <- simulateTrajectories(bundle, n = 1000, initial = c(N = 1), nSteps = 1,
    terminalStates = c("DN", "DP"), transitionEffects = effect, seed = 114)
  resumed <- advanceSimulation(unserialize(serialize(first, NULL)), 1)
  expect_identical(full$stageTransitions, resumed$stageTransitions)
  expect_identical(full$transitions, resumed$transitions)
  expect_identical(full$effects, resumed$effects)
  expect_identical(full$.state$rng, resumed$.state$rng)
  first_deaths <- subset(full$stageTransitions, source_cycle == 0 & stage == "mortality" &
    from_state == "P" & to_state == "DP")$simulation_id
  expect_gt(length(first_deaths), 0)
  onsets <- subset(full$stageTransitions, source_cycle == 0 & stage == "onset" &
    from_state == "N" & to_state == "P")$simulation_id
  expect_true(all(first_deaths %in% onsets))
  first_counts <- table(factor(first$population$state_label, levels = model$metadata$states)) / 1000
  expect_equal(as.numeric(first_counts), c(.81, .05, .09, .05), tolerance = .04)
  expect_true(all(full$effects$stage == "onset"))
  expect_equal(unique(full$stageTransitions$destination_day - full$stageTransitions$source_day), 7)
})
