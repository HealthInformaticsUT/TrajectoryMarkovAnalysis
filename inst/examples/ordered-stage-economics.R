# Synthetic observations. The analyst has already specified the events,
# observation support and their order within each weekly interval.
library(TrajectoryMarkovAnalysis)

states <- c("Stable", "Complication", "DeathWithout", "DeathAfter")
after_onset <- ifelse(1:100 <= 20, "Complication", "Stable")
after_mortality <- after_onset
after_mortality[1:10] <- "DeathAfter"
after_mortality[21:28] <- "DeathWithout"
observations <- rbind(
  data.frame(subject_id = as.character(1:100), source_day = 0,
    stage = "onset", from_state = "Stable", to_state = after_onset, observed = TRUE),
  data.frame(subject_id = as.character(1:100), source_day = 0,
    stage = "mortality", from_state = after_onset, to_state = after_mortality, observed = TRUE)
)
fit <- estimateStageTransitions(observations,
  stageOrder = c("onset", "mortality"), cycleLength = 7, states = states,
  absorbingStates = c("DeathWithout", "DeathAfter"),
  identityStates = list(onset = "Complication"))

# Reduce onset during four intervals. Later mortality follows the state reached.
effect <- list(prevention = list(stage = "onset", from = "Stable",
  to = "Complication", multiplier = .75, redistributeTo = "Stable", endDay = 28))
reference_rewards <- createRewardModel(
  stateCosts = c(Stable = 10, Complication = 30, DeathWithout = 0, DeathAfter = 0),
  upfrontCosts = c(program = 0),
  aliveStates = c("Stable", "Complication"),
  utilities = c(Stable = .8, Complication = .6, DeathWithout = 0, DeathAfter = 0),
  costTiming = "onset", healthTiming = "start", discountTiming = "midpoint",
  costDiscountRate = .035, healthDiscountRate = .035, currency = "EUR", priceYear = 2026)

intervention_rewards <- updateRewardModel(reference_rewards, upfrontCosts = c(program = 15))

reference <- projectCohort(fit, nCycles = 52, initial = c(Stable = 1), cohortSize = 1000)
intervention <- projectCohort(fit, nCycles = 52, initial = c(Stable = 1),
  cohortSize = 1000, transitionEffects = effect)
reference_outcomes <- summariseOutcomes(reference, reference_rewards)
intervention_outcomes <- summariseOutcomes(intervention, intervention_rewards)
comparison <- compareScenarios(reference_outcomes, intervention_outcomes)
print(comparison)

# The same effect and accounting definitions can be applied to sampled paths.
# Fifty-two steps are fifty-two accounted intervals, totalling 364 days.
sampled <- simulateTrajectories(createModelBundle(fit), n = 1000,
  initial = c(Stable = 1), nSteps = 52, terminalStates = c("DeathWithout", "DeathAfter"),
  transitionEffects = effect, seed = 192)
sampled_outcomes <- summariseOutcomes(sampled, intervention_rewards, nCycles = 52)
print(outcomeTotals(sampled_outcomes))
# The sample varies around the expected results. It is not a parameter-uncertainty analysis.
