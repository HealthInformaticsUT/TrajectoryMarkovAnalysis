# Synthetic data illustrate switching to a supplied late-phase model.
library(TrajectoryMarkovAnalysis)

states <- c("Stable", "Complication", "Dead")
index <- as.Date("2025-01-01")
observed <- data.frame(
  subject_id = rep(c("training_1", "training_2"), each = 3),
  state_label = rep(c("Stable", "Complication"), each = 3),
  state_start_date = index + rep(c(0, 7, 14), 2),
  state_end_date = index + rep(c(6, 13, 20), 2)
)
fitted <- estimateTransitions(observed, cycleLength = 7,
  states = states, absorbingStates = "Dead")
original <- createModelBundle(fitted)

# Supply full-cycle probabilities as a single stage in each period.
early <- rbind(Stable = c(0.8, 0.1, 0.1),
               Complication = c(0, 0.8, 0.2), Dead = c(0, 0, 1))
late <- rbind(Stable = c(0.9, 0.05, 0.05),
              Complication = c(0, 0.9, 0.1), Dead = c(0, 0, 1))
colnames(early) <- colnames(late) <- states
supplied <- composeTransitionStages(
  list(full_cycle = list(Early = early, Later = late)),
  cycleLength = 7, absorbingStates = "Dead",
  periodBreaks = c(0, 14, Inf), periodLabels = c("Early", "Later")
)
extended <- extendModelBundle(original, fromDay = 14, transitions = supplied,
  source = "Synthetic external probabilities for this example")

# Original estimates still apply to source days 0 and 7. At source day 14,
# model_day becomes 0 for the external model. Its Later period starts at global
# day 28. The cutoff is supplied by the analyst.
population <- data.frame(
  subject_id = paste0("scenario_", 1:30),
  state_label = rep(c("Stable", "Complication"), c(20, 10))
)
run <- function(steps, treated = FALSE) {
  simulateTrajectories(extended, population = population, nSteps = steps,
    terminalStates = "Dead", seed = 42,
    costRules = list(care = list(type = "recurring", amount = 10)),
    currency = "EUR", priceYear = 2026,
    transitionEffects = if (treated) list(prevention = list(
      from = "Stable", to = "Complication", multiplier = 0.5,
      redistributeTo = "Stable", endDay = 28
    )) else NULL)
}

standard <- run(6)
treated <- run(6, treated = TRUE)
audit_columns <- c("source_day", "model_phase", "model_source", "model_day",
                   "source_period")
print(unique(standard$transitions[audit_columns]))

# The intervention window does not restart at the handover. It ends at global
# source day 28, regardless of the external model's relative period clock.
print(unique(treated$effects[c("source_day", "model_day", "model_phase")]))
summaries <- lapply(list(standard = standard, treated = treated), function(x) {
  summariseSimulation(x, aliveStates = c("Stable", "Complication"),
    utilities = c(Stable = 0.8, Complication = 0.6, Dead = 0))
})
comparison <- compareScenarios(summaries$standard, summaries$treated)
# A shared seed gives reproducibility but does not pair patient draws.
print(comparison$outcomes)
print(comparison$metadata$extrapolation)

# A checkpoint at the cutoff resumes with the same models and random stream.
checkpoint <- unserialize(serialize(run(2), NULL))
resumed <- advanceSimulation(checkpoint, nSteps = 4)
stopifnot(
  identical(original$models, extended$models),
  identical(standard$trajectories, resumed$trajectories),
  identical(standard$transitions, resumed$transitions),
  identical(standard$costs, resumed$costs),
  all(treated$effects$source_day < 28),
  all(standard$transitions$model_phase[
    standard$transitions$source_day < 14] == "base"),
  all(standard$transitions$model_day[
    standard$transitions$source_day == 28] == 14)
)

# For mortality extrapolation, see future-mortality-simulation.R.
