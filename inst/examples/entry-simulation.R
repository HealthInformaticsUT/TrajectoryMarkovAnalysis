# Synthetic histories illustrate the entry interface, not clinical estimates.
library(TrajectoryMarkovAnalysis)

index <- as.Date("2025-01-01")
observed <- data.frame(
  subject_id = c("observed_1", "observed_1", "observed_2", "observed_2",
                 "observed_3", "observed_4", "observed_4"),
  state_label = c("Stable", "Stable", "Complication", "Complication",
                  "Dead", "Stable", "Dead"),
  state_start_date = index + c(0, 7, 0, 7, 0, 0, 7),
  state_end_date = index + c(6, 13, 6, 13, 2, 6, 9)
)
histories <- prepareTrajectories(
  observed, cycleLength = 7,
  states = c("Stable", "Complication", "Dead"), absorbingStates = "Dead"
)
entry <- estimateEntryTransitions(histories, entryState = "Stable")
bundle <- createModelBundle(estimateTransitions(histories))
baseline_patients <- data.frame(
  subject_id = paste0("simulated_source_", seq_len(20)), state_label = "Stable"
)

simulation <- simulateTrajectories(
  bundle, population = baseline_patients, entry = entry, nSteps = 2,
  terminalStates = "Dead", seed = 42,
  costRules = list(
    care = list(type = "recurring", amount = 10),
    terminal_episode = list(type = "transition", from = "Stable",
                           to = "Dead", amount = 100)
  ),
  currency = "EUR", priceYear = 2026
)

print(table(simulation$entry$transitions$destination_state))
print(simulation$totals)
summary <- summariseSimulation(
  simulation, aliveStates = c("Stable", "Complication"),
  utilities = c(Stable = 0.8, Complication = 0.6, Dead = 0)
)
print(summary$totals)
stopifnot(
  nrow(simulation$trajectories) == 20 * 3,
  all(summary$initialPopulation$state_label == "Stable"),
  summary$metadata$initial_population_basis == "pre_entry"
)

# Three full labelled weeks, not a baseline week plus three outcome weeks.
# Terminal-entry fees can occur at day zero. No recurring charge follows death.
# A pre-entry treatment fee and effects on the entry matrix are not supplied.
