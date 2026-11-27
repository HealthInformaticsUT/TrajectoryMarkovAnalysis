# Synthetic histories, effects and prices illustrate entry modelling.
library(TrajectoryMarkovAnalysis)

index <- as.Date("2025-01-01")
observed <- data.frame(
  subject_id = c("observed_1", "observed_1", "observed_2", "observed_2",
                 "observed_3", "observed_4", "observed_4",
                 "observed_5", "observed_5", "observed_6", "observed_6"),
  state_label = c("Stable", "Stable", "Complication", "Complication",
                  "Dead", "Stable", "Dead", "Stable", "Complication",
                  "Complication", "Dead"),
  state_start_date = index + c(0, 7, 0, 7, 0, 0, 7, 0, 7, 0, 7),
  state_end_date = index + c(6, 13, 6, 13, 2, 6, 9, 6, 13, 6, 9)
)
histories <- prepareTrajectories(
  observed, cycleLength = 7,
  states = c("Stable", "Complication", "Dead"), absorbingStates = "Dead"
)
entry <- estimateEntryTransitions(histories, entryState = "Stable")
bundle <- createModelBundle(estimateTransitions(histories))
baseline_patients <- data.frame(
  subject_id = paste0("simulated_source_", seq_len(1000)), state_label = "Stable",
  treatment_eligible = seq_len(1000) <= 750
)

care <- list(
  care = list(type = "recurring", amount = 10),
  terminal_episode = list(type = "transition", from = "Stable",
                         to = "Dead", amount = 100)
)
eligible <- function(p) p$treatment_eligible
effects <- list(
  prevention_entry = list(
    from = "Stable", to = "Complication", multiplier = 0.75,
    redistributeTo = "Stable", applyAt = "entry", eligible = eligible
  ),
  prevention_later = list(
    from = "Stable", to = "Complication", multiplier = 0.75,
    redistributeTo = "Stable", applyAt = "transitions", endDay = 21,
    eligible = eligible
  )
)
intervention_costs <- c(care, list(treatment = list(
  type = "entry", amount = 100, eligible = eligible
)))

# Reuse the same profiles and eligibility in both scenarios.
standard <- simulateTrajectories(
  bundle, population = baseline_patients, entry = entry, nSteps = 3,
  terminalStates = "Dead", seed = 42, costRules = care,
  currency = "EUR", priceYear = 2026
)
treated <- simulateTrajectories(
  bundle, population = baseline_patients, entry = entry, nSteps = 3,
  terminalStates = "Dead", seed = 42,
  transitionEffects = effects, costRules = intervention_costs,
  currency = "EUR", priceYear = 2026
)

print(table(treated$entry$transitions$destination_state))
print(treated$totals)
summaries <- lapply(list(standard_care = standard, intervention = treated),
  function(run) summariseSimulation(
    run, aliveStates = c("Stable", "Complication"),
    utilities = c(Stable = 0.8, Complication = 0.6, Dead = 0)
  ))
comparison <- compareScenarios(summaries$standard_care, summaries$intervention)
print(comparison$outcomes)
print(comparison$costEffectiveness)

fees <- treated$costs[treated$costs$component == "treatment", ]
baseline <- treated$entry$population
early_deaths <- treated$entry$transitions$simulation_id[
  treated$entry$transitions$destination_state == "Dead"
]
eligible_ids <- baseline$simulation_id[baseline$treatment_eligible]
stopifnot(
  nrow(treated$trajectories) == nrow(baseline_patients) * 4,
  all(summaries$intervention$initialPopulation$state_label == "Stable"),
  summaries$intervention$metadata$initial_population_basis == "pre_entry",
  setequal(fees$simulation_id, eligible_ids),
  length(intersect(early_deaths, eligible_ids)) > 0,
  all(intersect(early_deaths, eligible_ids) %in% fees$simulation_id),
  sum(fees$amount) == sum(baseline_patients$treatment_eligible) * 100
)

# Costs and health outcomes cover four labelled weeks, including the first label.
# Eligible patients pay the entry fee even if they die during entry.
# Entry and the first adjacent transition both use source day zero. The two
# rules cover entry plus three later draws starting at days 0, 7 and 14.
# To target the same four draws with one rule, use applyAt = "both", endDay = 21.
# Entry probabilities are pooled within starting state.
# A shared seed gives reproducibility but does not pair patient draws.
