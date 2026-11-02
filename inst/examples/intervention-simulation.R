# Illustrative probabilities and prices, not estimates of a clinical effect.
library(TrajectoryMarkovAnalysis)

states <- c("Stable", "Complication", "Dead")
probabilities <- matrix(
  c(0.70, 0.20, 0.10,
    0.00, 0.80, 0.20,
    0.00, 0.00, 1.00),
  nrow = 3, byrow = TRUE, dimnames = list(states, states)
)
model <- composeTransitionStages(
  list(full_cycle = probabilities), cycleLength = 7, absorbingStates = "Dead"
)
bundle <- createModelBundle(model)
patients <- data.frame(
  subject_id = paste0("patient_", seq_len(1000)),
  state_label = "Stable", treatment_active = FALSE
)

effects <- list(prevention = list(
  from = "Stable", to = "Complication", multiplier = 0.75,
  redistributeTo = "Stable", startDay = 0, endDay = 28,
  eligible = function(p) p$treatment_active
))
care <- list(
  stable_care = list(type = "recurring", states = "Stable", amount = 10),
  complication_care = list(type = "recurring", states = "Complication", amount = 25)
)

standard <- simulateTrajectories(
  bundle, population = patients, nSteps = 12,
  terminalStates = "Dead", costRules = care,
  currency = "EUR", priceYear = 2026, seed = 42
)

treated_patients <- patients
treated_patients$treatment_active <- TRUE
intervention_costs <- c(care, list(treatment = list(
  type = "once", amount = 100,
  eligible = function(p) p$treatment_active
)))
treated <- simulateTrajectories(
  bundle, population = treated_patients, nSteps = 12,
  terminalStates = "Dead", transitionEffects = effects,
  costRules = intervention_costs,
  currency = "EUR", priceYear = 2026, seed = 42
)

summary_row <- function(run, scenario) {
  data.frame(
    scenario = scenario,
    alive_at_end = sum(run$population$state_label != "Dead"),
    complication_onsets = sum(run$transitions$source_state == "Stable" &
                              run$transitions$destination_state == "Complication"),
    total_cost = sum(run$totals$amount)
  )
}
print(rbind(summary_row(standard, "Standard care"),
            summary_row(treated, "Intervention")))
print(head(treated$effects))
stopifnot(identical(bundle$models$s1$matrices[[1]], probabilities))

# These utilities and discount rates illustrate the API, not clinical evidence.
summaries <- lapply(list(standard_care = standard, intervention = treated),
  function(run) summariseSimulation(
    run, aliveStates = c("Stable", "Complication"),
    utilities = c(Stable = 0.8, Complication = 0.6, Dead = 0),
    costDiscountRate = 0.035, healthDiscountRate = 0.035
  ))
print(summaries$intervention$totals)
print(summaries$intervention$costs)

comparison <- compareScenarios(
  summaries$standard_care, summaries$intervention,
  scenarioCovariates = "treatment_active"
)
print(comparison$outcomes)
print(comparison$costEffectiveness)

# Effects apply to net cycle probabilities, not the component stages.
# The same seed gives reproducible runs, not guaranteed paired patient draws.
# Costs include the initial label and 12 subsequent labels. The treatment
# effect applies only to steps starting on days 0, 7, 14 and 21.
# Health outcomes also use full labels, with no half-cycle correction. A patient
# alive throughout this run contributes 13 * 7 days, not 12 * 7 days.
