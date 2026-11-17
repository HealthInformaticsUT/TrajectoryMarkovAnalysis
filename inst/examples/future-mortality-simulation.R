# Synthetic data illustrate the mechanics, not clinically realistic estimates.
library(TrajectoryMarkovAnalysis)
index <- as.Date("2025-01-01")
states <- c("Stable", "Complication history", "Dead")
destinations <- c(rep("Stable", 7), rep("Complication history", 2), "Dead",
                  rep("Complication history", 9), "Dead")
source_states <- rep(states[1:2], each = 10)
observed <- do.call(rbind, lapply(seq_along(destinations), function(i) {
  data.frame(subject_id = paste0("observed_", i),
    state_label = c(source_states[i], destinations[i]),
    state_start_date = index + c(0, 7), state_end_date = index + c(6, 13),
    birth_date = as.Date("1950-01-01"))
}))
histories <- prepareTrajectories(observed, cycleLength = 7,
  states = states, absorbingStates = "Dead")
fitted <- createModelBundle(estimateTransitions(histories, periodLabels = "Observed"))

groups <- list(age_group = list(column = "age", breaks = c(0, 65, Inf),
  labels = c("under65", "65plus")))
background <- createModelBundle(createMortalityTransitions(
  data.frame(age_group = c("under65", "65plus"), death_probability = c(0.02, 0.08)),
  states = c("Living", "Death"), deathState = "Death", cycleLength = 7,
  probabilityDays = 365.25, strata = "age_group", source = "Synthetic annual mortality"
), groups = groups)
calibration <- calibrateExcessMortality(histories, background, deathStates = "Dead",
  periodLabels = "Observed")

# Ten days is deliberately not divisible by seven. Exact integration leaves
# 4.55 effective excess-hazard days in the first tail cycle, 0.45 in the next,
# and zero thereafter. The decline applies to excess hazard, not total mortality.
future <- extrapolateMortality(fitted, calibration, fromDay = 14,
  referencePeriod = "Observed", excess = "linear", declineDays = 10,
  source = "Synthetic excess declines over ten days after the handover")
profiles <- data.frame(subject_id = paste0("simulated_", 1:100), state_label = "Stable",
  birth_date = as.Date("1960-01-20"), index_date = index)
run <- function(steps) simulateTrajectories(future, population = profiles,
  nSteps = steps, terminalStates = "Dead", seed = 42,
  costRules = list(care = list(type = "recurring", amount = 10)),
  currency = "EUR", priceYear = 2026)
full <- run(6)
audit <- unique(full$transitions[c("source_day", "model_day", "model_phase",
  "background_death_probability", "excess_hazard_per_day", "integrated_excess_hazard",
  "mortality_probability")])
print(audit)

# The birthday changes background matching at the source cycle beginning day
# 21. Ageing and cost clocks do not restart at the day-14 mortality handover.
checkpoint <- unserialize(serialize(run(2), NULL))
resumed <- advanceSimulation(checkpoint, nSteps = 4)
stopifnot(identical(full$trajectories, resumed$trajectories),
          identical(full$transitions, resumed$transitions),
          identical(full$costs, resumed$costs))
late <- subset(audit, model_phase == "extrapolation" & model_day >= 10)
stopifnot(nrow(late) > 0,
          all(late$integrated_excess_hazard == 0),
          max(abs(late$mortality_probability - late$background_death_probability)) < 1e-12)

# The reference rows determine non-death destination shares. No within-cycle
# event ordering or causal effect of avoiding a complication is inferred.
