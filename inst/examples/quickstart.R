# Synthetic input. No database or patient records are needed.
library(TrajectoryMarkovAnalysis)

paths <- list(
  p1 = c("Stable", "Complication", "Dead"),
  p2 = c("Stable", "Stable", "Complication", "Dead"),
  p3 = c("Stable", "Complication", "Complication", "Dead"),
  p4 = c("Stable", "Stable", "Stable", "Stable")
)
trajectories <- do.call(rbind, lapply(names(paths), function(id) {
  dates <- as.Date("2026-01-01") + 7 * (seq_along(paths[[id]]) - 1)
  data.frame(subject_id = id, state_label = paths[[id]],
             state_start_date = dates, state_end_date = dates + 6)
}))
rownames(trajectories) <- NULL

histories <- prepareTrajectories(
  trajectories, cycleLength = 7, absorbingStates = "Dead"
)
transitions <- estimateTransitions(histories)
projection <- projectCohort(
  transitions, nCycles = 4, initial = c(Stable = 1), cohortSize = 100
)
print(transitions$estimates)
print(projection$occupancy)

# Cost estimation is independent of transition estimation.
# These amounts illustrate the interface, not clinical expenditure.
cost_records <- data.frame(
  subject_id = trajectories$subject_id,
  event_date = trajectories$state_start_date,
  amount = ifelse(trajectories$state_label == "Complication", 200, 100),
  currency_concept_id = "44818568", measure = "total_charge"
)
costs <- estimateStateCosts(
  histories, cost_records, terminalCostMap = c(Dead = "Stable")
)
print(costs$estimates)

# For a connected study, replace cost_records with:
# cost_records <- extractCosts(cdm, histories, measure = "total_charge",
#                              domains = "Visit Detail", allocation = "duration")
# The caller creates and closes cdm's connection.
