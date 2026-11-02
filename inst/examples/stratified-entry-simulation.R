# All patients, outcomes, effect sizes, utilities and prices here are synthetic.
# The tiny groups make model selection visible, not clinically reliable.
library(TrajectoryMarkovAnalysis)

index <- as.Date("2025-01-01")
states <- c("Stable", "Complication", "Dead")
groups <- list(
  age_group = list(column = "age", breaks = c(0, 65, Inf),
                   labels = c("under65", "65plus")),
  pgs_group = list(column = "pgs", breaks = c(-Inf, 0, Inf),
                   labels = c("lower", "higher"))
)

# Baseline records describe what is known BEFORE the first-cycle outcomes.
# PGS (polygenic score) is an invented covariate here, not calculated by TMA.
baseline <- data.frame(
  subject_id = paste0("entry_training_", 1:4), entry_state = "Stable",
  birth_date = as.Date(c("1960-01-05", "1975-01-01", "1950-01-01", "1945-01-01")),
  index_date = index, pgs = c(-1, 1, -1, 1)
)
baseline <- addCovariateGroups(baseline, groups, dateColumn = "index_date")
entry_outcomes <- data.frame(
  subject_id = baseline$subject_id,
  state_label = c("Stable", "Complication", "Complication", "Dead"),
  state_start_date = index, state_end_date = index + 6
)
entry <- estimateEntryTransitions(
  entry_outcomes, entryState = baseline, strata = c("age_group", "pgs_group"),
  cycleLength = 7, states = states, absorbingStates = "Dead"
)
print(entry$diagnostics$by_stratum)

# A second small set supplies observed adjacent-cycle pairs for BOTH living
# states in each age group. Later transitions use age, not PGS in this example.
followup <- data.frame(
  subject_id = rep(paste0("followup_training_", 1:4), each = 2),
  state_label = c("Stable", "Stable", "Complication", "Complication",
                  "Stable", "Complication", "Complication", "Complication"),
  state_start_date = index + rep(c(0, 7), 4),
  state_end_date = index + rep(c(6, 13), 4),
  age = rep(c(50, 50, 75, 75), each = 2)
)
followup <- addCovariateGroups(followup, groups["age_group"])
later <- estimateTransitions(
  followup, cycleLength = 7, states = states, absorbingStates = "Dead",
  strata = "age_group"
)
bundle <- createModelBundle(later, groups = groups["age_group"])

# Supply the simulation profiles explicitly. The entry estimator does not
# automatically reuse its training patients. PGS groups are already supplied.
# The bundle recomputes age and its group at each weekly boundary.
population <- baseline
population$subject_id <- paste0("scenario_patient_", 1:4)
names(population)[names(population) == "entry_state"] <- "state_label"

care <- list(care = list(type = "recurring", amount = 10))
prevention <- list(
  prevention = list(from = "Stable", to = "Complication", multiplier = 0,
                    redistributeTo = "Stable", applyAt = "entry")
)
fees <- c(care, list(treatment = list(type = "entry", amount = 100)))

# The hypothetical treatment prevents first-cycle complication only. A zero
# multiplier is deliberately extreme so the example has deterministic results.
# Everyone pays the upfront fee, including patients who die during entry.
standard <- simulateTrajectories(
  bundle, population = population, entry = entry, nSteps = 2,
  terminalStates = "Dead", costRules = care,
  currency = "EUR", priceYear = 2026, seed = 42
)
treated <- simulateTrajectories(
  bundle, population = population, entry = entry, nSteps = 2,
  terminalStates = "Dead", transitionEffects = prevention, costRules = fees,
  currency = "EUR", priceYear = 2026, seed = 42
)
print(treated$entry$transitions)
print(treated$entry$effects)

# Patient 1 turns 65 before the second snapshot. Entry is not repeated.
# The next adjacent transition uses the 65plus model at its source boundary.
first_patient <- subset(treated$trajectories, simulation_id == "sim_1")
print(first_patient[c("cycle", "elapsed_day", "age", "age_group", "state_label")])

summaries <- lapply(list(standard = standard, treated = treated), function(run) {
  summariseSimulation(run, aliveStates = c("Stable", "Complication"),
    utilities = c(Stable = 0.8, Complication = 0.6, Dead = 0))
})
comparison <- compareScenarios(summaries$standard, summaries$treated)
print(comparison$outcomes)

upfront <- subset(treated$costs, component == "treatment")
stopifnot(
  nrow(treated$entry$transitions) == nrow(population),
  all(first_patient$age == c(64, 65, 65)),
  identical(first_patient$state_label, c("Stable", "Stable", "Complication")),
  nrow(upfront) == nrow(population),
  all(upfront$elapsed_day == 0),
  summaries$treated$metadata$initial_population_basis == "pre_entry"
)

# There are three labelled weeks, not three weeks plus an entry week.
# Entry age/PGS combinations are selected once. Later age groups can change.
# An unknown entry group or unsupported starting-state row is an error, not a
# request to substitute pooled probabilities. Entry-specific group rules must
# be applied to profiles explicitly if the later bundle does not use them.
