# All probabilities and patient profiles here are synthetic.
library(TrajectoryMarkovAnalysis)

mortality <- data.frame(
  age_group = c("under65", "65plus", "under65", "65plus"),
  sex = c("female", "female", "male", "male"),
  death_probability = c(0.02, 0.08, 0.04, 0.12)
)
states <- c("Stable", "Complication", "Dead")
age_groups <- list(age_group = list(
  column = "age", breaks = c(0, 65, Inf),
  labels = c("under65", "65plus")
))
source_label <- "Synthetic annual probabilities, not clinical evidence"
background <- createMortalityTransitions(
  mortality, states = states, deathState = "Dead",
  cycleLength = 7, probabilityDays = 365.25,
  strata = c("age_group", "sex"), source = source_label
)
print(background$strata)
print(background$models[[1]]$matrix)
print(background$models[[1]]$metadata$mortality)

# The initial model keeps everyone in their starting state. From day 14,
# background mortality is the only cause of state changes.
unchanged <- diag(length(states))
dimnames(unchanged) <- list(states, states)
original <- createModelBundle(composeTransitionStages(
  list(full_cycle = unchanged), cycleLength = 7, absorbingStates = "Dead"
))
extended <- extendModelBundle(original, fromDay = 14, transitions = background,
  groups = age_groups, source = source_label)

profiles <- data.frame(
  subject_id = paste0("example_", 1:4),
  state_label = c("Stable", "Complication", "Stable", "Complication"),
  sex = c("female", "female", "male", "male"),
  birth_date = as.Date(c("1960-01-20", "1950-01-01", "1960-01-20", "1950-01-01")),
  index_date = as.Date("2025-01-01")
)
run <- simulateTrajectories(extended, population = profiles, nSteps = 6,
  terminalStates = "Dead", seed = 42)
print(run$trajectories[c("source_subject_id", "elapsed_day", "age", "age_group",
                         "sex", "state_label", "model_phase")])
print(run$transitions[c("source_day", "source_stratum", "model_source")])

# Profiles turning 65 on January 20 use the older group at the next source
# cycle boundary, January 22. The cycle that began on January 15 is not split.
# The older group's probability then remains constant at all higher ages in
# this example. A real application needs age coverage suited to its horizon.
checkpoint <- simulateTrajectories(extended, population = profiles, nSteps = 2,
  terminalStates = "Dead", seed = 42)
resumed <- advanceSimulation(unserialize(serialize(checkpoint, NULL)), nSteps = 4)
stopifnot(identical(run$trajectories, resumed$trajectories),
          identical(run$transitions, resumed$transitions))
