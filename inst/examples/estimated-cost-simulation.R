# Small synthetic example. No database, files or browser are needed.
library(TrajectoryMarkovAnalysis)

index_date <- as.Date("2025-01-01")
profiles <- data.frame(
  subject_id = c("one", "two", "three"),
  birth_date = as.Date(c("1960-01-08", "1964-01-01", "1955-01-01")),
  index_date = index_date,
  state_label = "Alive"
)

# The first patient turns 65 between the first and second weekly labels.
rows <- profiles[rep(seq_len(nrow(profiles)), each = 3), ]
rows$state_start_date <- index_date + rep(c(0, 7, 14), 3)
rows$state_end_date <- rows$state_start_date + 6
rows$state_label <- c("Alive", "Alive", "Dead",
                      "Alive", "Alive", "Alive",
                      "Alive", "Alive", "Dead")
rows$state_end_date[rows$state_label == "Dead"] <-
  rows$state_start_date[rows$state_label == "Dead"] + 2

groups <- list(age_group = list(
  column = "age", breaks = c(0, 65, Inf),
  labels = c("Under 65", "65 and older")
))
rows <- addCovariateGroups(rows, groups, dateColumn = "state_start_date")
histories <- prepareTrajectories(rows, cycleLength = 7, absorbingStates = "Dead")

# Invented daily amounts make the cost calculation easy to inspect.
# The cost period changes at day 10, partway through a weekly label.
events <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
  days <- seq(rows$state_start_date[i], rows$state_end_date[i], by = "day")
  daily <- if (rows$age_group[i] == "Under 65") 10 else 20
  data.frame(subject_id = rows$subject_id[i], event_date = days,
             amount = daily + ifelse(days >= index_date + 10, 5, 0),
             currency_concept_id = "EUR")
}))

transitions <- estimateTransitions(histories, strata = "age_group")
observed_costs <- estimateStateCosts(
  histories, events, strata = "age_group",
  periodBreaks = c(0, 10, Inf), periodLabels = c("Early", "Later"),
  terminalCostMap = c(Dead = "Alive")
)
healthcare <- createCostModel(
  observed_costs, unit = "day", currency = "EUR", priceYear = 2026,
  terminal = "entry"
)
bundle <- createModelBundle(transitions, costs = healthcare, groups = groups)

# The startup fee is separate from ordinary healthcare. It does not alter risk.
intervention <- list(startup = list(type = "once", amount = 100))
run <- simulateTrajectories(
  bundle, population = profiles, n = 12, replace = TRUE,
  nSteps = 6, terminalStates = "Dead", costRules = intervention, seed = 42
)

# The same run can be paused without repeating startup or healthcare charges.
partial <- simulateTrajectories(
  bundle, population = profiles, n = 12, replace = TRUE,
  nSteps = 2, terminalStates = "Dead", costRules = intervention, seed = 42
)
resumed <- advanceSimulation(partial, nSteps = 4)
stopifnot(identical(run$trajectories, resumed$trajectories),
          identical(run$costs, resumed$costs),
          identical(run$totals, resumed$totals))

print(run$totals)
print(head(run$costs[run$costs$component == "healthcare", ]))

# Terminal entries receive one full labelled-cycle charge under this assumption.
# This does not reproduce the three observed days in a short terminal interval.
