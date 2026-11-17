# Synthetic histories and probabilities illustrate the calculation only.
library(TrajectoryMarkovAnalysis)
index <- as.Date("2025-01-01")
rows <- function(id, labels, birth) data.frame(
  subject_id = id, state_label = labels,
  state_start_date = index + 7 * (seq_along(labels) - 1),
  state_end_date = index + 7 * seq_along(labels) - 1,
  birth_date = as.Date(birth)
)
observed <- rbind(
  rows("one", c("Alive", "Alive", "Dead"), "1960-01-08"),
  rows("two", c("Alive", "Dead"), "1970-01-01"),
  rows("three", c("Alive", "Alive", "Alive"), "1975-01-01"),
  rows("four", c("Alive", "Dead"), "1945-01-01")
)
histories <- prepareTrajectories(observed, cycleLength = 7,
  absorbingStates = "Dead", states = c("Alive", "Dead"))

age_groups <- list(age_group = list(column = "age", breaks = c(0, 65, Inf),
  labels = c("under65", "65plus")))
background <- createModelBundle(createMortalityTransitions(
  data.frame(age_group = c("under65", "65plus"), death_probability = c(0.02, 0.08)),
  states = c("Living", "Death"), deathState = "Death",
  cycleLength = 7, probabilityDays = 365.25, strata = "age_group",
  source = "Synthetic annual background probabilities"
), groups = age_groups)

calibration <- calibrateExcessMortality(histories, background,
  deathStates = "Dead", periodBreaks = c(0, 7, Inf),
  periodLabels = c("First cycle", "Later cycles"))
print(calibration$estimates)
print(calibration$reference)
print(calibration$background$strata)

# The first period uses four source cycles. The later period uses only two.
# Patient one turns 65 before contributing the later source cycle. The
# background probabilities follow this changing reference population.
fitted <- estimateTransitions(histories, periodBreaks = c(0, 7, Inf),
  periodLabels = c("First cycle", "Later cycles"))
death_rows <- subset(fitted$estimates, from_state == "Alive" & to_state == "Dead")
ordered <- calibration$estimates[match(death_rows$period,
                                     calibration$estimates$period), ]
stopifnot(
  identical(as.numeric(ordered$cycles), as.numeric(death_rows$denominator)),
  identical(as.numeric(ordered$deaths), as.numeric(death_rows$count)),
  max(abs(ordered$reconstructed_probability - ordered$observed_probability)) < 1e-12
)

# No extrapolation is inferred here. Selecting a reference period and deciding
# how excess mortality should behave later are separate modelling decisions.
