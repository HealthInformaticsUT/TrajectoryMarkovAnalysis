test_that("mortality probabilities use the declared time interval and preserve living states", {
  states <- c("Alive", "Complication history", "Dead")
  model <- createMortalityTransitions(data.frame(death_probability = 0.2),
    states = states, deathState = "Dead", cycleLength = 7,
    probabilityDays = 365, source = "Synthetic annual mortality")
  probabilities <- model$matrices[[1]]
  weekly <- -expm1(log1p(-0.2) * 7 / 365)
  expect_equal(probabilities["Alive", "Dead"], weekly)
  expect_equal(1 - (1 - probabilities["Alive", "Dead"])^(365 / 7), 0.2)
  expect_equal(probabilities["Complication history", ],
    c(Alive = 0, "Complication history" = 1 - weekly, Dead = weekly))
  expect_equal(probabilities["Dead", ], c(Alive = 0, "Complication history" = 0, Dead = 1))
  expect_equal(unname(rowSums(probabilities)), rep(1, 3))
  expect_equal(model$metadata$mortality$source, "Synthetic annual mortality")
  expect_equal(model$metadata$mortality$probabilityDays, 365)
  expect_equal(model$metadata$mortality$death_probability, 0.2)
  expect_equal(model$metadata$mortality$cycle_probability, weekly)

  for (probability in c(0, 1, 1e-14)) {
    boundary <- createMortalityTransitions(data.frame(death_probability = probability),
      states = states, deathState = "Dead", cycleLength = 7,
      probabilityDays = 365, source = "Synthetic boundary probabilities")
    expected <- -expm1(log1p(-probability) * 7 / 365)
    expect_equal(boundary$matrices[[1]]["Alive", "Dead"], expected, tolerance = 1e-20)
  }
})

test_that("age and sex mortality groups work across birthdays and saved extrapolation", {
  states <- c("Alive", "Complication history", "Dead")
  mortality <- data.frame(age_group = c("under65", "65plus", "under65", "65plus"),
    sex = c("female", "female", "male", "male"), death_probability = c(0, 1, 0, 0))
  model <- createMortalityTransitions(mortality, states = states, deathState = "Dead",
    cycleLength = 7, probabilityDays = 365, strata = c("age_group", "sex"),
    source = "Synthetic age and sex mortality")
  groups <- list(age_group = list(column = "age", breaks = c(0, 65, Inf),
    labels = c("under65", "65plus")))
  population <- data.frame(subject_id = c("woman", "man"), state_label = states[1:2],
    sex = c("female", "male"), birth_date = as.Date("1960-01-08"),
    index_date = as.Date("2025-01-01"))
  bundle <- createModelBundle(model, groups = groups)
  result <- simulateTrajectories(bundle, population, nSteps = 2,
    terminalStates = "Dead", seed = 21)
  expect_equal(result$population$state_label, c("Dead", "Complication history"))
  woman <- subset(result$trajectories, source_subject_id == "woman")
  expect_equal(woman$state_label, c("Alive", "Alive", "Dead"))
  expect_equal(woman$age[1:2], c(64, 65))
  registry <- model$strata
  femaleIds <- registry$stratum_id[match(c("under65", "65plus"),
    ifelse(registry$sex == "female", registry$age_group, NA_character_))]
  expect_equal(subset(result$transitions, simulation_id == "sim_1")$source_stratum,
    femaleIds)

  base <- createModelBundle(createMortalityTransitions(data.frame(death_probability = 0),
    states = states, deathState = "Dead", cycleLength = 7,
    probabilityDays = 365, source = "Synthetic zero mortality"))
  extended <- extendModelBundle(base, fromDay = 7, transitions = model, groups = groups,
    source = "Synthetic mortality handover")
  full <- simulateTrajectories(extended, population, nSteps = 2,
    terminalStates = "Dead", seed = 21)
  partial <- simulateTrajectories(extended, population, nSteps = 1,
    terminalStates = "Dead", seed = 21)
  resumed <- advanceSimulation(unserialize(serialize(partial, NULL)), 1)
  expect_identical(resumed$trajectories, full$trajectories)
  expect_identical(resumed$transitions, full$transitions)
  expect_equal(full$population$state_label, c("Dead", "Complication history"))
  expect_equal(subset(full$transitions, simulation_id == "sim_1")$model_phase,
    c("base", "extrapolation"))
  unsupported <- population[1, ]
  unsupported$sex <- "not covered"
  expect_error(simulateTrajectories(bundle, unsupported, nSteps = 1),
    "supports|covariate|strat")
})

test_that("mortality inputs reject ambiguous rows and invalid probability definitions", {
  make <- function(mortality = data.frame(death_probability = 0.2),
                   states = c("Alive", "Dead"), deathState = "Dead",
                   cycleLength = 7, probabilityDays = 365, strata = NULL,
                   source = "Synthetic mortality") {
    createMortalityTransitions(mortality, states = states, deathState = deathState,
      cycleLength = cycleLength, probabilityDays = probabilityDays,
      strata = strata, source = source)
  }
  expect_error(make(data.frame(death_probability = c(0.1, 0.2))), "row|strata")
  expect_error(make(data.frame(sex = c("female", "female"),
    death_probability = c(0.1, 0.2)), strata = "sex"), "duplicat|combination|one")
  expect_error(make(data.frame(sex = NA_character_, death_probability = 0.1),
    strata = "sex"), "missing|NA|strat")
  expect_error(make(strata = "sex"), "sex|column|strat")
  for (probability in c(-0.1, 1.1, Inf, NA_real_)) {
    expect_error(make(data.frame(death_probability = probability)), "probability|finite|missing")
  }
  for (duration in c(0, -1, Inf, NA_real_)) {
    expect_error(make(probabilityDays = duration), "probabilityDays|positive|finite")
    expect_error(make(cycleLength = duration), "cycleLength|positive|finite")
  }
  expect_error(make(deathState = "Unknown"), "deathState|state")
  expect_error(make(states = c("Alive", "Alive", "Dead")), "states|unique|duplicat")
  expect_error(make(source = " "), "source")
  expect_error(createMortalityTransitions(data.frame(death_probability = 0.2),
    states = c("Alive", "Dead"), deathState = "Dead", cycleLength = 7,
    source = "Synthetic mortality"), "probabilityDays|missing")
  expect_error(createMortalityTransitions(data.frame(death_probability = 0.2),
    states = c("Alive", "Dead"), deathState = "Dead", probabilityDays = 365,
    source = "Synthetic mortality"), "cycleLength|missing")
  expect_error(createMortalityTransitions(data.frame(death_probability = 0.2),
    states = c("Alive", "Dead"), deathState = "Dead", cycleLength = 7,
    probabilityDays = 365), "source|missing")
})
