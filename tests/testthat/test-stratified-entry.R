stratified_entry_inputs <- function(includeExcluded = FALSE) {
  ids <- paste0("entry-training-", 1:4)
  baseline <- data.frame(subject_id = ids, entry_state = "A",
    age_group = c("young", "young", "older", "older"),
    score_group = c("low", "high", "low", "high"))
  history <- data.frame(subject_id = ids, state_label = c("A", "B", "B", "D"),
    state_start_date = as.Date("2025-01-01"), state_end_date = as.Date("2025-01-07"),
    age_group = rev(baseline$age_group), score_group = rev(baseline$score_group))
  if (includeExcluded) {
    baseline <- rbind(baseline, data.frame(subject_id = "entry-training-excluded",
      entry_state = "A", age_group = "unobserved", score_group = "low"))
    history <- rbind(history, data.frame(subject_id = "entry-training-excluded",
      state_label = "A", state_start_date = as.Date("2025-01-01"),
      state_end_date = as.Date("2025-01-03"), age_group = "young", score_group = "low"))
  }
  list(history = history, baseline = baseline)
}

stratified_entry_estimate <- function(includeExcluded = FALSE) {
  inputs <- stratified_entry_inputs(includeExcluded)
  estimateEntryTransitions(inputs$history, inputs$baseline, cycleLength = 7,
    states = c("A", "B", "D"), absorbingStates = "D",
    strata = c("age_group", "score_group"))
}

stratified_entry_later_model <- function(a = c(1, 0, 0)) {
  probabilities <- rbind(A = a, B = c(0, 1, 0), D = c(0, 0, 1))
  colnames(probabilities) <- c("A", "B", "D")
  list(matrices = list(All = probabilities),
    periods = data.frame(period = "All", start_day = 0, end_day = Inf),
    metadata = list(states = c("A", "B", "D"), absorbingStates = "D", cycleLength = 7))
}

stratified_entry_population <- function() {
  population <- stratified_entry_inputs()$baseline
  population$subject_id <- paste0("source-", 1:4)
  names(population)[names(population) == "entry_state"] <- "state_label"
  population
}

test_that("entry strata use explicit baseline values and retain excluded group support", {
  expect_warning(entry <- stratified_entry_estimate(TRUE), "Excluded")
  expect_s3_class(entry, "tma_stratified_entry")
  expect_setequal(names(entry$models), entry$strata$stratum_id)
  for (i in seq_len(4)) {
    baseline <- stratified_entry_inputs()$baseline[i, ]
    group <- entry$strata$stratum_id[entry$strata$age_group == baseline$age_group &
      entry$strata$score_group == baseline$score_group]
    expect_equal(unname(entry$models[[group]]$matrix["A", ]),
      as.numeric(c("A", "B", "D") == c("A", "B", "B", "D")[i]))
  }
  expect_equal(entry$diagnostics$source_subjects, 5)
  expect_equal(entry$diagnostics$eligible_subjects, 4)
  support <- subset(entry$diagnostics$by_stratum, age_group == "unobserved")
  expect_equal(support[c("source_subjects", "eligible_subjects", "excluded_subjects")],
    data.frame(source_subjects = 1L, eligible_subjects = 0L, excluded_subjects = 1L),
    ignore_attr = TRUE)
  expect_true(all(is.na(entry$models[[support$stratum_id]]$matrix["A", ])))
  expect_equal(sum(entry$coverage$n), 5)
  expect_setequal(entry$subjects$subject_id, stratified_entry_inputs(TRUE)$baseline$subject_id)
  inputs <- stratified_entry_inputs()
  pooled <- estimateEntryTransitions(inputs$history, inputs$baseline, cycleLength = 7,
    states = c("A", "B", "D"), absorbingStates = "D")
  expect_equal(unname(pooled$matrix["A", ]), c(0.25, 0.5, 0.25))
  expect_false(inherits(pooled, "tma_stratified_entry"))
})

test_that("entry selects joint baseline groups independently of later models", {
  entry <- stratified_entry_estimate()
  population <- stratified_entry_population()
  population <- rbind(population, transform(population[1, ],
    subject_id = "already-dead", state_label = "D"))
  result <- simulateTrajectories(createModelBundle(stratified_entry_later_model()),
    population, entry = entry, nSteps = 0, terminalStates = "D", seed = 5)
  expect_equal(result$population$state_label, c("A", "B", "B", "D", "D"))
  expect_equal(result$entry$transitions$simulation_id, paste0("sim_", 1:4))
  expect_equal(result$entry$transitions$entry_stratum_id,
    entry$subjects$stratum_id[match(stratified_entry_inputs()$baseline$subject_id,
      entry$subjects$subject_id)])
  expect_identical(result$entry$strata, entry$strata)
  expect_setequal(names(result$entry$matrices), entry$strata$stratum_id)
  expect_equal(result$entry$population$state_label, population$state_label)
  expect_equal(result$trajectories$elapsed_day, rep(0, 5))
  saved <- as.character(unlist(result$entry, use.names = FALSE))
  expect_false(any(entry$subjects$subject_id %in% saved))
  terminal <- simulateTrajectories(createModelBundle(stratified_entry_later_model()),
    data.frame(state_label = "D"), entry = entry, nSteps = 0, terminalStates = "D")
  expect_s3_class(summariseSimulation(terminal), "tma_simulation_summary")
  terminal <- simulateTrajectories(createModelBundle(stratified_entry_later_model()),
    n = 2, initial = c(D = 1), entry = entry, nSteps = 0, terminalStates = "D")
  expect_s3_class(summariseSimulation(terminal), "tma_simulation_summary")
})

test_that("unsupported entry groups and source states fail before any profile is sampled", {
  entry <- suppressWarnings(stratified_entry_estimate(TRUE))
  bundle <- createModelBundle(stratified_entry_later_model())
  population <- stratified_entry_population()[1:2, ]
  population$age_group[2] <- "unknown"
  expect_error(simulateTrajectories(bundle, population, n = 1, nSteps = 0,
    entry = entry, seed = 1), "[Ee]ntry|strat|group")
  population$age_group[2] <- "unobserved"
  population$score_group[2] <- "low"
  expect_error(simulateTrajectories(bundle, population, n = 1, nSteps = 0,
    entry = entry, seed = 1), "[Uu]nestimated|[Ee]ntry")
  population <- transform(stratified_entry_population()[1, ], state_label = "B")
  expect_s3_class(simulateTrajectories(bundle, population, initial = c(A = 1, B = 0),
    entry = entry, nSteps = 0, seed = 1), "tma_simulation")
  expect_error(simulateTrajectories(bundle, population, initial = c(A = 0.999, B = 0.001),
    entry = entry, nSteps = 0, seed = 1), "[Uu]nestimated|[Ee]ntry")
  expect_error(simulateTrajectories(bundle, n = 1, initial = c(A = 1),
    entry = entry, nSteps = 0, seed = 1), "[Ee]ntry|covariate|population|strat")
  joint <- transform(stratified_entry_population()[1:2, ], probability = c(1, 0))
  joint$subject_id <- NULL
  joint$age_group[2] <- "unknown"
  expect_s3_class(simulateTrajectories(bundle, n = 1, initial = joint,
    entry = entry, nSteps = 0, seed = 1), "tma_simulation")
  joint$elapsed_day <- c(0, 7)
  expect_error(simulateTrajectories(bundle, n = 1, initial = joint,
    entry = entry, nSteps = 0, seed = 1), "zero|elapsed_day|clock")

  inputs <- stratified_entry_inputs(TRUE)
  fit <- function(baseline) estimateEntryTransitions(inputs$history, baseline,
    cycleLength = 7, states = c("A", "B", "D"), absorbingStates = "D",
    strata = c("age_group", "score_group"))
  expect_error(fit(inputs$baseline[-5, ]), "every|subject|cover")
  inputs$baseline$age_group[5] <- NA_character_
  expect_error(fit(inputs$baseline), "missing|complete|NA")
  expect_error(fit(inputs$baseline[c("subject_id", "entry_state")]), "strat|column|baseline")
  expect_error(fit("A"), "data frame|table|baseline|entryState")
})

test_that("later age changes select adjacent models without reapplying entry", {
  models <- structure(list(models = list(young = stratified_entry_later_model(),
    older = stratified_entry_later_model(c(0, 1, 0))),
    strata = data.frame(stratum_id = c("young", "older"), age_group = c("young", "older")),
    metadata = list(strata = "age_group")), class = "tma_stratified_transitions")
  bundle <- createModelBundle(models, groups = list(age_group = list(column = "age",
    breaks = c(0, 65, Inf), labels = c("young", "older"))))
  result <- simulateTrajectories(bundle,
    data.frame(state_label = "A", birth_date = as.Date("1960-01-05"),
      index_date = as.Date("2025-01-01"), score_group = "low"),
    entry = stratified_entry_estimate(), nSteps = 2, seed = 3)
  expect_equal(result$trajectories$state_label, c("A", "A", "B"))
  expect_equal(result$trajectories$age_group, c("young", "older", "older"))
  expect_equal(result$trajectories$age, c(64, 65, 65))
  expect_equal(result$transitions$source_stratum, c("young", "older"))
  expect_equal(nrow(result$entry$transitions), 1)
  expect_equal(result$entry$population$age, 64)
})

test_that("stratified entry effects, upfront fees and comparisons survive resume", {
  entry <- stratified_entry_estimate()
  bundle <- createModelBundle(stratified_entry_later_model())
  population <- stratified_entry_population()
  prevention <- list(from = "A", to = "D", multiplier = 0,
    redistributeTo = "A", applyAt = "entry")
  run <- function(steps, treatment = TRUE) simulateTrajectories(bundle, population,
    entry = entry, nSteps = steps, terminalStates = "D", seed = 8,
    transitionEffects = if (treatment) list(prevention = prevention) else NULL,
    costRules = if (treatment) list(treatment = list(type = "entry", amount = 100)) else
      list(care = list(type = "recurring", amount = 0)),
    currency = "EUR", priceYear = 2026)
  first <- run(0)
  expect_equal(first$entry$effects$entry_stratum_id, first$entry$transitions$entry_stratum_id)
  expect_equal(first$entry$effects$baseline_probability, c(0, 0, 0, 1))
  expect_equal(first$entry$effects$modified_probability, rep(0, 4))
  expect_equal(first$population$state_label, c("A", "B", "B", "A"))
  summarise <- function(x) summariseSimulation(x, aliveStates = c("A", "B"),
    utilities = c(A = 1, B = 0.5, D = 0), daysPerYear = 7)
  reference <- summarise(run(0, FALSE))
  intervention <- summarise(first)
  comparison <- compareScenarios(reference, intervention)
  expect_equal(subset(comparison$outcomes, metric == "cost")$incremental_total, 400)
  expect_equal(subset(comparison$outcomes, metric == "qalys")$incremental_total, 1)
  expect_equal(intervention$initialPopulation$state_label, rep("A", 4))
  full <- run(2)
  resumed <- advanceSimulation(unserialize(serialize(first, NULL)), 2)
  for (field in c("entry", "trajectories", "transitions", "costs", "totals")) {
    expect_identical(resumed[[field]], full[[field]])
  }
  expect_identical(resumed$.state$rng, full$.state$rng)
  expect_equal(nrow(resumed$costs), 4)
  expect_equal(resumed$costs$elapsed_day, rep(0, 4))
})
