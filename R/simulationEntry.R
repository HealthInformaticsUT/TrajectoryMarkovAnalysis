# Validate the estimator at the same seam used by cohort projection. Retain
# only the matrix and aggregate descriptions, never its training subject IDs.
.tmaSimulationEntryModel <- function(entry, bundle) {
  if (is.null(entry)) return(NULL)
  model <- bundle$metadata
  validated <- .tmaProjectionEntry(entry, model$states, model$absorbingStates,
    model$cycleLength, tolerance = 1e-10)$entry
  list(matrix = validated$matrix,
    metadata = list(estimator = "entry_outcomes", eligibility = validated$eligibility,
      source_subjects = validated$source_subjects,
      eligible_subjects = validated$eligible_subjects,
      excluded_subjects = validated$source_subjects - validated$eligible_subjects,
      coverage = validated$coverage[c("status", "n")],
      fitting = "pooled within entry state", application = "once before cycle zero"))
}

# Called before profile and state sampling so unsupported rows cannot disappear
# by chance. The caller supplies effective states when initial overrides them.
.tmaSimulationEntryProfiles <- function(entry, population = NULL,
                                        states = character()) {
  if (is.null(entry)) return(invisible(NULL))
  if (!is.null(population) &&
      any(population$elapsed_day != 0 | population$cycle != 0)) {
    stop("Entry profiles must start at elapsed_day = 0 and cycle = 0, including profiles not sampled.",
      call. = FALSE)
  }
  states <- unique(states)
  unsupported <- states[rowSums(is.na(entry$matrix[states, , drop = FALSE])) > 0]
  if (length(unsupported)) {
    stop("Unestimated entry row for supplied baseline state(s): ",
      paste(unsupported, collapse = ", "), ".", call. = FALSE)
  }
  invisible(NULL)
}

# Entry supplies the first outcome label. Baseline covariates are already
# derived, and terminal patients keep them without a draw or another clock tick.
.tmaSimulationApplyEntry <- function(bundle, baseline, entry, terminalStates,
                                     effects = list()) {
  population <- baseline
  active <- which(!baseline$state_label %in% terminalStates)
  drawProbability <- numeric(length(active))
  entryRules <- effects[vapply(effects, function(rule)
    rule$applyAt %in% c("entry", "both"), logical(1))]
  if (length(entryRules)) {
    # Complete eligibility checks before draws, using the supplied baseline
    # profiles rather than outcomes generated for preceding patients.
    effectIds <- .tmaEligibleEffects(entryRules, baseline[active, , drop = FALSE],
      applyAt = "entry")
    baselineDraw <- numeric(length(active))
    baselineTarget <- modifiedTarget <- baselineRedistributed <- modifiedRedistributed <-
      rep(NA_real_, length(active))
    effectRows <- .tmaSimulationEmptyEffects()
  }
  for (j in seq_along(active)) {
    row <- active[j]
    probability <- entry$matrix[baseline$state_label[row], ]
    if (length(entryRules)) {
      original <- probability
      if (!is.na(effectIds[j])) {
        rule <- entryRules[[effectIds[j]]]
        probability <- tryCatch(
          .tmaApplyTransitionEffect(original, rule, bundle$metadata$states),
          error = function(error) stop("Entry transition effect '", effectIds[j],
            "' failed for ", baseline$simulation_id[row], " in source state '",
            baseline$state_label[row], "' at day ", baseline$elapsed_day[row],
            ": ", conditionMessage(error), call. = FALSE))
        baselineTarget[j] <- original[rule$to]
        modifiedTarget[j] <- probability[rule$to]
        baselineRedistributed[j] <- original[rule$redistributeTo]
        modifiedRedistributed[j] <- probability[rule$redistributeTo]
      }
    }
    destination <- sample.int(length(probability), 1L, prob = probability)
    population$state_label[row] <- names(probability)[destination]
    drawProbability[j] <- probability[destination]
    if (length(entryRules)) baselineDraw[j] <- original[destination]
  }
  alive <- which(!population$state_label %in% terminalStates)
  if (length(alive)) {
    population <- .tmaSimulationMergeRows(population, alive,
      .tmaSimulationCovariates(bundle, population[alive, , drop = FALSE]))
    population$stratum_id[alive] <- .tmaBundleModel(bundle,
      population[alive, , drop = FALSE], allowUnmatched = TRUE)
  }
  transitions <- data.frame(
    simulation_id = baseline$simulation_id[active],
    source_cycle = baseline$cycle[active], destination_cycle = population$cycle[active],
    source_day = baseline$elapsed_day[active], destination_day = population$elapsed_day[active],
    source_state = baseline$state_label[active], destination_state = population$state_label[active],
    transition_probability = drawProbability, stringsAsFactors = FALSE)
  if (length(entryRules)) {
    transitions$baseline_transition_probability <- baselineDraw
    transitions$effect <- unname(effectIds)
    applied <- which(!is.na(effectIds))
    if (length(applied)) {
      rules <- entryRules[effectIds[applied]]
      rows <- active[applied]
      effectRows <- data.frame(
        simulation_id = baseline$simulation_id[rows], source_cycle = baseline$cycle[rows],
        source_day = baseline$elapsed_day[rows], source_state = baseline$state_label[rows],
        source_stratum = baseline$stratum_id[rows], source_period = "entry",
        effect = unname(effectIds[applied]),
        to_state = vapply(rules, function(rule) rule$to, character(1)),
        redistribute_to = vapply(rules, function(rule) rule$redistributeTo, character(1)),
        multiplier = vapply(rules, function(rule) rule$multiplier, numeric(1)),
        baseline_probability = baselineTarget[applied], modified_probability = modifiedTarget[applied],
        baseline_redistributed_probability = baselineRedistributed[applied],
        modified_redistributed_probability = modifiedRedistributed[applied],
        row.names = NULL, stringsAsFactors = FALSE)
    }
  }
  result <- list(population = population, entry = list(population = baseline,
    transitions = transitions, matrix = entry$matrix, metadata = entry$metadata))
  if (length(entryRules)) {
    result$entry$effects <- effectRows
    result$entry$effectRules <- entryRules
  }
  result
}
