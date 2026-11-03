# Validate the estimator at the same seam used by cohort projection. Retain
# only the matrix and aggregate descriptions, never its training subject IDs.
.tmaSimulationEntryModel <- function(entry, bundle) {
  if (is.null(entry)) return(NULL)
  model <- bundle$metadata
  describe <- function(validated) {
    list(estimator = "entry_outcomes", eligibility = validated$eligibility,
      source_subjects = validated$source_subjects,
      eligible_subjects = validated$eligible_subjects,
      excluded_subjects = validated$source_subjects - validated$eligible_subjects,
      coverage = validated$coverage[c("status", "n")],
      fitting = "pooled within entry state", application = "once before cycle zero")
  }
  if (!inherits(entry, "tma_stratified_entry")) {
    validated <- .tmaProjectionEntry(entry, model$states, model$absorbingStates,
      model$cycleLength, tolerance = 1e-10)$entry
    return(list(matrix = validated$matrix, metadata = describe(validated)))
  }
  columns <- entry$metadata$strata
  registry <- entry$strata
  if (!is.character(columns) || !length(columns) || anyNA(columns) ||
      anyDuplicated(columns) || !is.data.frame(registry) ||
      anyDuplicated(names(registry)) || !nrow(registry) ||
      !setequal(names(registry), c("stratum_id", columns)) ||
      !is.character(registry$stratum_id) || anyNA(registry$stratum_id) ||
      any(!nzchar(trimws(registry$stratum_id))) || anyDuplicated(registry$stratum_id) ||
      !is.list(entry$models) || is.null(names(entry$models)) ||
      anyDuplicated(names(entry$models)) ||
      !setequal(registry$stratum_id, names(entry$models))) {
    stop("Stratified entry must identify each model once in its strata registry.", call. = FALSE)
  }
  .tmaStrata(registry, columns)
  if (anyDuplicated(registry[columns])) {
    stop("Entry strata contain duplicate baseline covariate combinations.", call. = FALSE)
  }
  if (!setequal(entry$metadata$states, model$states) ||
      !setequal(entry$metadata$absorbingStates, model$absorbingStates) ||
      .tmaProjectionInteger(entry$metadata$cycleLength, "entry cycleLength", 1) != model$cycleLength) {
    stop("Entry states, absorbingStates and cycleLength must match the adjacent-cycle model.", call. = FALSE)
  }
  checked <- lapply(entry$models[registry$stratum_id], function(value)
    .tmaProjectionEntry(value, model$states, model$absorbingStates,
      model$cycleLength, tolerance = 1e-10, allowEmpty = TRUE)$entry)
  metadata <- describe(checked[[1L]])
  metadata$strata <- columns
  metadata$fitting <- "within entry state and supplied baseline strata"
  countFields <- c("source_subjects", "eligible_subjects", "excluded_subjects")
  counts <- data.frame(stratum_id = registry$stratum_id, stringsAsFactors = FALSE)
  for (field in countFields) {
    counts[[field]] <- vapply(checked, function(value) describe(value)[[field]], numeric(1))
    metadata[[field]] <- sum(counts[[field]])
  }
  metadata$strata_counts <- counts
  metadata$coverage <- do.call(rbind, lapply(seq_along(checked), function(i)
    data.frame(stratum_id = registry$stratum_id[i], checked[[i]]$coverage[c("status", "n")],
      row.names = NULL, stringsAsFactors = FALSE)))
  rownames(metadata$coverage) <- NULL
  list(matrices = lapply(checked, `[[`, "matrix"),
    strata = registry[c("stratum_id", columns)], metadata = metadata)
}

# Called before profile and state sampling so unsupported rows cannot disappear
# by chance. Independent initial states are checked against every profile.
.tmaSimulationEntryProfiles <- function(entry, population = NULL,
                                        states = NULL, bundle = NULL,
                                        terminalStates = character(), probability = NULL) {
  if (is.null(entry)) return(invisible(NULL))
  if (!is.null(population) &&
      any(population$elapsed_day != 0 | population$cycle != 0)) {
    stop("Entry profiles must start at elapsed_day = 0 and cycle = 0, including profiles not sampled.",
      call. = FALSE)
  }
  if (is.null(population) && length(entry$metadata$strata) &&
      length(setdiff(states, terminalStates))) {
    stop("Stratified entry requires population covariates or a joint initial distribution.", call. = FALSE)
  }
  check <- function(profiles) {
    profiles <- profiles[!profiles$state_label %in% terminalStates, , drop = FALSE]
    if (!nrow(profiles)) return(invisible(NULL))
    if (!is.null(bundle)) {
      profiles <- .tmaSimulationCovariates(bundle, profiles)
      .tmaBundleModel(bundle, profiles)
    }
    ids <- .tmaEntryModelIds(entry, profiles)
    for (id in unique(ids)) {
      labels <- unique(profiles$state_label[ids == id])
      matrix <- .tmaEntryMatrix(entry, id)
      unsupported <- labels[rowSums(is.na(matrix[labels, , drop = FALSE])) > 0]
      if (length(unsupported)) {
        stop("Unestimated entry row for supplied baseline state(s): ",
          paste(unsupported, collapse = ", "),
          if (length(entry$metadata$strata)) paste0(" in entry stratum '", id, "'") else "",
          ".", call. = FALSE)
      }
    }
    invisible(NULL)
  }
  if (is.null(population)) {
    check(data.frame(state_label = unique(states), stringsAsFactors = FALSE))
  } else {
    if (!is.null(probability)) population <- population[probability > 0, , drop = FALSE]
    if (is.null(states)) check(population) else for (state in setdiff(unique(states), terminalStates)) {
      population$state_label <- state
      check(population)
    }
  }
  invisible(NULL)
}

# Entry and later models can use different covariate tuples. Only rows that
# require an entry draw are supplied here; skipped terminal profiles need none.
.tmaEntryModelIds <- function(entry, population) {
  if (!nrow(population)) return(character())
  columns <- entry$metadata$strata
  if (!length(columns)) return(rep("pooled", nrow(population)))
  if (!all(columns %in% names(population))) {
    stop("Population is missing entry covariates: ",
      paste(setdiff(columns, names(population)), collapse = ", "), ".", call. = FALSE)
  }
  .tmaStrata(population, columns)
  values <- as.data.frame(population[columns])
  registry <- entry$strata
  for (column in columns) {
    if (is.factor(values[[column]])) values[[column]] <- as.character(values[[column]])
    if (is.factor(registry[[column]])) registry[[column]] <- as.character(registry[[column]])
  }
  matched <- dplyr::left_join(values, registry, by = columns)
  if (nrow(matched) != nrow(population) || anyNA(matched$stratum_id)) {
    stop("No entry model supports one or more baseline covariate combinations. Supply supported profiles or an explicit model; no fallback is applied.",
      call. = FALSE)
  }
  matched$stratum_id
}

.tmaEntryMatrix <- function(entry, id) {
  if (length(entry$metadata$strata)) entry$matrices[[id]] else entry$matrix
}

# Entry supplies the first outcome label. Baseline covariates are already
# derived, and terminal patients keep them without a draw or another clock tick.
.tmaSimulationApplyEntry <- function(bundle, baseline, entry, terminalStates,
                                     effects = list()) {
  population <- baseline
  active <- which(!baseline$state_label %in% terminalStates)
  entryIds <- if (length(active)) .tmaEntryModelIds(entry, baseline[active, , drop = FALSE]) else character()
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
    probability <- .tmaEntryMatrix(entry, entryIds[j])[baseline$state_label[row], ]
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
    selection <- .tmaBundleSelection(bundle,
      population[alive, , drop = FALSE], allowUnmatched = TRUE)
    population$stratum_id[alive] <- selection$stratum_id
    if (!is.null(bundle$extrapolation)) {
      population$model_phase[alive] <- selection$model_phase
    }
  }
  transitions <- data.frame(
    simulation_id = baseline$simulation_id[active],
    source_cycle = baseline$cycle[active], destination_cycle = population$cycle[active],
    source_day = baseline$elapsed_day[active], destination_day = population$elapsed_day[active],
    source_state = baseline$state_label[active], destination_state = population$state_label[active],
    transition_probability = drawProbability, stringsAsFactors = FALSE)
  if (length(entry$metadata$strata)) transitions$entry_stratum_id <- entryIds
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
    if (length(entry$metadata$strata)) effectRows$entry_stratum_id <- entryIds[applied]
  }
  saved <- if (length(entry$metadata$strata)) {
    list(matrices = entry$matrices, strata = entry$strata)
  } else list(matrix = entry$matrix)
  result <- list(population = population, entry = c(list(population = baseline,
    transitions = transitions), saved, list(metadata = entry$metadata)))
  if (length(entryRules)) {
    result$entry$effects <- effectRows
    result$entry$effectRules <- entryRules
  }
  result
}
