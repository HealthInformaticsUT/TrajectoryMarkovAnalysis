# Reconcile saved entry draws and their probability audit without rerunning
# eligibility callbacks or sampling. The baseline matrix stays an estimate.
.tmaSummaryEntryEffects <- function(simulation, draws, baseline, expected, entryIds) {
  entry <- simulation$entry
  model <- simulation$.config$bundle$metadata
  rules <- .tmaTransitionEffects(entry$effectRules, model$states,
    model$absorbingStates, model$cycleLength)
  if (!length(rules)) {
    if (!is.null(entry$effects) || any(c("effect", "baseline_transition_probability") %in% names(draws))) {
      stop("Entry effect records require their saved rules.", call. = FALSE)
    }
    return(expected)
  }
  if (any(vapply(rules, function(rule) !rule$applyAt %in% c("entry", "both"), logical(1))) ||
      !all(c("effect", "baseline_transition_probability") %in% names(draws)) ||
      !is.character(draws$effect) || any(!is.na(draws$effect) & !draws$effect %in% names(rules))) {
    stop("Entry effects must reference saved entry-applicable rules.", call. = FALSE)
  }
  original <- draws$baseline_transition_probability
  if (!is.numeric(original) || is.complex(original) || anyNA(original) ||
      any(!is.finite(original) | original < 0 | original > 1) ||
      anyNA(expected) || any(original != expected)) {
    stop("Baseline entry probabilities must agree with the saved entry matrix.", call. = FALSE)
  }
  audit <- entry$effects
  fields <- names(.tmaSimulationEmptyEffects())
  if (!is.null(entry$matrices)) fields <- c(fields, "entry_stratum_id")
  applied <- !is.na(draws$effect)
  if (!is.data.frame(audit) || anyDuplicated(names(audit)) || !all(fields %in% names(audit)) ||
      !is.character(audit$simulation_id) || anyNA(audit$simulation_id) ||
      anyDuplicated(audit$simulation_id) ||
      !setequal(audit$simulation_id, draws$simulation_id[applied])) {
    stop("Entry effect audit must identify each affected draw exactly once.", call. = FALSE)
  }
  # The same intervention may change a different estimated row in each group.
  for (id in unique(entryIds[applied])) for (name in unique(draws$effect[applied & entryIds == id])) {
    rule <- rules[[name]]
    selected <- which(!is.na(draws$effect) & draws$effect == name & entryIds == id)
    source <- baseline[selected, , drop = FALSE]
    if (any(source$state_label != rule$from) ||
        any(source$elapsed_day < rule$startDay | source$elapsed_day >= rule$endDay)) {
      stop("Entry effects must agree with their source states and time windows.", call. = FALSE)
    }
    originalRow <- .tmaEntryMatrix(entry, id)[rule$from, ]
    modified <- .tmaApplyTransitionEffect(originalRow, rule, model$states)
    expected[selected] <- modified[draws$destination_state[selected]]
    saved <- audit[match(source$simulation_id, audit$simulation_id), , drop = FALSE]
    calculated <- data.frame(
      simulation_id = source$simulation_id, source_cycle = source$cycle,
      source_day = source$elapsed_day, source_state = source$state_label,
      source_stratum = source$stratum_id, source_period = "entry", effect = name,
      to_state = rule$to, redistribute_to = rule$redistributeTo, multiplier = rule$multiplier,
      baseline_probability = unname(originalRow[rule$to]),
      modified_probability = unname(modified[rule$to]),
      baseline_redistributed_probability = unname(originalRow[rule$redistributeTo]),
      modified_redistributed_probability = unname(modified[rule$redistributeTo]),
      stringsAsFactors = FALSE)
    if (!is.null(entry$matrices)) calculated$entry_stratum_id <- id
    if (!all(vapply(fields, function(field) isTRUE(all.equal(saved[[field]],
        calculated[[field]], tolerance = 0, check.attributes = FALSE)), logical(1)))) {
      stop("Entry effect audit must agree with saved rules and calculated probabilities.", call. = FALSE)
    }
  }
  expected
}

# Stored entry IDs are local to the entry registry, not to the later models.
# Check the baseline tuple again so an audit cannot silently select another row.
.tmaSummaryEntryModelIds <- function(entry, baseline, draws, states) {
  if (is.null(entry$matrices)) {
    matrices <- list(pooled = entry$matrix)
    if (length(entry$metadata$strata)) {
      stop("Stratified entry summaries must retain their matrices and registry.", call. = FALSE)
    }
  } else {
    columns <- entry$metadata$strata
    registry <- entry$strata
    matrices <- entry$matrices
    if (!length(columns) || !is.data.frame(registry) || anyDuplicated(names(registry)) ||
        !all(c("stratum_id", columns) %in% names(registry)) ||
        !is.character(registry$stratum_id) || anyNA(registry$stratum_id) ||
        anyDuplicated(registry$stratum_id) || !is.list(matrices) ||
        is.null(names(matrices)) || anyNA(names(matrices)) || anyDuplicated(names(matrices)) ||
        !setequal(registry$stratum_id, names(matrices)) || !is.null(entry$matrix)) {
      stop("Stratified entry summaries must retain one matrix for every registry group.", call. = FALSE)
    }
    .tmaStrata(registry, columns)
    if (anyDuplicated(registry[columns])) {
      stop("Entry registry contains duplicate baseline covariate combinations.", call. = FALSE)
    }
  }
  for (matrix in matrices) {
    if (!is.matrix(matrix) || !is.numeric(matrix) || is.complex(matrix) ||
        !identical(dim(matrix), rep(length(states), 2L)) ||
        anyDuplicated(rownames(matrix)) || anyDuplicated(colnames(matrix)) ||
        !setequal(rownames(matrix), states) || !setequal(colnames(matrix), states)) {
      stop("Entry matrices must retain the model state dictionary.", call. = FALSE)
    }
  }
  ids <- .tmaEntryModelIds(entry, baseline)
  if (!is.null(entry$matrices) && (!is.character(draws$entry_stratum_id) ||
      length(draws$entry_stratum_id) != nrow(draws) || anyNA(draws$entry_stratum_id) ||
      any(draws$entry_stratum_id != ids))) {
    stop("Saved entry stratum IDs must match the pre-entry patient covariates.", call. = FALSE)
  }
  ids
}
