#' Attach explicit transition probabilities for later simulation cycles
#'
#' @param bundle A model bundle from createModelBundle(), without an existing
#'   extrapolation.
#' @param fromDay Positive exact whole source day, aligned with cycleLength.
#'   The supplied transitions apply when source elapsed_day is at least fromDay.
#' @param transitions A transition model accepted by createModelBundle(). It
#'   must share the original states, absorbing states and cycle length.
#' @param groups Optional grouping rules for the late transition model, as in
#'   createModelBundle(). Its strata and groups may differ from the original.
#' @param source Required nonblank analyst-supplied evidence or assumption label.
#' @details Exactly one explicit switch is supported. Late-model period day
#'   zero means fromDay: period selection uses elapsed_day - fromDay. Original
#'   periods, global elapsed time, calendar ageing and cost periods retain their
#'   existing clocks. Cost models and cost rules are not replaced.
#'
#'   Only the selected phase's grouping rules and original cost-model grouping
#'   rules are applied. Existing derived columns from an inactive phase are
#'   cleared to NA; raw covariates are retained. Effects and callbacks should
#'   use raw covariates or the active phase's groups. Reusing a derived stratum
#'   name in the late model requires its grouping rule there as well, unless
#'   the original cost-model grouping already supplies it.
#'   Before updateCovariates runs, a group with a missing raw input is NA; the
#'   callback must supply that input before strict grouping and model selection.
#'
#'   No support cutoff or probabilities are inferred from fitted counts. The
#'   analyst chooses the switch day, late probabilities and source label. This
#'   helper does not fit mortality or another extrapolation algorithm.
#'   For a late model combining supplied background mortality with calibrated
#'   excess mortality, use extrapolateMortality() instead.
#' @return A tma_model_bundle retaining its original models and costs, with a
#'   validated extrapolation definition for simulateTrajectories().
#' @export
extendModelBundle <- function(bundle, fromDay, transitions, groups = NULL, source) {
  if (inherits(bundle, "tma_mortality_bundle")) {
    stop("A mortality extrapolation must remain attached to its original bundle. Use extrapolateMortality() with the original model bundle.", call. = FALSE)
  }
  if (inherits(bundle, "tma_model_bundle") && !is.null(bundle$extrapolation)) {
    stop("This bundle already has an extrapolation; only one explicit switch is supported.", call. = FALSE)
  }
  bundle <- .tmaValidateModelBundle(bundle)
  if (inherits(transitions, "tma_model_bundle") ||
      (is.list(transitions) && !is.null(transitions$extrapolation))) {
    stop("transitions must be a transition model, not a bundle or nested extrapolation.", call. = FALSE)
  }
  if (missing(source)) stop("source must be one nonblank analyst-supplied label.", call. = FALSE)
  extension <- list(fromDay = fromDay, source = source,
                    bundle = createModelBundle(transitions, groups = groups))
  bundle$extrapolation <- .tmaValidateBundleExtrapolation(extension, bundle)
  bundle
}

.tmaValidateBundleExtrapolation <- function(extension, base) {
  if (!is.list(extension) || is.data.frame(extension) ||
      anyDuplicated(names(extension)) ||
      !setequal(names(extension), c("fromDay", "source", "bundle"))) {
    stop("Bundle extrapolation must contain fromDay, source and one transition bundle.", call. = FALSE)
  }
  fromDay <- .tmaProjectionInteger(extension$fromDay, "fromDay", minimum = 1)
  if (fromDay %% base$metadata$cycleLength != 0) {
    stop("fromDay must align with the bundle cycleLength.", call. = FALSE)
  }
  source <- extension$source
  if (!is.character(source) || length(source) != 1L || !is.null(dim(source)) ||
      is.na(source) || !nzchar(trimws(source))) {
    stop("source must be one nonblank analyst-supplied label.", call. = FALSE)
  }
  tail <- extension$bundle
  if (!inherits(tail, "tma_model_bundle")) {
    stop("Bundle extrapolation must contain a transition bundle from createModelBundle().", call. = FALSE)
  }
  if (!is.null(tail$extrapolation)) {
    stop("Nested extrapolations are not supported; only one explicit switch is allowed.", call. = FALSE)
  }
  if (!is.null(tail$costModel) || !is.null(tail$costEstimates)) {
    stop("Extrapolation must not replace or attach costs; the original cost configuration is retained.", call. = FALSE)
  }
  tail <- .tmaValidateModelBundle(tail)
  states <- base$metadata$states
  if (!setequal(tail$metadata$states, states) ||
      !setequal(tail$metadata$absorbingStates, base$metadata$absorbingStates) ||
      tail$metadata$cycleLength != base$metadata$cycleLength) {
    stop("Extrapolation must share the original states, absorbing states and cycleLength.", call. = FALSE)
  }
  if (inherits(tail, "tma_mortality_bundle")) {
    if (!identical(tail$metadata$states, states)) {
      stop("A mortality extrapolation must retain the original state order. Recreate it with extrapolateMortality().", call. = FALSE)
    }
  } else {
    # Ordinary late matrices may have an independently supplied state order.
    for (id in names(tail$models)) {
      model <- tail$models[[id]]
      model$metadata$states <- states
      model$matrices <- lapply(model$matrices, function(x) x[states, states, drop = FALSE])
      model$matrix <- if (length(model$matrices) == 1L) model$matrices[[1L]] else NULL
      if (!is.null(model$stageMatrices)) {
        model$stageMatrices <- .tmaStageDefinition(model$stageMatrices, model$periods,
          states, base$metadata$absorbingStates)$stageMatrices
      }
      tail$models[[id]] <- model
    }
    tail$metadata$states <- states
  }
  outputNames <- union(names(base$groups), names(tail$groups))
  rawNames <- vapply(c(base$groups, tail$groups), function(rule) rule$column, character(1))
  collisions <- intersect(outputNames, rawNames)
  if (length(collisions)) {
    stop("Grouping outputs must not replace raw grouping inputs across model phases: ",
         paste(collisions, collapse = ", "), ".", call. = FALSE)
  }
  costGroups <- intersect(names(base$groups), base$costModel$metadata$strata)
  reused <- intersect(setdiff(tail$metadata$strata, union(names(tail$groups), costGroups)),
                       names(base$groups))
  if (length(reused)) {
    stop("Extrapolation strata reusing original grouping outputs must declare their grouping rules: ",
         paste(reused, collapse = ", "), ".", call. = FALSE)
  }
  directCost <- setdiff(base$costModel$metadata$strata, names(base$groups))
  collisions <- intersect(names(tail$groups), directCost)
  if (length(collisions)) {
    stop("Extrapolation grouping outputs must not overwrite directly supplied original cost-model covariates: ",
         paste(collisions, collapse = ", "), ".", call. = FALSE)
  }
  for (name in intersect(names(tail$groups), costGroups)) {
    original <- base$groups[[name]]
    late <- tail$groups[[name]]
    if (!identical(original$column, late$column) ||
        !isTRUE(all.equal(original$breaks, late$breaks)) ||
        !identical(original$labels, late$labels)) {
      stop("Extrapolation grouping rule '", name,
           "' conflicts with the original cost-model grouping rule.", call. = FALSE)
    }
  }
  # Check combined rules too: a late output cannot silently become the raw
  # input for an original cost grouping rule, or vice versa.
  .tmaCovariateGroups(c(tail$groups,
    base$groups[setdiff(costGroups, names(tail$groups))]))
  list(fromDay = fromDay, source = source, bundle = tail)
}

.tmaBundleElapsed <- function(population) {
  if (!"elapsed_day" %in% names(population)) return(rep(0, nrow(population)))
  elapsed <- population$elapsed_day
  if (!is.numeric(elapsed) || is.complex(elapsed) || inherits(elapsed, "integer64") ||
      !is.null(dim(elapsed)) || anyNA(elapsed) ||
      any(!is.finite(elapsed) | elapsed < 0 | elapsed != trunc(elapsed) | elapsed >= 2^53)) {
    stop("Population elapsed_day must contain exact nonnegative finite whole days.", call. = FALSE)
  }
  elapsed
}

.tmaBundlePhase <- function(bundle, population) {
  phase <- rep("base", nrow(population))
  if (!is.null(bundle$extrapolation)) {
    phase[.tmaBundleElapsed(population) >= bundle$extrapolation$fromDay] <- "extrapolation"
  }
  phase
}

.tmaBundleSelection <- function(bundle, population, allowUnmatched = FALSE) {
  elapsed <- .tmaBundleElapsed(population)
  result <- data.frame(model_phase = .tmaBundlePhase(bundle, population),
    model_source = rep("original bundle", nrow(population)),
    stratum_id = rep(NA_character_, nrow(population)),
    period = rep(NA_character_, nrow(population)), model_day = elapsed,
    stringsAsFactors = FALSE)
  for (phase in unique(result$model_phase)) {
    rows <- which(result$model_phase == phase)
    selected <- if (phase == "base") bundle else bundle$extrapolation$bundle
    if (phase == "extrapolation") {
      result$model_source[rows] <- bundle$extrapolation$source
      result$model_day[rows] <- elapsed[rows] - bundle$extrapolation$fromDay
    }
    result$stratum_id[rows] <- .tmaBundleStratum(selected,
      population[rows, , drop = FALSE], allowUnmatched)
    result$period[rows] <- selected$metadata$periods$period[
      .tmaPeriodIndex(result$model_day[rows], selected$metadata$periods)]
  }
  result
}

.tmaBundleTransitionModel <- function(bundle, phase, id) {
  if (!is.character(phase) || length(phase) != 1L || is.na(phase) ||
      !phase %in% c("base", "extrapolation")) {
    stop("Transition model phase must be 'base' or 'extrapolation'.", call. = FALSE)
  }
  selected <- if (phase == "base") bundle else bundle$extrapolation$bundle
  if (is.null(selected) || !is.character(id) || length(id) != 1L ||
      is.na(id) || !id %in% names(selected$models)) {
    stop("No transition model is available for the selected phase and stratum.", call. = FALSE)
  }
  selected$models[[id]]
}

.tmaBundleUsesFutureMortality <- function(bundle) {
  inherits(bundle$extrapolation$bundle, "tma_mortality_bundle")
}

.tmaBundleMortalityAudit <- function(n) {
  data.frame(background_stratum_id = rep(NA_character_, n),
    background_death_probability = rep(NA_real_, n),
    excess_hazard_per_day = rep(NA_real_, n),
    integrated_excess_hazard = rep(NA_real_, n),
    mortality_probability = rep(NA_real_, n), stringsAsFactors = FALSE)
}

# Both model forms supply complete source rows before any intervention or draw.
# Background grouping stays private to the mortality evaluator and cannot
# overwrite clinical or cost groups in the patient profile.
.tmaBundleProbabilityRows <- function(bundle, population, selection) {
  probability <- vector("list", nrow(population))
  audit <- if (.tmaBundleUsesFutureMortality(bundle)) {
    .tmaBundleMortalityAudit(nrow(population))
  } else NULL
  for (phase in unique(selection$model_phase)) {
    rows <- which(selection$model_phase == phase)
    selected <- if (phase == "base") bundle else bundle$extrapolation$bundle
    if (inherits(selected, "tma_mortality_bundle")) {
      future <- .tmaFutureMortalityRows(selected, population[rows, , drop = FALSE],
        selection$model_day[rows], selection$stratum_id[rows])
      probability[rows] <- future$probabilities
      audit[rows, ] <- future$audit
    } else {
      probability[rows] <- lapply(rows, function(row) {
        model <- .tmaBundleTransitionModel(bundle, phase, selection$stratum_id[row])
        result <- model$matrices[[selection$period[row]]][population$state_label[row], ]
        if (anyNA(result)) {
          stop("Unestimated transition row for state '", population$state_label[row],
            "' in stratum '", selection$stratum_id[row], "', period '", selection$period[row],
            if (!is.null(bundle$extrapolation)) paste0("', model phase '", phase) else "",
            "' for ", population$simulation_id[row], ".", call. = FALSE)
        }
        result
      })
    }
  }
  list(probabilities = probability, audit = audit)
}

.tmaBundleGroupNames <- function(bundle) {
  union(names(bundle$groups), names(bundle$extrapolation$bundle$groups))
}

.tmaBundleExtrapolationInfo <- function(bundle) {
  extension <- bundle$extrapolation
  if (is.null(extension)) return(NULL)
  registry <- function(x) {
    result <- list(strata = x$strata, groups = x$groups,
                   periods = x$metadata$periods)
    if (inherits(x, "tma_mortality_bundle")) {
      result$mortality <- .tmaFutureMortalityInfo(x)
    }
    result
  }
  list(fromDay = extension$fromDay, source = extension$source,
       clock = "days since fromDay", base = registry(bundle),
       extrapolation = registry(extension$bundle))
}
