#' Estimate ordered stages from explicitly supplied observations
#'
#' @param observations Output of prepareStageObservations(), or a data frame with subject_id, source_day, stage, from_state,
#'   to_state and observed. Each subject/day/stage has at most one row.
#'   observed must be logical. FALSE records an unevaluable outcome, which is
#'   excluded from its stage denominator. An unobserved to_state may be NA.
#'   Covariate columns are preserved. Optional index_date and birth_date allow
#'   mortality calibration to calculate age on the source day.
#' @param stageOrder Unique stage names in the order they operate within a cycle.
#'   Inherited from prepareStageObservations() when omitted.
#'   Names "start", "end" and "transition" are reserved for interval accounting.
#' @param cycleLength Positive integer days per complete cycle. Inherited from
#'   definition when omitted. Supply it explicitly if definition has no cycle.
#' @param states Optional complete ordered state dictionary.
#' @param absorbingStates States that remain unchanged in every stage.
#' @param identityStates Named list of stage-specific unchanged states. These
#'   identity rows are structural assumptions, even without observed support.
#' @param periodBreaks Follow-up boundaries from zero to Inf, aligned to cycles.
#' @param periodLabels Optional period names.
#' @param strata Optional covariate column names. Values are taken from each
#'   cycle's source observations and may change between cycles. Their values
#'   must agree across the stages of one subject/cycle.
#' @param definition Optional prepared trajectories or Cohort2Trajectory data
#'   frame carrying trajectory_definition. Supplies omitted cycleLength, states
#'   and absorbingStates; conflicting explicit arguments are rejected. Raw
#'   definition lists are not accepted. The definition attached by
#'   prepareStageObservations() is used when definition is omitted.
#'   A continuous source supplies labels only;
#'   an explicit cycleLength defines the supplied stage observations.
#' @details The analyst supplies stage membership, event order and evaluability,
#'   either directly or through aligned views passed to prepareStageObservations().
#'   This function does not infer intermediate events from weekly labels.
#'   First-cycle observations are included when observed is TRUE. A censored
#'   outcome is not a self-transition. Unknown nonstructural rows stay NA.
#'   Consecutive observed stages for the same subject/day must join coherently.
#'   Stages add no person-time. Probabilities are conditional on an observed
#'   outcome in that stage and do not adjust for informative censoring.
#'
#'   Supply an explicit initial distribution when projecting a fitted stage
#'   model. The fit retains observations for calibration. These patient-level
#'   rows must stay in the analysis environment. createModelBundle() removes
#'   them and keeps the aggregate models only.
#' @return A tma_stage_transitions model with composed matrices, stageMatrices,
#'   stage-tagged estimates, observations, coverage, diagnostics and metadata.
#'   With strata, also a tma_stratified_transitions object with models and a
#'   stratum registry. Counts, denominators and distinct supporting patients
#'   are returned for every stage/state/period, including unsupported cells.
#' @export
estimateStageTransitions <- function(observations, stageOrder = NULL, cycleLength = NULL,
                                     states = NULL, absorbingStates = character(),
                                     identityStates = list(), periodBreaks = NULL,
                                     periodLabels = NULL, strata = NULL,
                                     definition = NULL) {
  declaredOrder <- attr(observations, "stage_order", exact = TRUE)
  if (!is.null(declaredOrder) && is.data.frame(observations) && "stage" %in% names(observations)) {
    declaredOrder <- declaredOrder[declaredOrder %in% observations$stage]
  }
  if (is.null(stageOrder)) {
    stageOrder <- declaredOrder
  } else if (!is.null(declaredOrder) && !identical(stageOrder, declaredOrder)) {
    stop("stageOrder conflicts with the order of the prepared stage observations.", call. = FALSE)
  }
  if (!is.null(attr(observations, "trajectory_definition", exact = TRUE))) {
    if (is.null(definition)) {
      definition <- observations
    } else {
      suppliedDefinition <- .tmaTrajectoryDefinition(definition, preparedFields = TRUE)
      if (is.null(suppliedDefinition)) {
        stop("definition must supply a trajectory definition.", call. = FALSE)
      }
      .tmaResolveTrajectoryDefinition(.tmaTrajectoryDefinition(observations),
        suppliedDefinition$cycleLength, suppliedDefinition$states,
        suppliedDefinition$absorbingStates,
        supplied = c(cycleLength = TRUE, states = TRUE, absorbingStates = TRUE))
    }
  }
  supplied <- c(cycleLength = !missing(cycleLength), states = !missing(states),
                absorbingStates = !missing(absorbingStates))
  if (!is.null(definition)) {
    definition <- .tmaTrajectoryDefinition(definition, preparedFields = TRUE)
    if (is.null(definition)) {
      stop("definition data frame must carry a trajectory_definition attribute.", call. = FALSE)
    }
  }
  resolved <- .tmaResolveTrajectoryDefinition(definition, cycleLength, states,
    absorbingStates, supplied, allowUndeclaredCycle = TRUE)
  cycleLength <- resolved$cycleLength
  states <- resolved$states
  absorbingStates <- resolved$absorbingStates
  if (is.null(cycleLength)) {
    stop("cycleLength must be supplied explicitly or inherited from definition for stage estimation.",
         call. = FALSE)
  }
  cycleLength <- .tmaProjectionInteger(cycleLength, "cycleLength", 1)
  periods <- .tmaPeriods(periodBreaks, periodLabels, cycleLength)
  required <- c("subject_id", "source_day", "stage", "from_state", "to_state", "observed")
  if (!is.data.frame(observations) || anyDuplicated(names(observations)) ||
      !all(required %in% names(observations)) || !nrow(observations)) {
    stop("observations must contain nonempty subject_id, source_day, stage, from_state, to_state and observed columns.", call. = FALSE)
  }
  if (!is.character(stageOrder) || !is.null(dim(stageOrder)) || !length(stageOrder) ||
      anyNA(stageOrder) || anyDuplicated(stageOrder) || any(!nzchar(trimws(stageOrder)))) {
    stop("stageOrder must contain unique, nonempty names in execution order.", call. = FALSE)
  }
  x <- as.data.frame(observations)
  x$subject_id <- .tmaSubjectIds(x$subject_id)
  for (column in c("stage", "from_state", "to_state")) {
    if (is.factor(x[[column]])) x[[column]] <- as.character(x[[column]])
    if (!is.character(x[[column]])) stop(column, " must contain character labels.", call. = FALSE)
  }
  if (!is.numeric(x$source_day) || is.complex(x$source_day) || anyNA(x$source_day) ||
      any(!is.finite(x$source_day) | x$source_day < 0 | x$source_day >= 2^53 |
            x$source_day %% cycleLength != 0)) {
    stop("source_day must contain nonnegative exact cycle-aligned elapsed days.", call. = FALSE)
  }
  if (!is.logical(x$observed) || anyNA(x$observed)) {
    stop("observed must contain nonmissing TRUE/FALSE outcomes.", call. = FALSE)
  }
  if (anyNA(x$stage) || any(!x$stage %in% stageOrder) || anyNA(x$from_state) ||
      any(!nzchar(trimws(x$from_state))) || anyNA(x$to_state[x$observed])) {
    stop("Stages and source states must be known; observed outcomes need a destination state.", call. = FALSE)
  }
  if (anyDuplicated(x[c("subject_id", "source_day", "stage")])) {
    stop("Each subject_id/source_day/stage must occur once.", call. = FALSE)
  }
  dictionary <- .tmaStateDictionary(unique(c(x$from_state, x$to_state[!is.na(x$to_state)])),
                                   states, absorbingStates)
  states <- dictionary$states
  absorbingStates <- dictionary$absorbingStates
  if (any(!x$from_state %in% states) || any(!x$to_state[!is.na(x$to_state)] %in% states)) {
    stop("Observation state labels must belong to states.", call. = FALSE)
  }
  if (!is.list(identityStates) || is.data.frame(identityStates) ||
      (length(identityStates) && (is.null(names(identityStates)) || anyNA(names(identityStates)) ||
       anyDuplicated(names(identityStates)) || any(!names(identityStates) %in% stageOrder)))) {
    stop("identityStates must be a named list keyed by stageOrder.", call. = FALSE)
  }
  identities <- stats::setNames(vector("list", length(stageOrder)), stageOrder)
  for (stage in stageOrder) {
    value <- identityStates[[stage]]
    if (is.null(value)) value <- character()
    if (!is.character(value) || anyNA(value) || anyDuplicated(value) || any(!value %in% states)) {
      stop("identityStates must name declared states without duplicates.", call. = FALSE)
    }
    identities[[stage]] <- union(absorbingStates, value)
    structural <- x$stage == stage & x$from_state %in% identities[[stage]] & x$observed
    if (any(x$to_state[structural] != x$from_state[structural])) {
      stop("An observed transition contradicts an absorbing or stage-specific identity state.", call. = FALSE)
    }
  }
  x <- x[order(x$subject_id, x$source_day, match(x$stage, stageOrder)), , drop = FALSE]
  rownames(x) <- NULL
  if (nrow(x) > 1L) {
    previous <- seq_len(nrow(x) - 1L)
    nextRow <- previous + 1L
    consecutive <- x$subject_id[previous] == x$subject_id[nextRow] &
      x$source_day[previous] == x$source_day[nextRow] &
      match(x$stage[nextRow], stageOrder) == match(x$stage[previous], stageOrder) + 1L &
      x$observed[previous]
    if (any(x$to_state[previous[consecutive]] != x$from_state[nextRow[consecutive]])) {
      stop("Consecutive stages must join from the preceding observed destination.", call. = FALSE)
    }
  }
  if (any(strata %in% c(required, "period", "stratum_id"))) {
    stop("strata must name covariates, not stage observation fields.", call. = FALSE)
  }
  grouping <- .tmaStrata(x, strata)
  registry <- if (is.null(grouping)) data.frame(stratum_id = "s1") else grouping$registry
  x$stratum_id <- if (is.null(grouping)) rep("s1", nrow(x)) else grouping$row_ids
  if (!is.null(grouping) && nrow(x) > 1L) {
    previous <- seq_len(nrow(x) - 1L)
    nextRow <- previous + 1L
    sameCycle <- x$subject_id[previous] == x$subject_id[nextRow] &
      x$source_day[previous] == x$source_day[nextRow]
    if (any(x$stratum_id[previous[sameCycle]] != x$stratum_id[nextRow[sameCycle]])) {
      stop("strata must agree across stages of one subject/cycle. Encode stage-dependent history in states.", call. = FALSE)
    }
  }
  x$period <- periods$period[.tmaPeriodIndex(x$source_day, periods)]
  models <- stats::setNames(lapply(registry$stratum_id, function(id) {
    .tmaStageObservationModel(x[x$stratum_id == id, , drop = FALSE], stageOrder,
      states, absorbingStates, identities, cycleLength, periods)
  }), registry$stratum_id)
  for (id in names(models)) {
    models[[id]]$metadata$strata <- if (is.null(strata)) character() else strata
    models[[id]]$metadata$stratum_id <- id
  }
  if (is.null(grouping)) return(models[[1L]])
  result <- list(models = models, strata = registry, periods = periods,
    observations = x,
    estimates = do.call(rbind, lapply(seq_along(models), function(i)
      .tmaAddStratum(models[[i]]$estimates, registry[i, , drop = FALSE]))),
    coverage = do.call(rbind, lapply(seq_along(models), function(i)
      .tmaAddStratum(models[[i]]$coverage, registry[i, , drop = FALSE]))),
    diagnostics = list(observations = nrow(x), excluded_outcomes = sum(!x$observed),
      by_stratum = lapply(models, function(model) model$diagnostics)),
    metadata = models[[1L]]$metadata)
  result$metadata$stratum_id <- NULL
  class(result) <- c("tma_stage_transitions", "tma_stratified_transitions")
  result
}

.tmaStageObservationModel <- function(x, stageOrder, states, absorbingStates,
                                       identities, cycleLength, periods) {
  stageMatrices <- stats::setNames(vector("list", length(stageOrder)), stageOrder)
  estimates <- coverage <- list()
  for (stage in stageOrder) {
    stageMatrices[[stage]] <- stats::setNames(vector("list", nrow(periods)), periods$period)
    for (period in periods$period) {
      z <- x[x$stage == stage & x$period == period, , drop = FALSE]
      observed <- z[z$observed, , drop = FALSE]
      counts <- matrix(0, length(states), length(states), dimnames = list(states, states))
      if (nrow(observed)) {
        counts[] <- tabulate((match(observed$to_state, states) - 1L) * length(states) +
          match(observed$from_state, states), nbins = length(states)^2)
      }
      denominator <- rowSums(counts)
      probability <- counts / denominator
      probability[denominator == 0, ] <- NA_real_
      for (state in identities[[stage]]) probability[state, ] <- as.numeric(states == state)
      e <- expand.grid(from_state = states, to_state = states, KEEP.OUT.ATTRS = FALSE,
                       stringsAsFactors = FALSE)
      e$stage <- stage
      e$period <- period
      e$count <- as.vector(counts)
      e$denominator <- unname(denominator[e$from_state])
      e$probability <- as.vector(probability)
      e$structural <- e$from_state %in% identities[[stage]]
      e$patients <- vapply(seq_len(nrow(e)), function(i)
        length(unique(observed$subject_id[observed$from_state == e$from_state[i] &
          observed$to_state == e$to_state[i]])), integer(1))
      e$source_patients <- vapply(e$from_state, function(state)
        length(unique(observed$subject_id[observed$from_state == state])), integer(1))
      estimates[[length(estimates) + 1L]] <- e[c("stage", "period", "from_state", "to_state",
        "count", "denominator", "probability", "structural", "patients", "source_patients")]
      stageMatrices[[stage]][[period]] <- probability
      coverage[[length(coverage) + 1L]] <- data.frame(stage = stage, period = period,
        observations = nrow(z), observed_outcomes = nrow(observed),
        excluded_outcomes = sum(!z$observed), patients = length(unique(z$subject_id)))
    }
  }
  model <- composeTransitionStages(stageMatrices, cycleLength, absorbingStates,
    c(periods$start_day, tail(periods$end_day, 1L)), periods$period)
  model$estimates <- do.call(rbind, estimates)
  model$observations <- x
  model$coverage <- do.call(rbind, coverage)
  model$diagnostics <- list(observations = nrow(x), excluded_outcomes = sum(!x$observed),
    patients = length(unique(x$subject_id)))
  model$metadata$estimator <- "observed_stages"
  model$metadata$identityStates <- identities
  class(model) <- "tma_stage_transitions"
  model
}
