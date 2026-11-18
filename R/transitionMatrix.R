#' Extract a transition matrix from a fitted model or cohort projection
#'
#' @param x A result from estimateTransitions(), estimateStageTransitions(),
#'   composeTransitionStages() or createMortalityTransitions(), an ordinary
#'   createModelBundle() bundle, or a result from projectCohort().
#' @param period Exact period label. Required when a fitted model has more than
#'   one period. Do not supply period for a projection; cycle selects its period.
#' @param stratum Exact stratum_id from x$strata, not a covariate value. Required
#'   for stratified fits or bundles with more than one model. Omit for an
#'   individual model or projection.
#' @param stage Exact stage name. NULL returns the combined full-cycle matrix;
#'   a name selects that stage's conditional matrix.
#' @param cycle Zero-based source cycle, required for projections only. For
#'   example, cycle = 0 returns the matrix used from occupancy cycle 0 to 1.
#'   Only completed transition steps can be selected.
#' @details Fit and bundle queries return the original stored probabilities,
#'   including unestimated NA rows. Projection queries return the matrices
#'   actually used at the requested source cycle, including transition effects.
#'   A stage query on a projection uses its effective stage matrix; it fails if
#'   stage paths were unavailable after net-cycle effects. To inspect original
#'   stage probabilities, query the fitted model instead.
#'
#'   Selectors match exact names and never pool groups or choose the first of
#'   several periods or strata. Omitting stage deliberately selects the
#'   combined matrix. Simulation results, mortality extrapolation bundles and
#'   bundles with a late-model switch are not supported because their effective
#'   probabilities need additional source-model or patient-profile selection.
#'   This accessor does not modify, normalise or recalculate probabilities.
#' @return One numeric matrix with the stored state names and ordering.
#' @seealso composeTransitionStages(), projectCohort()
#' @export
transitionMatrix <- function(x, period = NULL, stratum = NULL, stage = NULL,
                             cycle = NULL) {
  for (name in c("period", "stratum", "stage")) {
    value <- get(name)
    if (!is.null(value) && (!is.character(value) || length(value) != 1L ||
        !is.null(dim(value)) || is.na(value) || !nzchar(trimws(value)))) {
      stop(name, " must be NULL or one nonempty exact name.", call. = FALSE)
    }
  }
  if (!is.list(x) || is.data.frame(x)) {
    stop("x must be a transition model, model bundle or cohort projection.", call. = FALSE)
  }
  if (inherits(x, "tma_simulation")) {
    stop("Simulation results are not supported; query the source transition model or bundle.",
         call. = FALSE)
  }
  if (inherits(x, "tma_mortality_bundle") || !is.null(x$extrapolation)) {
    stop("Mortality extrapolations and bundles with a late-model switch are not supported; select an ordinary source model explicitly.",
         call. = FALSE)
  }
  if ("cycleMatrices" %in% names(x) && is.data.frame(x$steps)) {
    if (is.null(cycle)) {
      stop("Supply cycle as a zero-based source cycle for a projection.", call. = FALSE)
    }
    if (!is.null(period) || !is.null(stratum)) {
      stop("Projection queries use cycle; do not supply period or stratum.", call. = FALSE)
    }
    cycle <- .tmaProjectionInteger(cycle, "cycle", minimum = 0)
    if (!nrow(x$steps)) {
      stop("This projection has no completed transition steps to select.", call. = FALSE)
    }
    index <- match(cycle, x$steps$source_cycle)
    if (is.na(index)) {
      stop("Unknown source cycle ", cycle, "; select a completed transition step.", call. = FALSE)
    }
    if (is.null(stage)) {
      matrix <- x$cycleMatrices[[index]]
    } else {
      if (is.null(x$stageCycleMatrices)) {
        stop("Effective stage matrices are unavailable for this projection, including after net-cycle effects.",
             call. = FALSE)
      }
      matrix <- .tmaSelectStoredMatrix(x$stageCycleMatrices[[index]], stage, "stage")
    }
  } else {
    if (!is.null(cycle)) {
      stop("cycle is supported only for a cohort projection; select a fitted model by period.",
           call. = FALSE)
    }
    if (inherits(x, "tma_stratified_transitions") || inherits(x, "tma_model_bundle")) {
      x <- .tmaSelectStoredMatrix(x$models, stratum, "stratum")
    } else if (!is.null(stratum)) {
      stop("stratum is supported only for stratified fits or model bundles.", call. = FALSE)
    }
    if (!is.list(x$matrices) || !is.data.frame(x$periods) || !is.list(x$metadata)) {
      stop("x must be a transition model, model bundle or cohort projection.", call. = FALSE)
    }
    matrices <- if (is.null(stage)) x$matrices else
      .tmaSelectStoredMatrix(x$stageMatrices, stage, "stage")
    matrix <- .tmaSelectStoredMatrix(matrices, period, "period")
  }
  if (!is.matrix(matrix) || !is.numeric(matrix) || is.complex(matrix)) {
    stop("The selected object does not contain a numeric transition matrix.", call. = FALSE)
  }
  matrix
}

# Select stored definitions by exact name, without implicit list indexing or
# partial matches. The same rule applies to models, stages and periods.
.tmaSelectStoredMatrix <- function(values, selector, name) {
  labels <- names(values)
  if (!is.list(values) || is.data.frame(values) || !length(values) ||
      is.null(labels) || anyNA(labels) || anyDuplicated(labels) ||
      any(!nzchar(labels))) {
    stop("No uniquely named ", name, " definitions are available.", call. = FALSE)
  }
  if (is.null(selector)) {
    if (length(values) != 1L) {
      stop("Supply ", name, "; available names are: ", paste(labels, collapse = ", "), ".",
           call. = FALSE)
    }
    selector <- labels[1L]
  }
  if (!selector %in% labels) {
    stop("Unknown ", name, " '", selector, "'. Available names are: ",
         paste(labels, collapse = ", "), ".", call. = FALSE)
  }
  values[[selector]]
}
