#' Combine ordered transition stages into a cycle model
#'
#' @param stages A nonempty named list in execution order. Each element is a
#'   square numeric probability matrix with state names on both axes, or a named
#'   list containing one such matrix for every declared period. A single matrix
#'   applies in all periods. Every matrix must use the same state dictionary.
#' @param cycleLength Positive integer cycle length in days. Passing through all
#'   stages advances one cycle, regardless of the number of stages.
#' @param absorbingStates State labels that remain absorbing in every stage.
#'   No state is marked as structurally absorbing unless declared here.
#' @param periodBreaks Period boundaries in days from 0 to Inf. Finite boundaries
#'   must align with cycleLength. NULL uses one period for all follow-up.
#' @param periodLabels Optional unique period names. A stage supplied as a list
#'   must name its matrices with these period names.
#' @details The first stage acts on the current state distribution. Each later
#'   stage acts on the distribution produced by the preceding stage. The
#'   combined matrix is their ordered product. The period is selected from the
#'   source cycle's start day and is shared by all stages in that cycle.
#'
#'   These are specified conditional probabilities, not estimates from patient
#'   records. An ordinary full-cycle transition estimate does not become a
#'   within-cycle estimate by placing it in this list. The analyst must establish
#'   what each stage represents and whether the supplied probabilities apply to
#'   the intended population. If a later stage depends on earlier history, that
#'   history must remain represented in the states.
#'
#'   Stages can include reversible transitions. An identity row leaves a state
#'   unchanged during that stage without making it globally absorbing. Declared
#'   absorbing states must have exact identity rows in every stage. Entire NA
#'   rows represent unavailable probabilities. A starting state's combined row
#'   is NA when the stage sequence reaches an unavailable row. Unavailable rows on
#'   zero-probability paths do not affect the result. Probabilities are never
#'   normalised or filled in automatically.
#'
#'   The order is a model definition, not a reconstruction of event dates.
#'   Intermediate stages add no timepoints or person-time. No initial population
#'   is inferred from the matrices. Supply an explicit initial distribution or
#'   a separate entry model when calling projectCohort().
#' @return A transition-model list with matrices, matrix, periods, stageMatrices
#'   and metadata. matrices contains the combined matrix for each period. matrix
#'   contains that matrix only for a single-period model. stageMatrices retains
#'   the validated stage-by-period matrix lists in execution order. State order
#'   follows the first supplied matrix's row names. No observed counts, patient
#'   identifiers or estimated entry distribution are added.
#' @export
composeTransitionStages <- function(stages, cycleLength,
                                     absorbingStates = character(),
                                     periodBreaks = NULL, periodLabels = NULL) {
  if (missing(cycleLength)) {
    stop("Supply cycleLength explicitly for the complete sequence of stages.", call. = FALSE)
  }
  cycleLength <- .tmaProjectionInteger(cycleLength, "cycleLength", minimum = 1)
  periods <- .tmaPeriods(periodBreaks, periodLabels, cycleLength)
  if (!is.list(stages) || is.data.frame(stages) || !is.null(dim(stages)) ||
      !length(stages)) {
    stop("stages must be a nonempty named list in execution order.", call. = FALSE)
  }
  first <- stages[[1L]]
  if (is.list(first) && !is.data.frame(first) && is.null(dim(first)) && length(first)) {
    first <- first[[1L]]
  }
  if (!is.matrix(first) || !nrow(first) || is.null(rownames(first))) {
    stop("The first stage must supply a nonempty matrix with state names on both axes.", call. = FALSE)
  }
  dictionary <- .tmaStateDictionary(character(), rownames(first), absorbingStates)
  definition <- .tmaStageDefinition(stages, periods, dictionary$states,
                                     dictionary$absorbingStates)
  list(
    matrices = definition$matrices,
    matrix = if (nrow(periods) == 1L) definition$matrices[[1L]] else NULL,
    periods = periods,
    stageMatrices = definition$stageMatrices,
    metadata = list(
      states = dictionary$states,
      absorbingStates = dictionary$absorbingStates,
      cycleLength = cycleLength,
      estimator = "specified_stages",
      stage_order = names(definition$stageMatrices),
      probability_source = "analyst-supplied conditional stage probabilities",
      time_unit = "days",
      time_meaning = "one complete ordered stage sequence per cycle",
      period_assignment = "source cycle start, days since index_date",
      period_endpoints = "lower inclusive, upper exclusive",
      stage_timing = "ordered steps without additional timepoints or person-time",
      initialisation = "explicit initial distribution or separate entry model required"
    )
  )
}

# Callers validate the common periods and state dictionary before using this
# helper. Retain every stage so projection can inspect intermediate flows.
.tmaStageDefinition <- function(stages, periods, states, absorbingStates) {
  if (!is.list(stages) || is.data.frame(stages) || !is.null(dim(stages)) ||
      !length(stages) || is.null(names(stages)) || anyNA(names(stages)) ||
      any(!nzchar(trimws(names(stages)))) || anyDuplicated(names(stages))) {
    stop("stages must be a nonempty list with unique, nonempty names in execution order.", call. = FALSE)
  }
  stageMatrices <- vector("list", length(stages))
  names(stageMatrices) <- names(stages)
  for (i in seq_along(stages)) {
    supplied <- stages[[i]]
    if (is.matrix(supplied)) {
      supplied <- rep(list(supplied), nrow(periods))
      names(supplied) <- periods$period
    } else if (!is.list(supplied) || is.data.frame(supplied) || !is.null(dim(supplied)) ||
               length(supplied) != nrow(periods) || is.null(names(supplied)) ||
               anyNA(names(supplied)) || anyDuplicated(names(supplied)) ||
               !setequal(names(supplied), periods$period)) {
      stop("Stage '", names(stages)[i], "' must be a matrix or a named list with one matrix for every period.",
           call. = FALSE)
    }
    supplied <- supplied[periods$period]
    for (j in seq_len(nrow(periods))) {
      supplied[[j]] <- .tmaTransitionMatrix(
        supplied[[j]], states, absorbingStates,
        paste0("Stage '", names(stages)[i], "' in period '", periods$period[j], "'")
      )
    }
    stageMatrices[[i]] <- supplied
  }

  nStates <- length(states)
  matrices <- vector("list", nrow(periods))
  names(matrices) <- periods$period
  for (j in seq_len(nrow(periods))) {
    combined <- diag(nStates)
    dimnames(combined) <- list(states, states)
    for (i in seq_along(stageMatrices)) {
      stage <- stageMatrices[[i]][[j]]
      unavailable <- rowSums(is.na(stage)) > 0L
      for (origin in seq_len(nStates)) {
        current <- combined[origin, ]
        if (anyNA(current)) next
        occupied <- which(current > 0)
        if (any(unavailable[occupied])) {
          combined[origin, ] <- NA_real_
        } else {
          combined[origin, ] <- as.numeric(current[occupied] %*%
                                            stage[occupied, , drop = FALSE])
        }
      }
    }
    matrices[[j]] <- .tmaTransitionMatrix(
      combined, states, absorbingStates,
      paste0("Combined matrix for period '", periods$period[j], "'")
    )
  }
  list(stageMatrices = stageMatrices, matrices = matrices)
}
