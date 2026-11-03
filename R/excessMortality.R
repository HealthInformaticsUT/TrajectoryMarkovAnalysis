#' Calibrate excess mortality against the observed source-cycle population
#'
#' @param trajectories Lowercase C2T trajectories or a tma_trajectories object.
#'   Raw trajectories use deathStates as their absorbing states. To declare
#'   other absorbing outcomes or unobserved clinical states, prepare trajectories
#'   first. All declared absorbing outcomes terminate follow-up, but only
#'   deathStates count as deaths.
#' @param background A bundle from createModelBundle() containing only models
#'   from createMortalityTransitions() and optional covariate grouping rules.
#'   Its cycle length must match the trajectories. Background state labels need
#'   not match the clinical trajectory labels. Costs and extrapolation are not
#'   used and must not be attached to this reference bundle.
#' @param deathStates Nonempty character vector naming the death outcomes.
#'   These must be declared absorbing states in prepared trajectories.
#' @param cycleLength Positive integer cycle length in days. Required unless
#'   stored in prepared trajectories.
#' @param periodBreaks Period boundaries in days since each patient's index,
#'   using the same rules as estimateTransitions(). NULL pools all follow-up.
#' @param periodLabels Optional unique labels for the follow-up periods.
#' @param strata Optional trajectory columns defining estimation strata. These
#'   are separate from the background mortality strata and use the original
#'   source-row values, even when a background grouping rule has the same name.
#' @param negativeExcess How to handle observed mortality below its matched
#'   background expectation. "error" stops. "zero" explicitly floors the
#'   excess hazard at zero, warns, and retains the signed raw estimate and the
#'   resulting calibration difference.
#' @details This function uses exactly the adjacent, known pairs with a full
#'   source cycle that contribute to estimateTransitions(). Only nonabsorbing
#'   source states are calibrated. A first-cycle death has no observed preceding
#'   source state and does not contribute. Final censored cycles, gaps and
#'   unresolved pairs do not contribute either. Follow-up periods and estimation
#'   strata are assigned from the source row.
#'
#'   The background model is matched separately to each eligible source row.
#'   With birth_date, age is calculated at state_start_date using calendar
#'   birthdays. Otherwise any supplied age is used as that row's age. Grouping
#'   rules apply to this separate reference copy and do not change the original
#'   estimation strata. No age, sex, calendar trend or unsupported background
#'   stratum is inferred.
#'
#'   Within each estimation stratum, period and source state, let N be the
#'   eligible cycle count, D the number entering any deathState, q = D/N, and b
#'   the mean matched background cycle death probability. The raw excess hazard
#'   per day is [log(1 - b) - log(1 - q)] / cycleLength. Each eligible cycle has
#'   equal weight, so this reference describes the patients contributing to that
#'   particular estimate, not the baseline patient mix. This is an aggregate
#'   moment calibration, not an individual event-time likelihood fit.
#'
#'   For a common excess hazard e, the mean reconstructed death probability is
#'   1 - (1 - b) * exp(-e * cycleLength). Without flooring it reproduces q on the
#'   reference cycles. It need not reproduce the same aggregate probability in a
#'   future population with a different covariate distribution. A matched
#'   background probability of one or a supported cell in which every cycle
#'   ends in death cannot yield this finite-hazard calibration and is rejected.
#'
#'   The calculation does not correct informative censoring, establish a causal
#'   disease effect, provide confidence intervals or infer a future decline in
#'   excess mortality. It does not generate a transition model or alter existing
#'   all-cause transition estimates. It returns the calibrated inputs and their
#'   reference composition for a separately specified extrapolation model.
#' @return A tma_excess_mortality list with estimates, reference, strata, periods,
#'   background and metadata. estimates includes every nonabsorbing source state
#'   in every estimation stratum and period. Unsupported cells have status
#'   "unestimated" and NA probabilities and hazards. Supported cells have status
#'   "calibrated" or "floored". calibration_difference is the reconstructed
#'   minus observed death probability. reference reports counts and weights by
#'   background_stratum_id within each supported cell. Patient counts describe
#'   support within a cell and cannot be added across cells. The background
#'   bundle and its supplied source labels are retained, but patient identifiers
#'   and histories are not.
#' @export
calibrateExcessMortality <- function(trajectories, background, deathStates,
                                     cycleLength = NULL, periodBreaks = NULL,
                                     periodLabels = NULL, strata = NULL,
                                     negativeExcess = "error") {
  if (missing(deathStates) || !is.character(deathStates) ||
      !is.null(dim(deathStates)) || !length(deathStates) || anyNA(deathStates) ||
      any(!nzchar(trimws(deathStates))) || anyDuplicated(deathStates) ||
      any(deathStates %in% c("START", "EXIT", "OUT OF COHORT"))) {
    stop("deathStates must name one or more unique, nonempty clinical death states.",
         call. = FALSE)
  }
  if (!is.character(negativeExcess) || length(negativeExcess) != 1L ||
      !is.null(dim(negativeExcess)) || is.na(negativeExcess) ||
      !negativeExcess %in% c("error", "zero")) {
    stop("negativeExcess must be 'error' or 'zero'.", call. = FALSE)
  }
  args <- list(trajectories = trajectories)
  if (!missing(cycleLength)) args$cycleLength <- cycleLength
  if (inherits(trajectories, "tma_trajectories")) {
    if (!all(deathStates %in% trajectories$absorbingStates)) {
      stop("deathStates must be declared absorbing states in prepared trajectories.",
           call. = FALSE)
    }
  } else {
    args$absorbingStates <- deathStates
  }
  prepared <- do.call(prepareTrajectories, args)
  if (is.null(prepared$cycleLength)) {
    stop("cycleLength must be supplied explicitly or stored in prepared trajectories.",
         call. = FALSE)
  }
  cycleLength <- prepared$cycleLength
  background <- .tmaBackgroundMortalityBundle(background, cycleLength)
  periods <- .tmaPeriods(periodBreaks, periodLabels, cycleLength)
  data <- prepared$data
  grouping <- .tmaStrata(data, strata)
  registry <- if (is.null(grouping)) {
    data.frame(stratum_id = "s1", stringsAsFactors = FALSE)
  } else grouping$registry
  rowStratum <- if (is.null(grouping)) rep("s1", nrow(data)) else grouping$row_ids
  sourceStates <- setdiff(prepared$states, prepared$absorbingStates)
  support <- .tmaTransitionSupport(prepared, periods)
  eligible <- support$eligible &
    !data$state_label[support$source] %in% prepared$absorbingStates
  source <- support$source[eligible]
  target <- support$target[eligible]

  estimates <- expand.grid(from_state = sourceStates, period = periods$period,
    stratum_id = registry$stratum_id, stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE)[c("stratum_id", "period", "from_state")]
  for (field in c("cycles", "patients", "deaths")) {
    estimates[[field]] <- rep(0L, nrow(estimates))
  }
  numericFields <- c("observed_probability", "expected_background_deaths",
    "background_probability", "raw_excess_hazard_per_day", "excess_hazard_per_day",
    "reconstructed_probability", "calibration_difference")
  for (field in numericFields) estimates[[field]] <- rep(NA_real_, nrow(estimates))
  estimates$status <- rep("unestimated", nrow(estimates))
  reference <- data.frame(stratum_id = character(), period = character(),
    from_state = character(), background_stratum_id = character(),
    cycles = integer(), patients = integer(), deaths = integer(), weight = numeric(),
    background_probability = numeric(), expected_background_deaths = numeric(),
    stringsAsFactors = FALSE)

  if (length(source)) {
    referenceData <- addCovariateGroups(data[source, , drop = FALSE], background$groups,
      dateColumn = if ("birth_date" %in% names(data)) "state_start_date" else NULL)
    backgroundId <- .tmaBundleStratum(background, referenceData)
    backgroundProbabilities <- vapply(background$models, function(model) {
      model$metadata$mortality$cycle_probability
    }, numeric(1))
    probability <- unname(backgroundProbabilities[backgroundId])
    if (any(probability == 1)) {
      stop("A matched background cycle probability is one. Finite excess-hazard calibration requires probabilities below one.",
           call. = FALSE)
    }
    death <- data$state_label[target] %in% deathStates
    cell <- (match(rowStratum[source], registry$stratum_id) - 1L) *
      nrow(periods) * length(sourceStates) +
      (support$periodIndex[source] - 1L) * length(sourceStates) +
      match(data$state_label[source], sourceStates)
    rowsByCell <- split(seq_along(source), cell)
    referenceRows <- list()
    for (cellName in names(rowsByCell)) {
      i <- as.integer(cellName)
      rows <- rowsByCell[[cellName]]
      cycles <- length(rows)
      deaths <- sum(death[rows])
      if (deaths == cycles) {
        stop("Every eligible cycle ends in death for stratum '", estimates$stratum_id[i],
             "', period '", estimates$period[i], "', state '", estimates$from_state[i],
             "'. A finite excess hazard cannot be calibrated.", call. = FALSE)
      }
      observed <- deaths / cycles
      meanBackground <- mean(probability[rows])
      rawExcess <- (log1p(-meanBackground) - log1p(-observed)) / cycleLength
      estimates$cycles[i] <- cycles
      estimates$patients[i] <- length(unique(data$subject_id[source[rows]]))
      estimates$deaths[i] <- deaths
      estimates$observed_probability[i] <- observed
      estimates$expected_background_deaths[i] <- sum(probability[rows])
      estimates$background_probability[i] <- meanBackground
      estimates$raw_excess_hazard_per_day[i] <- rawExcess
      estimates$excess_hazard_per_day[i] <- if (negativeExcess == "zero") max(rawExcess, 0) else rawExcess
      estimates$reconstructed_probability[i] <- -expm1(log1p(-meanBackground) -
        estimates$excess_hazard_per_day[i] * cycleLength)
      estimates$calibration_difference[i] <- estimates$reconstructed_probability[i] - observed
      estimates$status[i] <- if (rawExcess < 0 && negativeExcess == "zero") "floored" else "calibrated"
      for (id in unique(backgroundId[rows])) {
        matched <- rows[backgroundId[rows] == id]
        referenceRows[[length(referenceRows) + 1L]] <- data.frame(
          stratum_id = estimates$stratum_id[i], period = estimates$period[i],
          from_state = estimates$from_state[i], background_stratum_id = id,
          cycles = length(matched),
          patients = length(unique(data$subject_id[source[matched]])),
          deaths = sum(death[matched]), weight = length(matched) / cycles,
          background_probability = unname(backgroundProbabilities[id]),
          expected_background_deaths = sum(probability[matched]),
          stringsAsFactors = FALSE)
      }
    }
    reference <- do.call(rbind, referenceRows)
    negative <- which(estimates$raw_excess_hazard_per_day < 0)
    if (length(negative) && negativeExcess == "error") {
      i <- negative[1L]
      stop("Observed mortality is below matched background mortality for stratum '",
           estimates$stratum_id[i], "', period '", estimates$period[i], "', state '",
           estimates$from_state[i], "'. Review the reference or explicitly use negativeExcess = 'zero'.",
           call. = FALSE)
    }
    if (length(negative)) {
      warning("Excess mortality was floored at zero in ", length(negative),
              " supported cell(s). Signed raw estimates and calibration differences are retained.",
              call. = FALSE)
    }
  }
  rownames(estimates) <- rownames(reference) <- NULL
  structure(list(estimates = estimates, reference = reference, strata = registry,
    periods = periods, background = background,
    metadata = list(states = prepared$states, absorbingStates = prepared$absorbingStates,
      deathStates = unname(deathStates), cycleLength = cycleLength,
      strata = if (is.null(grouping)) character() else grouping$columns,
      negativeExcess = negativeExcess, estimator = "source_cycle_moment_calibration",
      excess_hazard_unit = "per day",
      denominator = "observed adjacent known pairs with a full nonabsorbing source cycle",
      stratum_assignment = "original source-row values; pairs formed before grouping",
      period_assignment = "source cycle start, days since index_date",
      background_matching = "source-row covariates; separate background grouping rules",
      age = "completed years at state_start_date when birth_date exists; otherwise supplied row age",
      reference_weighting = "equal weights for eligible source cycles within each calibration cell",
      calibration_target = "observed cycle death probability on the matched reference population")),
    class = "tma_excess_mortality")
}
