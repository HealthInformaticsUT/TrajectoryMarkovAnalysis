#' Extend fitted transitions with an explicit future mortality assumption
#'
#' @param bundle A fitted model bundle without an existing extrapolation.
#' @param calibration A result from calibrateExcessMortality() for the same
#'   clinical states, source-cycle population, periods and estimation strata.
#'   Its retained background table supplies future background mortality.
#' @param fromDay Positive whole source day aligned with the cycle length.
#' @param referencePeriod One period label shared by the fitted model and
#'   calibration. Its bounds must also match. Every nonabsorbing state and
#'   estimation stratum must have supported estimates in this period.
#' @param excess Required assumption, either "constant" or "linear". Constant
#'   retains the calibrated excess hazard. Linear reduces it to zero over
#'   declineDays, starting at fromDay.
#' @param declineDays Positive finite decline duration in days, required only
#'   when excess is "linear". It need not align with a cycle boundary.
#' @param deathAllocation For several death states, an explicit named character
#'   vector mapping each nonabsorbing state to a death state, or a probability
#'   matrix with nonabsorbing states as rows and death states as columns. NULL
#'   is allowed when there is only one death state.
#' @param source Required nonblank label describing the future assumption.
#' @details Future mortality replaces the selected fitted row's total death
#'   probability. It is not added as another mortality stage. The relative
#'   probabilities of all nondeath destinations are retained conditional on
#'   survival. Other absorbing outcomes keep their identity rows.
#'
#'   Background mortality is matched separately to each current source profile.
#'   Age and other covariates use the simulator's usual updating rules. The
#'   source profile's background probability is held fixed within each cycle.
#'   For background cycle probability b, calibrated excess hazard e per day,
#'   and integrated decline weight A over the cycle, death probability is
#'   1 - (1 - b) * exp(-e * A). Constant excess uses A = cycleLength. Linear
#'   decline integrates max(1 - t / declineDays, 0) exactly over the cycle,
#'   where t is days since fromDay. Once the decline ends, only background
#'   mortality remains. Linear excess hazard does not imply a linear plot of
#'   death probabilities.
#'
#'   No missing state, stratum or reference period is filled automatically.
#'   Fit and calibrate from the same prepared histories. Matching aggregate
#'   counts cannot establish that different datasets contained the same cycles.
#'   This is an explicit extrapolation assumption, not a causal disease effect
#'   or an inferred ordering of events within a cycle. It does not forecast
#'   the background table or infer a future clinical pathway. The original
#'   costs, ageing, intervention and discount clocks are unchanged. Use
#'   simulateTrajectories() and advanceSimulation(); projectCohort() does not
#'   support this covariate-dependent future model.
#' @return A model bundle with a compact mortality extrapolation definition.
#'   Selected fitted probabilities, calibration support, background inputs,
#'   death allocation and decline assumptions are retained for review and
#'   continuation. No training patient histories are retained.
#' @export
extrapolateMortality <- function(bundle, calibration, fromDay, referencePeriod,
                                excess, declineDays = NULL,
                                deathAllocation = NULL, source) {
  if (inherits(bundle, "tma_mortality_bundle") || !is.null(bundle$extrapolation)) {
    stop("Supply an original fitted bundle without an existing extrapolation.", call. = FALSE)
  }
  bundle <- .tmaValidateModelBundle(bundle)
  if (missing(excess)) stop("Choose excess = 'constant' or 'linear' explicitly.", call. = FALSE)
  if (missing(source)) stop("source must be one nonblank analyst-supplied label.", call. = FALSE)
  reference <- bundle
  reference$costModel <- reference$costEstimates <- NULL
  reference$groups <- reference$groups[intersect(names(reference$groups), reference$metadata$strata)]
  reference <- .tmaValidateModelBundle(reference)
  future <- .tmaBuildFutureMortalityBundle(reference, calibration, referencePeriod,
    excess, declineDays, deathAllocation)
  bundle$extrapolation <- .tmaValidateBundleExtrapolation(
    list(fromDay = fromDay, source = source, bundle = future), bundle)
  bundle
}

.tmaMortalityEqual <- function(x, y) {
  is.numeric(x) && is.numeric(y) && length(x) == length(y) &&
    all(is.finite(x)) && all(is.finite(y)) &&
    all(abs(x - y) <= 1e-10 * pmax(1, abs(x), abs(y)))
}

.tmaMortalityCounts <- function(x, positive = FALSE) {
  is.numeric(x) && !is.complex(x) && !inherits(x, "integer64") &&
    is.null(dim(x)) && !anyNA(x) && all(is.finite(x)) &&
    all(x >= if (positive) 1 else 0) && all(x == trunc(x)) && all(x < 2^53)
}

.tmaFutureDeathAllocation <- function(allocation, living, deaths) {
  if (is.null(allocation)) {
    if (length(deaths) != 1L) {
      stop("deathAllocation must be supplied explicitly when there are several death states.", call. = FALSE)
    }
    return(matrix(1, length(living), 1L, dimnames = list(living, deaths)))
  }
  if (is.character(allocation) && is.null(dim(allocation))) {
    if (is.null(names(allocation)) || anyNA(allocation) || anyNA(names(allocation)) ||
        anyDuplicated(names(allocation)) || !setequal(names(allocation), living) ||
        any(!allocation %in% deaths)) {
      stop("Named deathAllocation must map every nonabsorbing state to one declared death state.", call. = FALSE)
    }
    result <- matrix(0, length(living), length(deaths), dimnames = list(living, deaths))
    result[cbind(seq_along(living), match(allocation[living], deaths))] <- 1
    return(result)
  }
  if (!is.matrix(allocation) || !is.numeric(allocation) || is.complex(allocation) ||
      inherits(allocation, "integer64") ||
      !identical(dim(allocation), c(length(living), length(deaths))) ||
      is.null(rownames(allocation)) || is.null(colnames(allocation)) ||
      anyDuplicated(rownames(allocation)) || anyDuplicated(colnames(allocation)) ||
      !setequal(rownames(allocation), living) || !setequal(colnames(allocation), deaths) ||
      anyNA(allocation) || any(!is.finite(allocation) | allocation < 0 | allocation > 1) ||
      any(abs(rowSums(allocation) - 1) > 1e-10)) {
    stop("deathAllocation must be a stochastic matrix with nonabsorbing state rows and death state columns.", call. = FALSE)
  }
  allocation[living, deaths, drop = FALSE]
}

.tmaBuildFutureMortalityBundle <- function(reference, calibration, referencePeriod,
                                          excess, declineDays, deathAllocation) {
  if (inherits(reference, "tma_mortality_bundle") || !is.null(reference$extrapolation) ||
      !is.null(reference$costModel) || !is.null(reference$costEstimates)) {
    stop("The future mortality reference must be an ordinary fitted bundle without costs or extrapolation.", call. = FALSE)
  }
  reference <- .tmaValidateModelBundle(reference)
  if (!is.character(referencePeriod) || length(referencePeriod) != 1L ||
      !is.null(dim(referencePeriod)) || is.na(referencePeriod) ||
      !referencePeriod %in% reference$metadata$periods$period) {
    stop("referencePeriod must name one fitted follow-up period.", call. = FALSE)
  }
  if (!is.character(excess) || length(excess) != 1L || !is.null(dim(excess)) ||
      is.na(excess) || !excess %in% c("constant", "linear")) {
    stop("excess must be 'constant' or 'linear'.", call. = FALSE)
  }
  if (excess == "constant" && !is.null(declineDays)) {
    stop("declineDays is used only with excess = 'linear'.", call. = FALSE)
  }
  if (excess == "linear" && (!is.numeric(declineDays) || is.complex(declineDays) ||
      inherits(declineDays, "integer64") || length(declineDays) != 1L ||
      !is.null(dim(declineDays)) || is.na(declineDays) ||
      !is.finite(declineDays) || declineDays <= 0)) {
    stop("Linear excess requires one positive finite declineDays duration.", call. = FALSE)
  }
  checked <- .tmaFutureCalibration(calibration, reference, referencePeriod)
  calibration <- checked$calibration
  living <- setdiff(reference$metadata$states, reference$metadata$absorbingStates)
  allocation <- .tmaFutureDeathAllocation(deathAllocation, living, calibration$metadata$deathStates)
  periods <- .tmaPeriods(NULL, "Future", reference$metadata$cycleLength)
  metadata <- list(states = reference$metadata$states,
    absorbingStates = reference$metadata$absorbingStates,
    cycleLength = reference$metadata$cycleLength, strata = reference$metadata$strata,
    periods = periods, selection = "source clinical covariates; separate current background lookup",
    estimator = "calibrated_future_mortality", clock = "days since extrapolation fromDay",
    mortality = "replace fitted death probability; preserve nondeath conditional shares",
    background = "source profile held fixed within each cycle",
    excess = excess, declineDays = declineDays,
    age = reference$metadata$age)
  structure(list(reference = reference, calibration = calibration,
    referencePeriod = referencePeriod, excess = excess, declineDays = declineDays,
    deathAllocation = allocation, parameters = checked$parameters,
    strata = reference$strata, groups = reference$groups,
    costModel = NULL, costEstimates = NULL, metadata = metadata),
    class = c("tma_mortality_bundle", "tma_model_bundle"))
}

.tmaValidateFutureMortalityBundle <- function(bundle) {
  required <- c("reference", "calibration", "referencePeriod", "excess", "declineDays",
    "deathAllocation", "parameters", "strata", "groups", "costModel", "costEstimates", "metadata")
  if (!inherits(bundle, "tma_mortality_bundle") || !is.list(bundle) ||
      anyDuplicated(names(bundle)) || !setequal(names(bundle), required)) {
    stop("Supply a future mortality bundle created by extrapolateMortality().", call. = FALSE)
  }
  rebuilt <- .tmaBuildFutureMortalityBundle(bundle$reference, bundle$calibration,
    bundle$referencePeriod, bundle$excess, bundle$declineDays, bundle$deathAllocation)
  for (name in c("parameters", "strata", "groups", "metadata", "costModel", "costEstimates")) {
    if (!identical(bundle[[name]], rebuilt[[name]])) {
      stop("Future mortality ", name, " do not match the retained model inputs. Recreate the extrapolation.", call. = FALSE)
    }
  }
  rebuilt
}

.tmaFutureCalibration <- function(calibration, reference, referencePeriod) {
  fields <- c("estimates", "reference", "strata", "periods", "background", "metadata")
  if (!inherits(calibration, "tma_excess_mortality") || !is.list(calibration) ||
      anyDuplicated(names(calibration)) || !setequal(names(calibration), fields) ||
      !is.list(calibration$metadata)) {
    stop("calibration must be a result from calibrateExcessMortality().", call. = FALSE)
  }
  meta <- calibration$metadata
  states <- reference$metadata$states
  absorbing <- reference$metadata$absorbingStates
  living <- setdiff(states, absorbing)
  columns <- reference$metadata$strata
  cycle <- reference$metadata$cycleLength
  if (!identical(meta$estimator, "source_cycle_moment_calibration") ||
      !setequal(meta$states, states) || !setequal(meta$absorbingStates, absorbing) ||
      !identical(meta$cycleLength, cycle) || !setequal(meta$strata, columns) ||
      !is.character(meta$deathStates) || !length(meta$deathStates) ||
      anyNA(meta$deathStates) || anyDuplicated(meta$deathStates) ||
      any(!meta$deathStates %in% absorbing) || !length(living) ||
      !identical(meta$excess_hazard_unit, "per day") ||
      !is.character(meta$negativeExcess) || length(meta$negativeExcess) != 1L ||
      is.na(meta$negativeExcess) || !meta$negativeExcess %in% c("error", "zero")) {
    stop("Calibration and fitted models must share states, absorbing states, cycleLength and estimation strata.", call. = FALSE)
  }
  registry <- calibration$strata
  if (!is.data.frame(registry) || anyDuplicated(names(registry)) ||
      !setequal(names(registry), c("stratum_id", columns)) ||
      !is.character(registry$stratum_id) || anyNA(registry$stratum_id) ||
      anyDuplicated(registry$stratum_id) || any(!nzchar(registry$stratum_id)) ||
      nrow(registry) != nrow(reference$strata)) {
    stop("Calibration strata must match the fitted covariate combinations exactly.", call. = FALSE)
  }
  ids <- .tmaBundleStratum(reference, registry)
  if (anyDuplicated(ids) || !setequal(ids, reference$strata$stratum_id)) {
    stop("Calibration strata must match the fitted covariate combinations exactly.", call. = FALSE)
  }
  periods <- calibration$periods
  if (!is.data.frame(periods) || !identical(names(periods), c("period", "start_day", "end_day")) ||
      !nrow(periods)) stop("Calibration periods are invalid.", call. = FALSE)
  checkedPeriods <- .tmaPeriods(c(periods$start_day, tail(periods$end_day, 1L)), periods$period, cycle)
  if (!isTRUE(all.equal(periods, checkedPeriods, check.attributes = FALSE)) ||
      !referencePeriod %in% periods$period) {
    stop("Calibration referencePeriod must have valid period bounds.", call. = FALSE)
  }
  a <- periods[periods$period == referencePeriod, , drop = FALSE]
  b <- reference$metadata$periods[reference$metadata$periods$period == referencePeriod, , drop = FALSE]
  if (!identical(a$start_day, b$start_day) || !identical(a$end_day, b$end_day)) {
    stop("Calibration and fitted referencePeriod must have the same bounds.", call. = FALSE)
  }
  calibration$background <- .tmaBackgroundMortalityBundle(calibration$background, cycle)
  bg <- calibration$background
  bgProb <- vapply(bg$models, function(x) x$metadata$mortality$cycle_probability, numeric(1))
  est <- calibration$estimates
  required <- c("stratum_id", "period", "from_state", "cycles", "patients", "deaths",
    "observed_probability", "expected_background_deaths", "background_probability",
    "raw_excess_hazard_per_day", "excess_hazard_per_day", "reconstructed_probability",
    "calibration_difference", "status")
  ref <- calibration$reference
  refRequired <- c("stratum_id", "period", "from_state", "background_stratum_id", "cycles",
    "patients", "deaths", "weight", "background_probability", "expected_background_deaths")
  if (!is.data.frame(est) || anyDuplicated(names(est)) || !setequal(names(est), required) ||
      !is.data.frame(ref) || anyDuplicated(names(ref)) || !setequal(names(ref), refRequired)) {
    stop("Calibration estimates and reference composition must retain their original fields.", call. = FALSE)
  }
  keys <- c("stratum_id", "period", "from_state")
  if (anyNA(est[keys]) || anyDuplicated(est[keys]) ||
      any(!est$stratum_id %in% registry$stratum_id) || any(!est$period %in% periods$period) ||
      any(!est$from_state %in% living) ||
      nrow(est) != nrow(registry) * nrow(periods) * length(living) ||
      anyNA(ref[c(keys, "background_stratum_id")]) ||
      anyDuplicated(ref[c(keys, "background_stratum_id")]) ||
      any(!ref$stratum_id %in% registry$stratum_id) || any(!ref$period %in% periods$period) ||
      any(!ref$from_state %in% living) || any(!ref$background_stratum_id %in% names(bgProb))) {
    stop("Calibration cells or background references are missing, duplicated or unknown.", call. = FALSE)
  }
  if (!all(vapply(est[c("cycles", "patients", "deaths")], .tmaMortalityCounts, logical(1))) ||
      !all(vapply(ref[c("cycles", "patients", "deaths")], .tmaMortalityCounts, logical(1))) ||
      any(est$patients > est$cycles | est$deaths > est$cycles) ||
      any(ref$cycles < 1 | ref$patients < 1 | ref$patients > ref$cycles | ref$deaths > ref$cycles)) {
    stop("Calibration support must contain valid cycle, patient and death counts.", call. = FALSE)
  }
  numericFields <- setdiff(required, c(keys, "cycles", "patients", "deaths", "status"))
  if (!all(vapply(est[numericFields], function(x) is.numeric(x) && !is.complex(x) &&
      !inherits(x, "integer64") && is.null(dim(x)), logical(1))) ||
      !all(vapply(ref[c("weight", "background_probability", "expected_background_deaths")],
        function(x) is.numeric(x) && !is.complex(x) && is.null(dim(x)), logical(1)))) {
    stop("Calibration probabilities, hazards and weights must be numeric.", call. = FALSE)
  }
  for (i in seq_len(nrow(est))) {
    cell <- est[i, , drop = FALSE]
    r <- ref[ref$stratum_id == cell$stratum_id & ref$period == cell$period &
      ref$from_state == cell$from_state, , drop = FALSE]
    if (!cell$cycles) {
      if (!identical(cell$status, "unestimated") || cell$patients != 0 || cell$deaths != 0 ||
          any(!is.na(unlist(cell[numericFields]))) || nrow(r)) {
        stop("Unsupported calibration cells must remain explicitly unestimated.", call. = FALSE)
      }
      next
    }
    if (!cell$status %in% c("calibrated", "floored") || cell$patients < 1 ||
        cell$deaths == cell$cycles || !nrow(r) ||
        sum(r$cycles) != cell$cycles || sum(r$deaths) != cell$deaths ||
        any(r$patients > cell$patients) || sum(r$patients) < cell$patients ||
        any(bgProb[r$background_stratum_id] >= 1)) {
      stop("Calibration support or reference composition is inconsistent.", call. = FALSE)
    }
    probs <- unname(bgProb[r$background_stratum_id])
    q <- cell$deaths / cell$cycles
    back <- sum(r$cycles * probs) / cell$cycles
    calculatedRaw <- (log1p(-back) - log1p(-q)) / cycle
    if (!.tmaMortalityEqual(cell$raw_excess_hazard_per_day, calculatedRaw)) {
      stop("Calibration excess hazard does not match the retained reference inputs.", call. = FALSE)
    }
    # The original mean and the equivalent grouped sum may round differently
    # near zero. Keep the validated recorded sign when checking an explicit floor.
    raw <- cell$raw_excess_hazard_per_day
    applied <- if (meta$negativeExcess == "zero") max(raw, 0) else raw
    reconstructed <- -expm1(log1p(-back) - applied * cycle)
    status <- if (raw < 0 && meta$negativeExcess == "zero") "floored" else "calibrated"
    expected <- c(q, sum(r$cycles * probs), back, raw, applied, reconstructed, reconstructed - q)
    if (!.tmaMortalityEqual(unlist(cell[numericFields], use.names = FALSE), expected) ||
        !identical(cell$status, status) ||
        !.tmaMortalityEqual(r$weight, r$cycles / cell$cycles) ||
        !.tmaMortalityEqual(r$background_probability, probs) ||
        !.tmaMortalityEqual(r$expected_background_deaths, r$cycles * probs)) {
      stop("Calibration hazards, probabilities or reference weights do not match the retained inputs.", call. = FALSE)
    }
  }
  parameters <- list()
  for (id in reference$strata$stratum_id) {
    model <- reference$models[[id]]
    if (!identical(model$metadata$estimator, "adjacent_cycles") ||
        !is.data.frame(model$estimates)) {
      stop("Future mortality requires fitted adjacent-cycle transition models.", call. = FALSE)
    }
    calibrationId <- registry$stratum_id[match(id, ids)]
    for (state in living) {
      cell <- est[est$stratum_id == calibrationId & est$period == referencePeriod &
        est$from_state == state, , drop = FALSE]
      if (cell$cycles == 0 || !is.finite(cell$excess_hazard_per_day) || cell$excess_hazard_per_day < 0) {
        stop("The selected referencePeriod requires supported, finite nonnegative excess for every nonabsorbing state and stratum.", call. = FALSE)
      }
      fitted <- model$estimates[model$estimates$period == referencePeriod &
        model$estimates$from_state == state, , drop = FALSE]
      if (!all(c("to_state", "count", "denominator", "probability", "structural") %in% names(fitted)) ||
          nrow(fitted) != length(states) || anyDuplicated(fitted$to_state) ||
          !setequal(fitted$to_state, states) ||
          !.tmaMortalityCounts(fitted$count) || !.tmaMortalityCounts(fitted$denominator, TRUE) ||
          anyNA(fitted$structural) || any(fitted$structural) ||
          any(fitted$denominator != cell$cycles) || sum(fitted$count) != cell$cycles ||
          sum(fitted$count[fitted$to_state %in% meta$deathStates]) != cell$deaths) {
        stop("Selected fitted counts and denominators do not match the calibration source-cycle support.", call. = FALSE)
      }
      row <- model$matrices[[referencePeriod]][state, ]
      if (anyNA(row) || !.tmaMortalityEqual(fitted$probability, fitted$count / cell$cycles) ||
          !.tmaMortalityEqual(fitted$probability, row[fitted$to_state]) ||
          !.tmaMortalityEqual(sum(row[meta$deathStates]), cell$observed_probability) ||
          sum(row[setdiff(states, meta$deathStates)]) <= 0) {
        stop("Selected fitted probabilities do not match calibration or have no supported nondeath outcome.", call. = FALSE)
      }
      parameters[[length(parameters) + 1L]] <- data.frame(stratum_id = id,
        calibration_stratum_id = calibrationId, from_state = state,
        observed_probability = cell$observed_probability,
        excess_hazard_per_day = cell$excess_hazard_per_day, stringsAsFactors = FALSE)
    }
  }
  list(calibration = calibration, parameters = do.call(rbind, parameters))
}

.tmaFutureMortalityRows <- function(tail, population, modelDays, ids) {
  states <- tail$metadata$states
  absorbing <- tail$metadata$absorbingStates
  deathStates <- tail$calibration$metadata$deathStates
  n <- nrow(population)
  if (!is.data.frame(population) || !"state_label" %in% names(population) ||
      length(modelDays) != n || length(ids) != n || !is.numeric(modelDays) ||
      anyNA(modelDays) || any(!is.finite(modelDays) | modelDays < 0) ||
      anyNA(population$state_label) || any(!population$state_label %in% states)) {
    stop("Future mortality needs known source states, current covariates and nonnegative model days.", call. = FALSE)
  }
  probabilities <- vector("list", n)
  audit <- data.frame(background_stratum_id = rep(NA_character_, n),
    background_death_probability = rep(NA_real_, n),
    excess_hazard_per_day = rep(NA_real_, n), integrated_excess_hazard = rep(NA_real_, n),
    mortality_probability = rep(NA_real_, n), stringsAsFactors = FALSE)
  active <- which(!population$state_label %in% absorbing)
  for (i in setdiff(seq_len(n), active)) {
    probabilities[[i]] <- stats::setNames(as.numeric(states == population$state_label[i]), states)
  }
  if (!length(active)) return(list(probabilities = probabilities, audit = audit))
  if (anyNA(ids[active]) || any(!ids[active] %in% tail$strata$stratum_id)) {
    stop("No clinical reference stratum supports a future mortality source row.", call. = FALSE)
  }
  background <- tail$calibration$background
  profiles <- addCovariateGroups(population[active, , drop = FALSE], background$groups)
  bgIds <- .tmaBundleStratum(background, profiles)
  bgProb <- vapply(background$models, function(x) x$metadata$mortality$cycle_probability, numeric(1))
  cycle <- tail$metadata$cycleLength
  duration <- rep(cycle, length(active))
  if (tail$excess == "linear") {
    remaining <- pmax(tail$declineDays - modelDays[active], 0)
    width <- pmin(cycle, remaining)
    # Integrate the two endpoint weights without squaring very large durations.
    duration <- width * ((remaining / tail$declineDays) +
      ((remaining - width) / tail$declineDays)) / 2
  }
  for (j in seq_along(active)) {
    i <- active[j]
    state <- as.character(population$state_label[i])
    parameter <- tail$parameters[tail$parameters$stratum_id == ids[i] &
      tail$parameters$from_state == state, , drop = FALSE]
    if (nrow(parameter) != 1L) stop("Future mortality calibration row is unavailable.", call. = FALSE)
    e <- parameter$excess_hazard_per_day
    b <- unname(bgProb[bgIds[j]])
    q <- -expm1(log1p(-b) - e * duration[j])
    original <- tail$reference$models[[ids[i]]]$matrices[[tail$referencePeriod]][state, ]
    nondeath <- setdiff(states, deathStates)
    row <- stats::setNames(numeric(length(states)), states)
    row[nondeath] <- (1 - q) * original[nondeath] / sum(original[nondeath])
    row[deathStates] <- q * tail$deathAllocation[state, deathStates]
    if (anyNA(row) || any(!is.finite(row) | row < 0 | row > 1) || abs(sum(row) - 1) > 1e-10) {
      stop("Future mortality produced an invalid probability row.", call. = FALSE)
    }
    probabilities[[i]] <- row
    audit$background_stratum_id[i] <- bgIds[j]
    audit$background_death_probability[i] <- b
    audit$excess_hazard_per_day[i] <- e
    audit$integrated_excess_hazard[i] <- e * duration[j]
    audit$mortality_probability[i] <- q
  }
  list(probabilities = probabilities, audit = audit)
}

.tmaFutureMortalityInfo <- function(tail) {
  list(reference = tail$reference, calibration = tail$calibration,
    referencePeriod = tail$referencePeriod, excess = tail$excess,
    declineDays = tail$declineDays, deathAllocation = tail$deathAllocation,
    parameters = tail$parameters, metadata = tail$metadata)
}
