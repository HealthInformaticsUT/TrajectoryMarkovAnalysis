#' Turn estimated daily costs into an explicit simulation cost model
#'
#' @param costs A result from estimateStateCosts(). Only selected aggregate
#'   estimates and their definitions are retained, never patient records.
#' @param unit Explicitly choose "day" or "cycle". Daily rates are integrated
#'   over each full labelled cycle, splitting at cost-period boundaries while
#'   holding that snapshot's state and covariates fixed. Cycle amounts multiply
#'   the estimated daily mean by cycleLength and require aligned boundaries.
#' @param currency One explicit currency label. Its association with the source
#'   currency concept is the user's assertion; no conversion is performed.
#' @param priceYear Reference price year, with no inflation adjustment. Must
#'   match the target year if adjustCostPrices() was used before estimation.
#'   Without that provenance, this remains the analyst's assertion.
#' @param terminal Explicitly choose "entry" or "none". Entry charges one full
#'   labelled interval on an observed simulation transition into a terminal
#'   state. Initial terminal patients and later terminal residency are skipped.
#'   None skips all terminal labels. Any fitted terminalCostMap is retained.
#' @param cycleLength Positive integer days, required only when unit = "cycle".
#' @param currencyConceptId Optional single source currency code. NULL is allowed
#'   only when the estimates contain exactly one known, nonblank, nonzero code.
#'   Select NA_character_ explicitly for missing currency; blank and "0" codes
#'   must also be selected explicitly and are never pooled with another code.
#' @details This is an explicit modelling assumption linking a descriptive
#'   observed-day mean to a full labelled simulation interval. It is not a
#'   reconstruction of observed event timing or individual expenditure. Missing
#'   estimated cells are errors when requested, never zero-cost defaults.
#'
#'   Mapped terminal states must be declared terminal when simulating. Their
#'   entry charge uses the mapped cost state and the supplied destination
#'   covariates, including covariates frozen on terminal entry. With
#'   terminal = "none", fitted rates still include any attributed terminal
#'   amounts and days, although simulated terminal labels are skipped; this is
#'   not an exact replay of the estimation data. Absorbing states not declared
#'   terminal receive ordinary ongoing charges. No intervention effect,
#'   discounting, uncertainty or additional within-cycle events are inferred.
#'   Any supplied price-adjustment provenance is retained and passed to the
#'   simulation. Adjust individual records before estimation, not pooled rates.
#' @return A tma_cost_model with rates, periods and explicit unit, currency,
#'   price-year, terminal and covariate definitions in metadata.
#' @export
createCostModel <- function(costs, unit, currency, priceYear, terminal,
                            cycleLength = NULL, currencyConceptId = NULL) {
  if (missing(unit) || missing(currency) || missing(priceYear) || missing(terminal)) {
    stop("Supply explicit unit, currency, priceYear and terminal choices.", call. = FALSE)
  }
  if (!inherits(costs, "tma_state_costs") || !is.data.frame(costs$estimates) ||
      !is.list(costs$metadata)) {
    stop("costs must be a result from estimateStateCosts().", call. = FALSE)
  }
  if (!is.character(unit) || length(unit) != 1L || is.na(unit) ||
      !unit %in% c("day", "cycle")) {
    stop("unit must explicitly be 'day' or 'cycle'.", call. = FALSE)
  }
  if (unit == "cycle") {
    cycleLength <- .tmaProjectionInteger(cycleLength, "cycleLength", 1)
  } else if (!is.null(cycleLength)) {
    stop("Do not supply cycleLength for a daily cost model; simulation supplies the interval length.",
         call. = FALSE)
  }
  strata <- costs$metadata$strata
  if (is.null(strata)) strata <- character()
  required <- c(strata, "period", "cost_state", "currency_concept_id", "observed_days",
                "mean_cost_per_day", "measure")
  estimates <- as.data.frame(costs$estimates)
  if (anyDuplicated(names(estimates)) || !all(required %in% names(estimates)) || !nrow(estimates)) {
    stop("Cost estimates must contain nonempty aggregate state, period, currency, observed-day and rate columns.",
         call. = FALSE)
  }
  sourceCurrency <- estimates$currency_concept_id
  if (is.factor(sourceCurrency)) sourceCurrency <- as.character(sourceCurrency)
  if (!is.character(sourceCurrency)) {
    stop("Estimated currency_concept_id must contain character source currency codes.", call. = FALSE)
  }
  currencies <- unique(sourceCurrency)
  if (is.null(currencyConceptId)) {
    if (length(currencies) != 1L || is.na(currencies) ||
        !nzchar(trimws(currencies)) || trimws(currencies) == "0") {
      stop("Select one currencyConceptId explicitly; use NA_character_ for missing currency. Currencies are never combined.",
           call. = FALSE)
    }
    currencyConceptId <- currencies
  }
  if (!is.character(currencyConceptId) || length(currencyConceptId) != 1L ||
      !is.null(dim(currencyConceptId))) {
    stop("currencyConceptId must be one character code or NA_character_.", call. = FALSE)
  }
  selected <- if (is.na(currencyConceptId)) is.na(sourceCurrency) else {
    !is.na(sourceCurrency) & sourceCurrency == currencyConceptId
  }
  if (!any(selected)) stop("No estimates match the selected currencyConceptId.", call. = FALSE)
  rates <- estimates[selected, required, drop = FALSE]
  for (column in names(rates)) {
    if (is.factor(rates[[column]])) rates[[column]] <- as.character(rates[[column]])
  }
  .tmaCostAmounts(rates$mean_cost_per_day, "Selected mean_cost_per_day")
  rates$unit_cost <- rates$mean_cost_per_day * if (unit == "cycle") cycleLength else 1
  rownames(rates) <- NULL
  terminalMap <- costs$metadata$terminal_cost_map
  if (is.null(terminalMap)) terminalMap <- character()
  model <- structure(list(rates = rates, periods = costs$periods,
    metadata = list(strata = strata, currency = currency, priceYear = priceYear,
      unit = unit, cycleLength = cycleLength, terminal = terminal,
      source_currency_concept_id = unname(currencyConceptId),
      price_adjustment = costs$metadata$price_adjustment,
      allocation = costs$metadata$allocation,
      terminal_cost_map = terminalMap, measure = costs$metadata$measure,
      cost_scope = costs$metadata$cost_scope,
      source_visit_filter = costs$metadata$source_visit_filter,
      timing = "full labelled interval using destination state and covariates",
      calibration = "explicit use of the descriptive mean per observed day",
      conversion = "none", discounting = "none")), class = "tma_cost_model")
  .tmaValidateCostModel(model)
}

.tmaValidateCostModel <- function(model, bundle = NULL) {
  if (!inherits(model, "tma_cost_model") || !is.list(model$metadata) ||
      !is.data.frame(model$rates) || !is.data.frame(model$periods)) {
    stop("Supply a cost model created by createCostModel().", call. = FALSE)
  }
  meta <- model$metadata
  for (field in c("currency", "measure", "cost_scope")) {
    value <- meta[[field]]
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !nzchar(trimws(value))) {
      stop("Cost model ", field, " must be one nonempty label.", call. = FALSE)
    }
  }
  meta$priceYear <- .tmaProjectionInteger(meta$priceYear, "Cost model priceYear", 1)
  .tmaValidatePriceBasis(meta$price_adjustment, meta$priceYear, meta$source_currency_concept_id)
  if (!is.character(meta$unit) || length(meta$unit) != 1L || is.na(meta$unit) ||
      !meta$unit %in% c("day", "cycle")) {
    stop("Cost model unit must be 'day' or 'cycle'.", call. = FALSE)
  }
  if (!is.character(meta$terminal) || length(meta$terminal) != 1L || is.na(meta$terminal) ||
      !meta$terminal %in% c("entry", "none")) {
    stop("Cost model terminal must explicitly be 'entry' or 'none'.", call. = FALSE)
  }
  if (meta$unit == "cycle") {
    meta$cycleLength <- .tmaProjectionInteger(meta$cycleLength, "Cost model cycleLength", 1)
  } else if (!is.null(meta$cycleLength)) {
    stop("A daily cost model must not fix cycleLength.", call. = FALSE)
  }
  if (!is.character(meta$source_currency_concept_id) ||
      length(meta$source_currency_concept_id) != 1L) {
    stop("Cost model must retain one selected source currency concept, including explicit NA.", call. = FALSE)
  }
  strata <- meta$strata
  if (!is.character(strata) || anyNA(strata) || anyDuplicated(strata)) {
    stop("Cost model strata must be a character vector of unique column names.", call. = FALSE)
  }
  ledgerFields <- c("simulation_id", "cycle", "elapsed_day", "component", "type", "state_label",
                    "amount", "cost_period", "cost_state", "cost_unit", "unit_cost", "quantity",
                    "source_currency_concept_id")
  if (any(strata %in% ledgerFields)) {
    stop("Cost strata cannot replace charge-ledger fields: ",
         paste(intersect(strata, ledgerFields), collapse = ", "), ".", call. = FALSE)
  }
  rates <- model$rates
  fields <- c(strata, "period", "cost_state", "currency_concept_id", "observed_days",
               "mean_cost_per_day", "unit_cost", "measure")
  if (!nrow(rates) || anyDuplicated(names(rates)) || !setequal(names(rates), fields)) {
    stop("Cost model rates must contain only the selected aggregate rate fields and covariates.", call. = FALSE)
  }
  if (length(strata)) .tmaStrata(rates, strata)
  for (field in c("period", "cost_state", "measure")) {
    if (!is.character(rates[[field]]) || anyNA(rates[[field]]) ||
        any(!nzchar(trimws(rates[[field]])))) {
      stop("Cost model ", field, " must contain nonempty labels.", call. = FALSE)
    }
  }
  .tmaStateDictionary(unique(rates$cost_state), unique(rates$cost_state), character())
  if (any(rates$measure != meta$measure)) stop("Cost model measures must not be mixed.", call. = FALSE)
  if (!is.character(rates$currency_concept_id) ||
      any(if (is.na(meta$source_currency_concept_id)) !is.na(rates$currency_concept_id) else {
        is.na(rates$currency_concept_id) | rates$currency_concept_id != meta$source_currency_concept_id
      })) {
    stop("Cost model rates must use only the selected source currency concept.", call. = FALSE)
  }
  for (field in c("observed_days", "mean_cost_per_day", "unit_cost")) {
    .tmaCostAmounts(rates[[field]], paste0("Cost model ", field))
  }
  if (any(rates$observed_days <= 0)) stop("Cost estimates need positive observed_days.", call. = FALSE)
  expected <- rates$mean_cost_per_day * if (meta$unit == "cycle") meta$cycleLength else 1
  if (any(!is.finite(expected)) || any(rates$unit_cost != expected)) {
    stop("Cost model unit_cost must equal the finite daily rate times the declared unit length.", call. = FALSE)
  }
  if (anyDuplicated(rates[c(strata, "period", "cost_state")])) {
    stop("Cost model contains duplicate state, period and covariate keys.", call. = FALSE)
  }
  periods <- model$periods
  if (!all(c("period", "start_day", "end_day") %in% names(periods)) ||
      anyDuplicated(names(periods)) || !nrow(periods) ||
      !is.numeric(periods$end_day) || is.complex(periods$end_day) || anyNA(periods$end_day)) {
    stop("Cost model periods must describe contiguous intervals from day 0 to Inf.", call. = FALSE)
  }
  checked <- .tmaPeriods(c(periods$start_day, tail(periods$end_day, 1L)), periods$period,
                         cycleLength = if (meta$unit == "cycle") meta$cycleLength else NULL)
  if (any(periods$end_day != checked$end_day) || any(!rates$period %in% checked$period)) {
    stop("Cost rate periods must match contiguous cost-model periods.", call. = FALSE)
  }
  mapping <- meta$terminal_cost_map
  if (!is.character(mapping) || !is.null(dim(mapping)) || anyNA(mapping) ||
      any(!nzchar(trimws(mapping))) || (length(mapping) &&
        (is.null(names(mapping)) || anyNA(names(mapping)) ||
         any(!nzchar(trimws(names(mapping)))) || anyDuplicated(names(mapping))))) {
    stop("Cost model terminal_cost_map must have unique source names and nonempty destination labels.",
         call. = FALSE)
  }
  if (!is.null(bundle)) {
    states <- bundle$metadata$states
    absorbing <- bundle$metadata$absorbingStates
    if (any(!rates$cost_state %in% states) || any(!names(mapping) %in% absorbing) ||
        any(!mapping %in% states | mapping %in% absorbing)) {
      stop("Cost states must match bundle states; terminal mapping sources must be declared absorbing and targets nonabsorbing states.",
           call. = FALSE)
    }
    if (meta$unit == "cycle" && meta$cycleLength != bundle$metadata$cycleLength) {
      stop("Cost model cycleLength must equal the transition bundle cycleLength.", call. = FALSE)
    }
  }
  model$rates <- rates
  model$periods <- checked
  model$metadata <- meta
  model
}

.tmaModelCostCharge <- function(model, population, previous = NULL,
                                terminalStates = character(), cycleLength) {
  meta <- model$metadata
  cycleLength <- .tmaProjectionInteger(cycleLength, "Cost interval cycleLength", 1)
  if (meta$unit == "cycle" && cycleLength != meta$cycleLength) {
    stop("Cost model cycleLength must equal the simulated cycleLength.", call. = FALSE)
  }
  if (any(!names(meta$terminal_cost_map) %in% terminalStates)) {
    stop("All terminal_cost_map source states must be declared in terminalStates when simulating.", call. = FALSE)
  }
  ledger <- data.frame(simulation_id = character(), cycle = numeric(), elapsed_day = numeric(),
    component = character(), type = character(), state_label = character(), amount = numeric(),
    cost_period = character(), cost_state = character(), cost_unit = character(),
    unit_cost = numeric(), quantity = numeric(), source_currency_concept_id = character(),
    stringsAsFactors = FALSE)
  for (column in meta$strata) ledger[[column]] <- model$rates[[column]][FALSE]
  isTerminal <- population$state_label %in% terminalStates
  charge <- !isTerminal
  newlyTerminal <- rep(FALSE, nrow(population))
  if (!is.null(previous) && meta$terminal == "entry") {
    prior <- match(population$simulation_id, previous$simulation_id)
    if (anyNA(prior) || anyDuplicated(previous$simulation_id)) {
      stop("Previous cost snapshot must identify each current simulation_id exactly once.", call. = FALSE)
    }
    newlyTerminal <- isTerminal & !previous$state_label[prior] %in% terminalStates
    charge <- charge | newlyTerminal
  }
  selected <- which(charge)
  if (!length(selected)) return(ledger)
  if (length(meta$strata)) .tmaStrata(population[selected, , drop = FALSE], meta$strata)
  for (column in meta$strata) {
    if (is.factor(population[[column]])) population[[column]] <- as.character(population[[column]])
    source <- model$rates[[column]]
    current <- population[[column]]
    if (!(is.numeric(source) && is.numeric(current)) &&
        !identical(typeof(source), typeof(current))) {
      stop("Cost covariate '", column, "' must have the same type as its fitted values.", call. = FALSE)
    }
  }
  start <- population$elapsed_day[selected]
  end <- start + cycleLength
  if (any(!is.finite(end) | end >= 2^53)) {
    stop("Cost interval exceeds exact elapsed-day indexing.", call. = FALSE)
  }
  costState <- as.character(population$state_label[selected])
  mapped <- newlyTerminal[selected] & costState %in% names(meta$terminal_cost_map)
  costState[mapped] <- unname(meta$terminal_cost_map[costState[mapped]])
  startPeriod <- .tmaPeriodIndex(start, model$periods)
  if (meta$unit == "cycle" && any(end > model$periods$end_day[startPeriod])) {
    stop("A full-cycle cost interval cannot cross a cost-period boundary; align its elapsed_day or use unit = 'day'.",
         call. = FALSE)
  }
  pieces <- list()
  for (period in seq_len(nrow(model$periods))) {
    rows <- which(start < model$periods$end_day[period] & end > model$periods$start_day[period])
    if (!length(rows)) next
    item <- population[selected[rows], c("simulation_id", "cycle", "elapsed_day",
                                         "state_label", meta$strata), drop = FALSE]
    item$simulation_id <- as.character(item$simulation_id)
    item$cycle <- as.numeric(item$cycle)
    item$elapsed_day <- as.numeric(item$elapsed_day)
    item$state_label <- as.character(item$state_label)
    item$component <- "healthcare"
    item$type <- "estimated"
    item$cost_period <- model$periods$period[period]
    item$cost_state <- costState[rows]
    item$cost_unit <- meta$unit
    item$quantity <- if (meta$unit == "cycle") rep(1, length(rows)) else {
      pmin(end[rows], model$periods$end_day[period]) - pmax(start[rows], model$periods$start_day[period])
    }
    item$source_currency_concept_id <- meta$source_currency_concept_id
    pieces[[length(pieces) + 1L]] <- item
  }
  query <- do.call(rbind, pieces)
  rates <- model$rates[c(meta$strata, "period", "cost_state", "unit_cost")]
  keys <- c(stats::setNames(meta$strata, meta$strata), cost_period = "period", cost_state = "cost_state")
  result <- as.data.frame(dplyr::left_join(query, rates, by = keys))
  if (nrow(result) != nrow(query)) stop("Cost model contains duplicate lookup keys.", call. = FALSE)
  absent <- which(is.na(result$unit_cost))
  if (length(absent)) {
    row <- absent[1L]
    covariates <- if (length(meta$strata)) paste(vapply(meta$strata, function(column) {
      paste0(column, "=", as.character(result[[column]][row]))
    }, character(1)), collapse = ", ") else "pooled covariates"
    stop("No unique cost estimate for state '", result$cost_state[row], "', period '",
         result$cost_period[row], "', ", covariates, " (", result$simulation_id[row], ").",
         call. = FALSE)
  }
  result$amount <- result$unit_cost * result$quantity
  if (any(!is.finite(result$amount)) || !is.finite(sum(result$amount))) {
    stop("Estimated costs exceed finite numeric limits.", call. = FALSE)
  }
  ordering <- order(match(result$simulation_id, population$simulation_id),
                     match(result$cost_period, model$periods$period))
  ledger <- result[ordering, names(ledger), drop = FALSE]
  rownames(ledger) <- NULL
  ledger
}
