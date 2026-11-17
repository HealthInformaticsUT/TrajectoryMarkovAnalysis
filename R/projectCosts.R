#' Calculate costs from projected cycle labels and state changes
#'
#' @param projection A result from projectCohort(), including expected flows.
#' @param stateCosts Named numeric costs per patient per full cycle label, or a
#'   data frame with state and cost columns. An optional period column refers to
#'   the cost periods defined here. Without period, costs apply to every period.
#'   When supplied, every state and cost period must be covered, including zeros.
#' @param transitionCosts Optional data frame with from_state, to_state and cost
#'   columns, and optionally period. Each rule charges the represented state
#'   change, including changes during the entry step. Omitted pairs have no
#'   additional charge. Self-pairs are not allowed. Costs are additive to state
#'   costs and do not imply intermediate events that are absent from the model.
#'   These rules charge net cycle outcomes only. They do not use stageFlows from
#'   a composed model. A path A to B to A has two stage changes but no net change.
#' @param entryCost Optional scalar startup cost per projected patient, charged
#'   once in cycle zero. Requires a projection with an explicit entry model.
#' @param currency One currency label, such as "EUR". No conversion is performed.
#' @param priceYear Reference price year for every supplied cost. No inflation
#'   adjustment is performed.
#' @param periodBreaks Optional cost-period boundaries in days since index, from
#'   0 to Inf. Finite boundaries must align with the cycle length. These periods
#'   are independent of the source periods used for transition probabilities.
#' @param periodLabels Optional names for the cost periods.
#' @details Charges follow the destination cycle label's start day. Recurring
#'   state costs apply to every reported label, including cycle zero. The
#'   separate pre-entry baseline does not receive a recurring charge. A transition
#'   rule has the same meaning in entry and adjacent steps, although its amount
#'   can vary by cost period. The scalar entry fee applies to cohortSize once,
#'   not to the number of patients used to estimate the model.
#'
#'   NULL explicitly omits a component. Missing or nonfinite supplied amounts
#'   are errors, not zeros. Negative amounts can represent credits. At least one
#'   component must be supplied. Nonzero state costs on absorbing states recur
#'   in later cycles. Set those costs to zero when using a one-time terminal-care
#'   charge. Without an entry model, initial occupancy does not create an entry
#'   event or terminal-care charge.
#'
#'   State charges are stipulated amounts per full cycle label, not daily rates
#'   or observed person-time estimates. They are not prorated for shortened
#'   observed cycles and are not automatically calibrated from estimateStateCosts().
#'   Intermediate stages receive no extra recurring charges or person-time.
#'   Stage-specific event costing is not implemented by this function.
#'   For nCycles = k, labels 0 through k receive charges, representing k + 1
#'   labelled cycles. No discounting, utilities, random sampling or database
#'   operations are performed.
#' @return A list with byCycle, totals, details, costRules and metadata. Costs are
#'   expected cohort totals, with per-person totals also reported. Details retain
#'   probability periods separately from cost periods. No patient identifiers,
#'   files or plots are returned or created.
#' @export
projectCosts <- function(projection, stateCosts = NULL, transitionCosts = NULL,
                          entryCost = NULL, currency, priceYear,
                          periodBreaks = NULL, periodLabels = NULL) {
  projection <- .tmaCostProjection(projection)
  model <- projection$metadata
  if (missing(currency) || !is.character(currency) || length(currency) != 1L ||
      is.na(currency) || !nzchar(trimws(currency))) {
    stop("Supply one explicit currency label, such as 'EUR'.", call. = FALSE)
  }
  if (missing(priceYear)) stop("Supply the reference priceYear for these costs.", call. = FALSE)
  priceYear <- .tmaProjectionInteger(priceYear, "priceYear", 1)
  if (is.null(stateCosts) && is.null(transitionCosts) && is.null(entryCost)) {
    stop("Supply at least one cost component. NULL explicitly omits a component.", call. = FALSE)
  }
  periods <- .tmaPeriods(periodBreaks, periodLabels)
  if (any(periods$start_day %% model$cycleLength != 0)) {
    stop("Cost periodBreaks must align with cycleLength. Full-cycle charges are not prorated.", call. = FALSE)
  }
  if ((model$nCycles + 1) * model$cycleLength >= 2^53) {
    stop("The charged horizon exceeds exact elapsed-day indexing.", call. = FALSE)
  }
  stateRules <- .tmaProjectedCostRules(stateCosts, "state", model$states, periods, "stateCosts")
  transitionRules <- .tmaProjectedCostRules(transitionCosts, c("from_state", "to_state"),
                                           model$states, periods, "transitionCosts")
  if (any(transitionRules$from_state == transitionRules$to_state)) {
    stop("transitionCosts cannot contain self-pairs. Use stateCosts for recurring care or entryCost for a startup fee.", call. = FALSE)
  }
  if (!is.null(entryCost)) {
    .tmaCostAmounts(entryCost, "entryCost")
    if (length(entryCost) != 1L) stop("entryCost must be one cost per projected patient.", call. = FALSE)
    if (is.null(projection$entry)) {
      stop("entryCost requires a projection with an explicit entry model.", call. = FALSE)
    }
    entryCost <- unname(entryCost)
  }

  stateLedger <- projection$occupancy[c("cycle", "elapsed_day", "state", "expected_count")]
  stateLedger$cost_period <- periods$period[.tmaPeriodIndex(stateLedger$elapsed_day, periods)]
  stateLedger <- .tmaApplyCostRules(stateLedger, stateRules, c("state", "cost_period"))
  transitionLedger <- projection$flows[c("stage", "source_cycle", "source_day",
                                          "destination_cycle", "destination_day", "period",
                                          "from_state", "to_state", "transition_probability",
                                          "probability_mass", "expected_count")]
  names(transitionLedger)[names(transitionLedger) == "period"] <- "probability_period"
  transitionLedger$cost_period <- periods$period[.tmaPeriodIndex(transitionLedger$destination_day, periods)]
  transitionLedger <- .tmaApplyCostRules(transitionLedger, transitionRules,
                                         c("from_state", "to_state", "cost_period"))
  entryLedger <- data.frame(cycle = integer(), elapsed_day = numeric(),
                             cost_period = character(), expected_count = numeric(),
                             unit_cost = numeric(), expected_cost = numeric())
  if (!is.null(entryCost)) {
    entryLedger <- data.frame(cycle = 0L, elapsed_day = 0, cost_period = periods$period[1L],
                               expected_count = model$cohortSize, unit_cost = unname(entryCost),
                               expected_cost = model$cohortSize * unname(entryCost))
  }
  cycles <- seq.int(0, model$nCycles)
  sumByCycle <- function(amounts, cycle) {
    total <- numeric(length(cycles))
    if (length(amounts)) {
      sums <- rowsum(amounts, cycle, reorder = FALSE)
      total[as.numeric(rownames(sums)) + 1L] <- sums[, 1L]
    }
    total
  }
  byCycle <- data.frame(cycle = cycles, elapsed_day = cycles * model$cycleLength)
  byCycle$period <- periods$period[.tmaPeriodIndex(byCycle$elapsed_day, periods)]
  byCycle$state_cost <- sumByCycle(stateLedger$expected_cost, stateLedger$cycle)
  byCycle$transition_cost <- sumByCycle(transitionLedger$expected_cost, transitionLedger$destination_cycle)
  byCycle$entry_cost <- sumByCycle(entryLedger$expected_cost, entryLedger$cycle)
  byCycle$total_cost <- byCycle$state_cost + byCycle$transition_cost + byCycle$entry_cost
  byCycle$cumulative_cost <- cumsum(byCycle$total_cost)
  totals <- data.frame(component = c("state", "transition", "entry", "total"),
                        cost = c(sum(byCycle$state_cost), sum(byCycle$transition_cost),
                                  sum(byCycle$entry_cost), sum(byCycle$total_cost)))
  if (any(!is.finite(c(stateLedger$expected_cost, transitionLedger$expected_cost,
                       entryLedger$expected_cost, byCycle$total_cost, byCycle$cumulative_cost, totals$cost)))) {
    stop("Projected costs exceed finite numeric limits. Check the amounts and cohortSize.", call. = FALSE)
  }
  totals$cost_per_person <- totals$cost / model$cohortSize
  list(byCycle = byCycle, totals = totals,
       details = list(state = stateLedger, transition = transitionLedger, entry = entryLedger),
       costRules = list(state = stateRules, transition = transitionRules, entry = entryCost),
       metadata = list(currency = currency, priceYear = priceYear, cohortSize = model$cohortSize,
                       cycleLength = model$cycleLength, charged_cycles = length(cycles),
                       charged_duration_days = length(cycles) * model$cycleLength,
                       cost_periods = periods,
                       components = c(state = !is.null(stateCosts), transition = !is.null(transitionCosts),
                                       entry = !is.null(entryCost)),
                       state_cost_unit = "cost per projected patient per full cycle label",
                       transition_cost_unit = "additional cost per represented state change, including entry",
                       entry_cost_unit = "startup cost per projected patient, once in cycle zero",
                       timing = "destination cycle label start, including cycle zero; no baseline state charge",
                       discounting = "none", conversion = "none", observed_duration_proration = "none"))
}

.tmaCostAmounts <- function(x, name) {
  if (!is.numeric(x) || is.complex(x) || inherits(x, "integer64") || !is.null(dim(x)) ||
      anyNA(x) || any(!is.finite(x))) {
    stop(name, " must contain finite numeric amounts. Missing amounts are not zero.", call. = FALSE)
  }
  invisible(NULL)
}

.tmaProjectedCostRules <- function(input, keys, states, periods, name) {
  if (is.null(input)) {
    result <- as.data.frame(stats::setNames(rep(list(character()), length(keys)), keys),
                             stringsAsFactors = FALSE)
    result$cost_period <- character()
    result$unit_cost <- numeric()
    return(result)
  }
  if (identical(keys, "state") && is.numeric(input)) {
    .tmaCostAmounts(input, name)
    if (is.null(names(input)) || anyNA(names(input)) || anyDuplicated(names(input))) {
      stop("Named stateCosts must have one unique state name per cost.", call. = FALSE)
    }
    input <- data.frame(state = names(input), cost = unname(input), stringsAsFactors = FALSE)
  }
  required <- c(keys, "cost")
  if (!is.data.frame(input) || anyDuplicated(names(input)) ||
      !all(required %in% names(input)) || any(!names(input) %in% c(required, "period"))) {
    stop(name, " must be a table with ", paste(required, collapse = ", "),
         " and optionally period. Costs must already use the documented model units.", call. = FALSE)
  }
  rules <- as.data.frame(input, stringsAsFactors = FALSE)
  .tmaCostAmounts(rules$cost, paste0(name, "$cost"))
  for (key in keys) {
    if (is.factor(rules[[key]])) rules[[key]] <- as.character(rules[[key]])
    if (!is.character(rules[[key]]) || anyNA(rules[[key]]) || any(!rules[[key]] %in% states)) {
      stop(name, "$", key, " must contain defined state labels.", call. = FALSE)
    }
  }
  if (!"period" %in% names(rules)) {
    nRules <- nrow(rules)
    rules <- rules[rep(seq_len(nRules), times = nrow(periods)), , drop = FALSE]
    rules$period <- rep(periods$period, each = nRules)
  } else {
    if (is.factor(rules$period)) rules$period <- as.character(rules$period)
    if (!is.character(rules$period) || anyNA(rules$period) || any(!rules$period %in% periods$period)) {
      stop(name, "$period must name a defined cost period.", call. = FALSE)
    }
  }
  if (anyDuplicated(rules[c(keys, "period")])) {
    stop(name, " contains duplicate state/period rules. Combine intended additive charges explicitly.", call. = FALSE)
  }
  if (identical(keys, "state") && nrow(rules) != length(states) * nrow(periods)) {
    stop("stateCosts must explicitly cover every state in each cost period. Include zero costs when intended.", call. = FALSE)
  }
  names(rules)[names(rules) == "cost"] <- "unit_cost"
  names(rules)[names(rules) == "period"] <- "cost_period"
  rownames(rules) <- NULL
  rules[c(keys, "cost_period", "unit_cost")]
}

.tmaApplyCostRules <- function(ledger, rules, keys) {
  result <- as.data.frame(dplyr::left_join(ledger, rules, by = keys))
  result$rule_applied <- !is.na(result$unit_cost)
  result$unit_cost[!result$rule_applied] <- 0
  result$expected_cost <- result$expected_count * result$unit_cost
  result
}
