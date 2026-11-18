# Shared interval summaries keep deterministic and sampled outcome definitions
# identical. Compare like executions and explicit completed horizons.
.tmaCompareIntervalOutcomes <- function(reference, intervention, scenarioCovariates) {
  if (!inherits(reference, "tma_outcomes") || !inherits(intervention, "tma_outcomes"))
    stop("Both scenarios must use summariseOutcomes(). Do not mix interval and full-label accounting.", call. = FALSE)
  for (name in c("n", "states", "cycleLength", "nCycles", "source_day", "execution")) {
    if (!identical(reference$metadata[[name]], intervention$metadata[[name]]))
      stop("Scenario ", name, " must agree.", call. = FALSE)
  }
  r <- reference$rewards
  i <- intervention$rewards
  for (name in c("aliveStates", "utilities", "healthTiming", "discountTiming",
                 "costDiscountRate", "healthDiscountRate", "daysPerYear")) {
    if (!isTRUE(all.equal(r[[name]], i[[name]], check.attributes = TRUE)))
      stop("Scenario reward definition differs for ", name, ".", call. = FALSE)
  }
  if (r$costs_configured && i$costs_configured &&
      (!identical(r$currency, i$currency) || !identical(r$priceYear, i$priceYear)))
    stop("Scenario currency and priceYear must agree.", call. = FALSE)
  if (identical(reference$metadata$execution, "projection") &&
      "weight" %in% scenarioCovariates) {
    stop("scenarioCovariates cannot exclude initial distribution weight from a projection comparison.", call. = FALSE)
  }
  columns <- .tmaScenarioPopulation(reference$initialPopulation, intervention$initialPopulation, scenarioCovariates)
  rt <- reference$totals
  it <- intervention$totals[match(rt$metric, intervention$totals$metric), , drop = FALSE]
  outcomes <- .tmaScenarioDifference(rt$metric, rt$total, it$total, rt$per_patient, it$per_patient)
  components <- union(reference$costs$component, intervention$costs$component)
  costs <- do.call(rbind, lapply(components, function(component) {
    values <- function(x) {
      row <- match(component, x$costs$component)
      if (is.na(row)) return(if (x$metadata$costs_configured) c(0, 0) else c(NA_real_, NA_real_))
      unlist(x$costs[row, c("cost", "discounted_cost")], use.names = FALSE)
    }
    left <- values(reference); right <- values(intervention)
    data.frame(component = component, .tmaScenarioDifference(c("cost", "discounted_cost"),
      left, right, left / reference$metadata$n, right / intervention$metadata$n))
  }))
  if (is.null(costs)) costs <- data.frame(component = character(), metric = character())
  dc <- outcomes$incremental_per_patient[outcomes$metric == "discounted_cost"]
  dq <- outcomes$incremental_per_patient[match(c("discounted_life_years", "discounted_qalys"), outcomes$metric)]
  ce <- data.frame(outcome = c("life_years", "qalys"), basis = "discounted",
    incremental_cost = rep(dc, 2), incremental_effect = dq, icer = NA_real_, status = "unavailable")
  for (j in 1:2) {
    if (!is.finite(dc) || !is.finite(dq[j])) next
    if (dq[j] > 0) {
      ce$status[j] <- if (dc > 0) "higher_cost_more_effective" else if (dc < 0) "dominant" else "equal_cost_more_effective"
      if (dc >= 0) ce$icer[j] <- dc / dq[j]
    } else if (dq[j] < 0) {
      ce$status[j] <- if (dc > 0) "dominated" else if (dc < 0) "lower_cost_less_effective" else "equal_cost_less_effective"
    } else ce$status[j] <- if (dc > 0) "equal_effect_higher_cost" else if (dc < 0) "equal_effect_lower_cost" else "no_difference"
  }
  if (any(!is.finite(ce$icer[!is.na(ce$icer)]))) stop("ICER exceeds finite limits.", call. = FALSE)
  structure(list(outcomes = outcomes, costs = costs, costEffectiveness = ce,
    metadata = c(reference$metadata, list(checked_population_columns = columns))),
    class = "tma_scenario_comparison")
}
