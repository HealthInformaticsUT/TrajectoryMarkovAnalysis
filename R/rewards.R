#' Define costs and health outcomes for completed model intervals
#'
#' @param stateCosts Named costs per state per cycle, or a data frame with state
#'   and cost columns. Optional period and component columns select cost periods
#'   and name separate cost components. Each component must cover every state
#'   in every cost period, including explicit zeros.
#' @param transitionCosts Optional data frame with from_state, to_state and cost.
#'   Optional stage selects a named within-cycle stage (default "transition"
#'   means the net interval change). Optional period and component are supported.
#'   Only specified changes are charged. Self-pairs are not event costs.
#' @param upfrontCosts Optional named numeric vector of amounts per initial
#'   patient, charged once. No fitted entry model is required.
#' @param aliveStates Explicit living states for life-years, or NULL to omit
#'   health outcomes. An empty vector explicitly gives zero living time.
#' @param utilities Optional named utility for every state. Requires aliveStates.
#'   Nonliving states must have zero utility. Negative living utilities are valid.
#' @param costTiming,healthTiming "start", "end", or a named stage. A stage
#'   selects the state immediately after that stage. Stages add no duration.
#' @param discountTiming "start", "midpoint" or "end" of each interval.
#' @param costDiscountRate,healthDiscountRate Nonnegative annual discount rates.
#' @param currency,priceYear Required currency label and reference year when
#'   costs are supplied. These describe amounts, without converting prices.
#' @param periodBreaks,periodLabels Cost periods, in source elapsed days. NULL
#'   applies one period to the whole horizon. Boundaries must align with cycles.
#' @param daysPerYear Positive days per year for durations and discounting.
#' @details Costs are explicit amounts per model interval. A supplied observed
#'   weekly mean is not converted to a daily mean or multiplied by seven. Use
#'   estimateStateCosts() for allocation and choose the intended cost denominator
#'   before constructing these rewards. Upfront costs apply to every initial
#'   patient, including zero-follow-up patients, at their initial elapsed day.
#' @return A tma_rewards definition used by summariseOutcomes().
#' @export
createRewardModel <- function(stateCosts = NULL, transitionCosts = NULL,
                              upfrontCosts = NULL, aliveStates = NULL, utilities = NULL,
                              costTiming = "start", healthTiming = "start",
                              discountTiming = "midpoint", costDiscountRate = 0,
                              healthDiscountRate = 0, currency = NULL, priceYear = NULL,
                              periodBreaks = NULL, periodLabels = NULL, daysPerYear = 365.25) {
  for (name in c("costTiming", "healthTiming")) {
    x <- get(name)
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop(name, " must be 'start', 'end', or one stage name.", call. = FALSE)
  }
  if (length(discountTiming) != 1L || is.na(discountTiming) ||
      !discountTiming %in% c("start", "midpoint", "end"))
    stop("discountTiming must be 'start', 'midpoint' or 'end'.", call. = FALSE)
  for (name in c("costDiscountRate", "healthDiscountRate", "daysPerYear")) {
    x <- get(name)
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0 ||
        (name == "daysPerYear" && x == 0)) stop(name, " is invalid.", call. = FALSE)
  }
  if (!is.null(aliveStates) && (!is.character(aliveStates) || anyNA(aliveStates) ||
      anyDuplicated(aliveStates))) stop("aliveStates must contain unique state names.", call. = FALSE)
  if (!is.null(utilities)) {
    if (is.null(aliveStates) || !is.numeric(utilities) || is.null(names(utilities)) ||
        anyNA(names(utilities)) || anyDuplicated(names(utilities)) ||
        any(!is.finite(utilities) | utilities > 1) ||
        any(utilities[!names(utilities) %in% aliveStates] != 0))
      stop("Supply named utilities, with zero for nonliving states, and explicit aliveStates.", call. = FALSE)
  }
  if (!is.null(upfrontCosts) && (!is.numeric(upfrontCosts) || is.null(names(upfrontCosts)) ||
      !length(upfrontCosts) || anyNA(names(upfrontCosts)) || any(!nzchar(names(upfrontCosts))) ||
      anyDuplicated(names(upfrontCosts)) || any(!is.finite(upfrontCosts))))
    stop("upfrontCosts must be a finite numeric vector with unique component names.", call. = FALSE)
  hasCosts <- !is.null(stateCosts) || !is.null(transitionCosts) || !is.null(upfrontCosts)
  for (name in c("stateCosts", "transitionCosts")) {
    x <- get(name)
    if (!is.null(x) && length(x) == 0L || (is.data.frame(x) && nrow(x) == 0L))
      stop(name, " is empty. Use NULL to omit that component.", call. = FALSE)
  }
  if (hasCosts) {
    if (!is.character(currency) || length(currency) != 1L || is.na(currency) || !nzchar(trimws(currency)))
      stop("Supply a currency for the rewards.", call. = FALSE)
    priceYear <- .tmaProjectionInteger(priceYear, "priceYear", 1)
  }
  periods <- .tmaPeriods(periodBreaks, periodLabels)
  structure(list(stateCosts = stateCosts, transitionCosts = transitionCosts,
    upfrontCosts = upfrontCosts, aliveStates = aliveStates, utilities = utilities,
    costTiming = costTiming, healthTiming = healthTiming, discountTiming = discountTiming,
    costDiscountRate = costDiscountRate, healthDiscountRate = healthDiscountRate,
    currency = currency, priceYear = priceYear, periods = periods,
    daysPerYear = daysPerYear, costs_configured = hasCosts), class = "tma_rewards")
}

#' Calculate the same interval outcomes for projections and simulations
#'
#' @param result A projectCohort() or simulateTrajectories() result.
#' @param rewards A definition from createRewardModel().
#' @param nCycles Optional number of completed intervals to include. By default
#'   includes all completed intervals. The last endpoint is not another cycle.
#'   A longer explicit horizon is allowed only after every simulated patient
#'   has reached a declared absorbing terminal state.
#' @details A projection with 52 steps contributes 52 weekly intervals, not 53
#'   snapshot labels. Costs and health outcomes use the declared state timing.
#'   Upfront fees are counted once, including when there are no completed steps.
#'   Saved simulation cost ledgers are separate: this function rejects simulations
#'   configured with costRules or a cost model, to prevent silently discarding or
#'   double-counting those charges. Use an uncosted simulation with these shared
#'   rewards, or summariseSimulation() for the existing labelled-cycle ledger.
#'
#'   Stage rewards require retained stage events. The function never infers an
#'   intermediate event from a net transition. Simulation covariates and the
#'   starting population remain in the returned object for scenario comparison.
#'
#'   If a simulation stopped because all patients were terminal, an explicit
#'   longer nCycles carries their terminal snapshots to that common horizon.
#'   It adds no transitions, stage events or living time. Explicit recurring
#'   terminal-state costs still apply. No extension is allowed after max_steps
#'   or a custom stop, or when terminal states are not absorbing in every model
#'   phase. The original simulation is unchanged. Metadata records the actual
#'   recorded_steps and the number of terminal_extension intervals.
#' @return A tma_outcomes object containing byCycle, totals, costs, initialPopulation,
#'   metadata and, for simulations, byPatient. Counts are expected weights for a
#'   deterministic projection and actual counts for a simulation. No files are
#'   written and no disclosure policy is applied.
#' @export
summariseOutcomes <- function(result, rewards, nCycles = NULL) {
  if (!inherits(rewards, "tma_rewards")) stop("Supply createRewardModel() rewards.", call. = FALSE)
  args <- rewards[intersect(names(formals(createRewardModel)), names(rewards))]
  args$periodBreaks <- c(rewards$periods$start_day, tail(rewards$periods$end_day, 1L))
  args$periodLabels <- rewards$periods$period
  rewards <- do.call(createRewardModel, args)
  simulation <- inherits(result, "tma_simulation")
  if (simulation) {
    checked <- .tmaSimulationSummaryInput(result)
    if (checked$costsConfigured)
      stop("Use an uncosted simulation with shared rewards, or summariseSimulation() for its saved charges.", call. = FALSE)
    states <- checked$states
    cycleLength <- checked$cycleLength
    steps <- result$metadata$steps
    initial <- checked$initialPopulation
    if (!is.null(result$entry))
      stop("Shared interval rewards start from explicit boundary states. Use the labelled-cycle summary for an entry outcome model.", call. = FALSE)
    firstCycle <- checked$history$cycle[checked$first]
    firstDay <- checked$history$elapsed_day[checked$first]
    if (length(unique(firstCycle)) != 1L || length(unique(firstDay)) != 1L)
      stop("Shared rewards require one starting cycle and elapsed day for this simulation.", call. = FALSE)
    n <- nrow(initial)
    origin <- firstDay[1L]
    offset <- firstCycle[1L]
  } else {
    result <- .tmaComparisonProjection(result)
    states <- result$metadata$states
    cycleLength <- result$metadata$cycleLength
    steps <- result$metadata$nCycles
    if (!is.null(result$entry))
      stop("Shared interval rewards start from explicit boundary states. Use projectCosts() for labelled entry outcomes.", call. = FALSE)
    n <- result$metadata$cohortSize
    initial <- data.frame(state_label = states,
      weight = unname(result$metadata$initial_distribution[states]), stringsAsFactors = FALSE)
    origin <- offset <- 0
  }
  if (is.null(nCycles)) nCycles <- steps
  nCycles <- .tmaProjectionInteger(nCycles, "nCycles", 0)
  if (nCycles > steps) {
    if (!simulation || !identical(result$metadata$stop_reason, "all_terminal") ||
        !all(checked$population$state_label %in% checked$terminalStates)) {
      stop("nCycles exceeds completed intervals. Only an all_terminal simulation can be extended explicitly.", call. = FALSE)
    }
    bundle <- .tmaValidateModelBundle(result$.config$bundle)
    requireAbsorption <- function(x) {
      if (!all(checked$population$state_label %in% x$metadata$absorbingStates))
        stop("Terminal extension requires absorbing final states in every model phase.", call. = FALSE)
      if (!is.null(x$extrapolation)) requireAbsorption(x$extrapolation$bundle)
    }
    requireAbsorption(bundle)
    extra <- nCycles - steps
    if (nrow(checked$history) + n * extra > .Machine$integer.max ||
        origin + nCycles * cycleLength >= 2^53 || offset + nCycles >= 2^53)
      stop("The extended horizon exceeds supported interval indexing.", call. = FALSE)
    last <- checked$history[checked$last, , drop = FALSE]
    extension <- last[rep(seq_len(n), times = extra), , drop = FALSE]
    extension$cycle <- rep(offset + seq.int(steps + 1, nCycles), each = n)
    extension$elapsed_day <- origin + (extension$cycle - offset) * cycleLength
    checked$history <- rbind(checked$history, extension)
  }
  if (any(rewards$periods$start_day %% cycleLength != 0))
    stop("Reward periods must align with cycleLength.", call. = FALSE)
  if (!all(rewards$aliveStates %in% states) ||
      (!is.null(rewards$utilities) && !setequal(names(rewards$utilities), states)))
    stop("Reward state names must match the model's states.", call. = FALSE)
  if (simulation && any(rewards$aliveStates %in% checked$terminalStates))
    stop("Terminal states cannot contribute living time.", call. = FALSE)

  rules <- .tmaRewardRules(rewards, states)
  cycles <- seq_len(nCycles) - 1L + offset
  days <- origin + (seq_len(nCycles) - 1L) * cycleLength
  byCycle <- data.frame(cycle = cycles, source_day = days, destination_day = days + cycleLength)
  contributions <- list()
  add <- function(x, component, metric, amount, at = NULL) {
    if (!nrow(x)) return(invisible(NULL))
    if (is.null(at)) at <- x$source_day +
      c(start = 0, midpoint = .5, end = 1)[[rewards$discountTiming]] * cycleLength
    rate <- if (metric == "cost") rewards$costDiscountRate else rewards$healthDiscountRate
    value <- x$weight * amount
    contributions[[length(contributions) + 1L]] <<- data.frame(
      simulation_id = if ("simulation_id" %in% names(x)) as.character(x$simulation_id) else NA_character_,
      cycle = x$cycle, source_day = x$source_day, component = component, metric = metric,
      value = value, discounted = value * (1 + rate)^(-at / rewards$daysPerYear),
      stringsAsFactors = FALSE)
  }
  stateRows <- function(timing) .tmaRewardStates(result, timing, nCycles, simulation, checked =
    if (simulation) checked else NULL, origin = origin, offset = offset)
  if (nrow(rules$state)) {
    x <- stateRows(rewards$costTiming)
    x$period <- rewards$periods$period[.tmaPeriodIndex(x$source_day, rewards$periods)]
    for (component in unique(rules$state$component)) {
      r <- rules$state[rules$state$component == component, , drop = FALSE]
      key <- match(paste(x$state, x$period), paste(r$state, r$cost_period))
      if (anyNA(key)) stop("A state reward is missing for an accounted interval.", call. = FALSE)
      add(x, component, "cost", r$cost[key])
    }
  }
  if (nrow(rules$transition)) {
    for (stage in unique(rules$transition$stage)) {
      x <- .tmaRewardFlows(result, stage, nCycles, simulation, offset)
      x$period <- rewards$periods$period[.tmaPeriodIndex(x$source_day, rewards$periods)]
      for (component in unique(rules$transition$component[rules$transition$stage == stage])) {
        r <- rules$transition[rules$transition$stage == stage & rules$transition$component == component, , drop = FALSE]
        key <- match(paste(x$from_state, x$to_state, x$period), paste(r$from_state, r$to_state, r$cost_period))
        keep <- !is.na(key)
        add(x[keep, , drop = FALSE], component, "cost", r$cost[key[keep]])
      }
    }
  }
  if (!is.null(rewards$aliveStates)) {
    x <- stateRows(rewards$healthTiming)
    ly <- as.numeric(x$state %in% rewards$aliveStates) * cycleLength / rewards$daysPerYear
    add(x, "health", "life_years", ly)
    if (!is.null(rewards$utilities)) add(x, "health", "qalys", ly * rewards$utilities[x$state])
  }
  if (length(rewards$upfrontCosts)) {
    x <- data.frame(cycle = offset, source_day = origin, weight = if (simulation) rep(1, n) else n)
    if (simulation) x$simulation_id <- initial$simulation_id
    for (component in names(rewards$upfrontCosts))
      add(x, component, "cost", rewards$upfrontCosts[[component]], at = origin)
  }
  ledger <- if (length(contributions)) do.call(rbind, contributions) else data.frame(
    simulation_id = character(), cycle = numeric(), source_day = numeric(), component = character(),
    metric = character(), value = numeric(), discounted = numeric())
  if (any(!is.finite(ledger$value)) || any(!is.finite(ledger$discounted)))
    stop("Reward totals exceed finite numeric limits.", call. = FALSE)
  metrics <- c("cost", "life_years", "qalys")
  configured <- c(rewards$costs_configured, !is.null(rewards$aliveStates), !is.null(rewards$utilities))
  totals <- data.frame(metric = c(metrics, paste0("discounted_", metrics)), total = NA_real_)
  for (j in seq_along(metrics)) {
    m <- metrics[j]
    x <- ledger[ledger$metric == m, , drop = FALSE]
    for (discounted in c(FALSE, TRUE)) {
      name <- if (discounted) paste0("discounted_", m) else m
      v <- if (discounted) x$discounted else x$value
      totals$total[totals$metric == name] <- if (configured[j]) sum(v) else NA_real_
      byCycle[[name]] <- if (configured[j]) .tmaSimulationSummarySum(v[x$cycle %in% cycles],
        match(x$cycle[x$cycle %in% cycles], cycles), nCycles) else rep(NA_real_, nCycles)
    }
  }
  totals$per_patient <- totals$total / n
  components <- unique(c(rules$state$component, rules$transition$component, names(rewards$upfrontCosts)))
  costs <- data.frame(component = components, cost = numeric(length(components)), discounted_cost = numeric(length(components)))
  for (i in seq_along(components)) {
    x <- ledger[ledger$metric == "cost" & ledger$component == components[i], , drop = FALSE]
    costs$cost[i] <- sum(x$value)
    costs$discounted_cost[i] <- sum(x$discounted)
  }
  costs$cost_per_patient <- costs$cost / n
  costs$discounted_cost_per_patient <- costs$discounted_cost / n
  byPatient <- NULL
  if (simulation) {
    byPatient <- data.frame(simulation_id = initial$simulation_id)
    for (j in seq_along(metrics)) for (discounted in c(FALSE, TRUE)) {
      name <- if (discounted) paste0("discounted_", metrics[j]) else metrics[j]
      x <- ledger[ledger$metric == metrics[j], , drop = FALSE]
      byPatient[[name]] <- if (configured[j]) .tmaSimulationSummarySum(
        if (discounted) x$discounted else x$value, match(x$simulation_id, byPatient$simulation_id), n) else NA_real_
    }
  }
  structure(list(byCycle = byCycle, totals = totals, costs = costs, byPatient = byPatient,
    initialPopulation = initial, rewards = rewards,
    metadata = list(n = n, states = states, cycleLength = cycleLength, nCycles = nCycles,
      source_day = origin, end_day = origin + nCycles * cycleLength,
      accounted_days = nCycles * cycleLength, recorded_steps = steps,
      terminal_extension = max(0, nCycles - steps),
      execution = if (simulation) "simulation" else "projection",
      currency = rewards$currency, priceYear = rewards$priceYear,
      timing = "completed intervals only; final endpoint adds no time",
      costs_configured = rewards$costs_configured)), class = "tma_outcomes")
}

.tmaRewardRules <- function(rewards, states) {
  state <- rewards$stateCosts
  if (is.numeric(state)) state <- data.frame(state = names(state), cost = unname(state))
  collect <- function(x, keys, default) {
    if (is.null(x)) {
      out <- as.data.frame(stats::setNames(rep(list(character()), length(keys)), keys))
      out$cost <- numeric(); out$cost_period <- character(); out$component <- character()
      return(out)
    }
    x <- as.data.frame(x)
    if (!"component" %in% names(x)) x$component <- default
    if (anyNA(x$component) || any(!nzchar(x$component))) stop("Invalid reward component name.", call. = FALSE)
    do.call(rbind, lapply(unique(x$component), function(component) {
      z <- x[x$component == component, setdiff(names(x), "component"), drop = FALSE]
      z <- .tmaProjectedCostRules(z, keys, states, rewards$periods, "rewards")
      names(z)[names(z) == "unit_cost"] <- "cost"
      z$component <- component
      z
    }))
  }
  state <- collect(state, "state", "healthcare")
  transition <- rewards$transitionCosts
  if (is.null(transition)) {
    transition <- collect(NULL, c("from_state", "to_state"), "event")
    transition$stage <- character()
  } else {
    if (!is.data.frame(transition)) stop("transitionCosts must be a data frame.", call. = FALSE)
    if (!"stage" %in% names(transition)) transition$stage <- "transition"
    if (anyNA(transition$stage) || any(!nzchar(transition$stage))) stop("Invalid reward stage name.", call. = FALSE)
    transition <- do.call(rbind, lapply(unique(transition$stage), function(stage) {
      x <- collect(transition[transition$stage == stage, setdiff(names(transition), "stage"), drop = FALSE],
        c("from_state", "to_state"), "event")
      x$stage <- stage
      x
    }))
    if (any(transition$from_state == transition$to_state))
      stop("Use stateCosts for residency, not self-transition event costs.", call. = FALSE)
  }
  list(state = state, transition = transition)
}

.tmaRewardStates <- function(result, timing, nCycles, simulation, checked, origin, offset) {
  if (!simulation) {
    if (timing %in% c("start", "end")) {
      x <- result$occupancy[result$occupancy$cycle %in%
        (seq_len(nCycles) - if (timing == "start") 1L else 0L), , drop = FALSE]
      x$cycle <- x$cycle - as.integer(timing == "end")
      return(data.frame(cycle = x$cycle, source_day = x$cycle * result$metadata$cycleLength,
        state = x$state, weight = x$expected_count))
    }
    x <- .tmaRewardFlows(result, timing, nCycles, FALSE, offset)
    return(data.frame(cycle = x$cycle, source_day = x$source_day, state = x$to_state, weight = x$weight))
  }
  h <- checked$history
  start <- h[h$cycle >= offset & h$cycle < offset + nCycles, , drop = FALSE]
  x <- data.frame(simulation_id = start$simulation_id, cycle = start$cycle,
    source_day = start$elapsed_day, state = start$state_label, weight = rep(1, nrow(start)))
  if (timing == "start") return(x)
  if (timing == "end") {
    idx <- match(paste(x$simulation_id, x$cycle + 1), paste(h$simulation_id, h$cycle))
    if (anyNA(idx)) stop("An interval lacks its destination snapshot.", call. = FALSE)
    x$state <- h$state_label[idx]
    return(x)
  }
  events <- .tmaRewardFlows(result, timing, nCycles, TRUE, offset)
  idx <- match(paste(x$simulation_id, x$cycle), paste(events$simulation_id, events$cycle))
  absent <- is.na(idx)
  if (any(absent & !x$state %in% checked$terminalStates))
    stop("A requested stage has no retained event for an active interval.", call. = FALSE)
  x$state[!absent] <- events$to_state[idx[!absent]]
  x
}

.tmaRewardFlows <- function(result, stage, nCycles, simulation, offset) {
  if (simulation) {
    x <- if (stage == "transition") result$transitions else result$stageTransitions
    if (stage != "transition") {
      if (is.null(x) || !"stage" %in% names(x)) stop("The simulation has no retained stage events.", call. = FALSE)
      stageNames <- unique(unlist(lapply(result$.config$bundle$models, function(m) m$metadata$stage_order)))
      if (!stage %in% c(stageNames, unique(x$stage))) stop("Unknown reward stage.", call. = FALSE)
      x <- x[x$stage == stage, , drop = FALSE]
    }
    if (!"from_state" %in% names(x)) x$from_state <- x$source_state
    if (!"to_state" %in% names(x)) x$to_state <- x$destination_state
    x$weight <- rep(1, nrow(x))
  } else {
    x <- if (stage == "transition") result$flows else result$stageFlows
    if (is.null(x) || !stage %in% c("transition", result$metadata$stage_order))
      stop("The projection has no requested stage flows.", call. = FALSE)
    x <- x[x$stage == stage, , drop = FALSE]
    x$weight <- x$expected_count
  }
  x <- x[!is.na(x$source_cycle) & x$source_cycle >= offset & x$source_cycle < offset + nCycles, , drop = FALSE]
  if (simulation && stage != "transition") {
    draws <- result$transitions
    draws <- draws[draws$source_cycle >= offset & draws$source_cycle < offset + nCycles, , drop = FALSE]
    if (anyNA(match(paste(draws$simulation_id, draws$source_cycle), paste(x$simulation_id, x$source_cycle))))
      stop("A requested stage has no retained event for an active interval.", call. = FALSE)
  }
  if (anyNA(x$weight)) stop("Requested stage paths are unavailable after net transition effects.", call. = FALSE)
  x$cycle <- x$source_cycle
  x
}
