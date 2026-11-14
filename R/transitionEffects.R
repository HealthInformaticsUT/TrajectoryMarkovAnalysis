#' Explicit interventions on cycle transition probabilities
#'
#' @name transition-effects
#' @description Supply transitionEffects to simulateTrajectories() as a named
#'   list of rules. Each rule requires from, to, multiplier and redistributeTo.
#'   Optional fields are eligible, startDay, endDay, applyAt and stage. Component names must be
#'   unique and fields not listed here are errors.
#' @details A rule multiplies the source row's probability of to by multiplier
#'   and assigns the remaining probability of the to/redistributeTo pair to
#'   redistributeTo. Other probabilities remain unchanged. For example, the row
#'   Alive = 0.70, Pneumonia = 0.20, Dead = 0.10 becomes 0.75, 0.15, 0.10 when
#'   Pneumonia has multiplier = 0.75 and redistributeTo = "Alive". Multipliers
#'   above one take probability from redistributeTo; impossible changes fail.
#'   This operates on cycle probabilities, not hazards or hazard ratios. No
#'   normalization, hazard conversion or intervention effect is inferred.
#'
#'   from must be a nonabsorbing source state. to and redistributeTo must be
#'   distinct known states; either may equal from. multiplier is a finite
#'   nonnegative scalar. A multiplier of one, or an original zero probability
#'   for to, leaves the row unchanged. An unavailable source row is still an
#'   error, including when multiplier is zero.
#'
#'   applyAt defaults to "transitions", which applies the rule only to the
#'   adjacent-cycle steps after the first snapshot. Use "entry" for the explicit
#'   entry model, or "both" for entry and subsequent steps. An entry application
#'   uses the pre-entry state and covariates at elapsed day zero. Entry rules
#'   require an entry model. Eligibility callbacks are not called in phases
#'   excluded by applyAt, and the same rule is never applied twice at entry.
#'   Entry and the first adjacent step both have source day zero. With weekly
#'   cycles, applyAt = "both" and endDay = 28 affects entry plus four adjacent
#'   steps, not four outcome labels. For four affected labels, supply a separate
#'   entry rule and a transitions rule ending at source day 21.
#'
#'   startDay defaults to zero and endDay to infinity. Finite boundaries must
#'   be whole days aligned with cycleLength. The start is included and the end
#'   excluded, using the source snapshot's elapsed_day. eligible receives only
#'   the current nonterminal source rows matching from and this time window;
#'   return one logical value per supplied row, without missing values. It is
#'   not called for an empty candidate set. At most one rule may apply to a
#'   patient at a stage, even when different rules name different destinations.
#'   Separate stages may each have a rule. Without stage, this limit applies
#'   to the complete transition step.
#'
#'   Effects are applied to the selected transition row before sampling and
#'   leave stored models unchanged. For composed models they affect combined
#'   cycle outcomes unless stage names a declared stage. A stage rule changes
#'   that stage before later stages are evaluated. Stage eligibility receives
#'   cycle-start covariates with state_label set to each reachable stage source.
#'   Callbacks must be deterministic functions of these supplied values. Net
#'   and stage-targeted rules cannot be mixed in the same transition run. Cost rules and cost
#'   models are independent and need separate explicit specification.
#'
#'   The only endpoint accommodation is binary floating-point roundoff: a
#'   multiplied target exceeding the available pair mass by at most two machine
#'   epsilons relative to that mass is set to the pair mass. Larger excesses
#'   and other invalid probabilities are errors. Valid source rows use the
#'   package's existing sum tolerance of 1e-10; rows are never renormalized.
#' @seealso simulateTrajectories()
#' @examples
#' list(prevention = list(
#'   from = "Alive", to = "Pneumonia", multiplier = 0.75,
#'   redistributeTo = "Alive", startDay = 0, endDay = 28,
#'   eligible = function(p) p$treatment_active
#' ))
NULL

.tmaTransitionEffects <- function(rules, states, absorbingStates, cycleLength) {
  if (is.null(rules)) return(list())
  if (!is.list(rules) || is.data.frame(rules) ||
      (length(rules) && (is.null(names(rules)) || anyNA(names(rules)) ||
        any(!nzchar(trimws(names(rules)))) || anyDuplicated(names(rules))))) {
    stop("transitionEffects must be a list with unique, nonempty rule names.", call. = FALSE)
  }
  cycleLength <- .tmaProjectionInteger(cycleLength, "Transition effect cycleLength", 1)
  required <- c("from", "to", "multiplier", "redistributeTo")
  fields <- c(required, "eligible", "startDay", "endDay", "applyAt", "stage")
  for (name in names(rules)) {
    rule <- rules[[name]]
    label <- paste0("transitionEffects[['", name, "']]")
    if (!is.list(rule) || is.data.frame(rule) || is.null(names(rule)) ||
        anyNA(names(rule)) || anyDuplicated(names(rule)) ||
        !all(required %in% names(rule)) || any(!names(rule) %in% fields)) {
      stop(label, " must contain from, to, multiplier and redistributeTo, with only ",
           "eligible, startDay, endDay, applyAt and stage as optional fields.", call. = FALSE)
    }
    for (field in c("from", "to", "redistributeTo")) {
      value <- rule[[field]]
      if (!is.character(value) || length(value) != 1L || !is.null(dim(value)) ||
          is.na(value) || !value %in% states) {
        stop(label, "$", field, " must name one known state.", call. = FALSE)
      }
      rule[[field]] <- unname(value)
    }
    if (rule$from %in% absorbingStates) {
      stop(label, "$from cannot be a declared absorbing state.", call. = FALSE)
    }
    if (rule$to == rule$redistributeTo) {
      stop(label, "$to and redistributeTo must be distinct states.", call. = FALSE)
    }
    multiplier <- rule$multiplier
    if (!is.numeric(multiplier) || is.complex(multiplier) || inherits(multiplier, "integer64") ||
        length(multiplier) != 1L || !is.null(dim(multiplier)) ||
        !is.finite(multiplier) || multiplier < 0) {
      stop(label, "$multiplier must be one finite nonnegative numeric value.", call. = FALSE)
    }
    rule$multiplier <- unname(multiplier)
    if (is.null(rule$applyAt)) rule$applyAt <- "transitions"
    if (!is.character(rule$applyAt) || length(rule$applyAt) != 1L ||
        !is.null(dim(rule$applyAt)) || is.na(rule$applyAt) ||
        !rule$applyAt %in% c("transitions", "entry", "both")) {
      stop(label, "$applyAt must be 'transitions', 'entry' or 'both'.", call. = FALSE)
    }
    rule$applyAt <- unname(rule$applyAt)
    if (!is.null(rule$stage)) {
      if (!is.character(rule$stage) || length(rule$stage) != 1L ||
          is.na(rule$stage) || !nzchar(trimws(rule$stage))) {
        stop(label, "$stage must name one stage.", call. = FALSE)
      }
      if (rule$applyAt != "transitions") {
        stop(label, "$stage applies only to transition stages, not the separate entry matrix.", call. = FALSE)
      }
      rule$stage <- unname(rule$stage)
    }
    if (!is.null(rule$eligible) && !is.function(rule$eligible)) {
      stop(label, "$eligible must be a function of candidate source snapshots.", call. = FALSE)
    }
    if (is.null(rule$startDay)) rule$startDay <- 0
    if (is.null(rule$endDay)) rule$endDay <- Inf
    for (field in c("startDay", "endDay")) {
      day <- rule[[field]]
      if (!is.numeric(day) || is.complex(day) || inherits(day, "integer64") ||
          length(day) != 1L || !is.null(dim(day)) || is.na(day) || day < 0 ||
          (field == "startDay" && !is.finite(day))) {
        stop(label, "$", field, " must be a nonnegative day; only endDay may be Inf.", call. = FALSE)
      }
      if (is.finite(day) && (day != trunc(day) || day >= 2^53 || day %% cycleLength != 0)) {
        stop(label, "$", field, " must be an exact whole day aligned with cycleLength.", call. = FALSE)
      }
      rule[[field]] <- unname(day)
    }
    if (rule$endDay <= rule$startDay) {
      stop(label, "$endDay must be greater than startDay; the upper boundary is exclusive.", call. = FALSE)
    }
    rules[[name]] <- rule
  }
  rules
}

.tmaEligibleEffects <- function(rules, population, applyAt = "transitions", stage = NULL) {
  selected <- stats::setNames(rep(NA_character_, nrow(population)),
                               as.character(population$simulation_id))
  for (name in names(rules)) {
    rule <- rules[[name]]
    if (!rule$applyAt %in% c(applyAt, "both") || !identical(rule$stage, stage)) next
    candidates <- which(population$state_label == rule$from &
      population$elapsed_day >= rule$startDay & population$elapsed_day < rule$endDay)
    if (!length(candidates)) next
    if (!is.null(rule$eligible)) {
      eligible <- tryCatch(rule$eligible(population[candidates, , drop = FALSE]), error = function(e) {
        stop("transitionEffects[['", name, "']]$eligible failed: ", conditionMessage(e), call. = FALSE)
      })
      if (!is.logical(eligible) || !is.null(dim(eligible)) ||
          length(eligible) != length(candidates) || anyNA(eligible)) {
        stop("transitionEffects[['", name,
             "']]$eligible must return one logical value per candidate source row, without NA.",
             call. = FALSE)
      }
      candidates <- candidates[eligible]
    }
    overlap <- candidates[!is.na(selected[candidates])]
    if (length(overlap)) {
      row <- overlap[1L]
      stop("Overlapping transitionEffects '", selected[row], "' and '", name, "' for ",
           population$simulation_id[row], ": at most one rule may apply to a patient at each stage or net step.",
           call. = FALSE)
    }
    selected[candidates] <- name
  }
  selected
}

.tmaApplyTransitionEffect <- function(probability, rule, states) {
  if (!is.numeric(probability) || is.complex(probability) || inherits(probability, "integer64") ||
      !is.null(dim(probability)) || is.null(names(probability)) || anyNA(names(probability)) ||
      anyDuplicated(names(probability)) || !setequal(names(probability), states) ||
      any(!is.finite(probability) | probability < 0 | probability > 1) ||
      abs(sum(probability) - 1) > 1e-10) {
    stop("A transition effect needs a complete, finite stochastic source row with every state named.",
         call. = FALSE)
  }
  if (rule$multiplier == 1 || probability[[rule$to]] == 0) return(probability)
  pairMass <- probability[[rule$to]] + probability[[rule$redistributeTo]]
  target <- probability[[rule$to]] * rule$multiplier
  if (!is.finite(target)) stop("Transition effect produces nonfinite probability mass.", call. = FALSE)
  # Permit only subtraction-scale endpoint roundoff, never a substantive excess.
  if (target > pairMass && target - pairMass <= 2 * .Machine$double.eps * pairMass) {
    target <- pairMass
  }
  balance <- pairMass - target
  if (target < 0 || target > 1 || balance < 0 || balance > 1) {
    stop("Transition effect is impossible: the target and redistributeTo probabilities must remain in [0, 1] within their available pair mass.",
         call. = FALSE)
  }
  result <- probability
  result[[rule$to]] <- target
  result[[rule$redistributeTo]] <- balance
  if (any(!is.finite(result)) || abs(sum(result) - 1) > 1e-10 ||
      abs(sum(result) - sum(probability)) > 4 * .Machine$double.eps) {
    stop("Transition effect failed to preserve finite probability mass.", call. = FALSE)
  }
  result
}

# Retain a single effect vocabulary for deterministic and sampled execution.
.tmaEffectStages <- function(rules) {
  vapply(rules, function(rule) if (is.null(rule$stage)) NA_character_ else rule$stage,
    character(1))
}

.tmaCheckEffectStages <- function(rules, stageNames = character()) {
  phases <- vapply(rules, function(rule) rule$applyAt %in% c("transitions", "both"), logical(1))
  stages <- .tmaEffectStages(rules)[phases]
  if (any(!is.na(stages)) && any(is.na(stages))) {
    stop("Do not mix net-cycle and stage-targeted transition effects in one run.", call. = FALSE)
  }
  if (any(!is.na(stages) & !stages %in% stageNames)) {
    stop("A transition effect names an unavailable stage: ",
      paste(unique(stages[!is.na(stages) & !stages %in% stageNames]), collapse = ", "), ".", call. = FALSE)
  }
  invisible(NULL)
}

.tmaEffectMatrix <- function(matrix, rules, day, states, stage = NULL,
                             source = NULL, profile = NULL, applyAt = "transitions") {
  if (is.null(source)) source <- rep(1, length(states))
  rows <- which(source > 0)
  candidates <- if (is.null(profile)) {
    data.frame(simulation_id = states[rows], state_label = states[rows], elapsed_day = day)
  } else {
    x <- profile[rep(1L, length(rows)), , drop = FALSE]
    x$state_label <- states[rows]
    x$elapsed_day <- rep(day, length(rows))
    x
  }
  ids <- .tmaEligibleEffects(rules, candidates, applyAt = applyAt, stage = stage)
  applications <- list()
  for (k in which(!is.na(ids))) {
    j <- rows[k]
    rule <- rules[[ids[k]]]
    before <- matrix[j, ]
    after <- .tmaApplyTransitionEffect(before, rule, states)
    matrix[j, ] <- after
    applications[[length(applications) + 1L]] <- data.frame(
      stage = if (is.null(stage)) NA_character_ else stage,
      source_state = states[j], effect = unname(ids[k]), to_state = rule$to,
      redistribute_to = rule$redistributeTo, multiplier = rule$multiplier,
      baseline_probability = unname(before[rule$to]), modified_probability = unname(after[rule$to]),
      baseline_redistributed_probability = unname(before[rule$redistributeTo]),
      modified_redistributed_probability = unname(after[rule$redistributeTo]),
      stringsAsFactors = FALSE)
  }
  list(matrix = matrix, applications = if (length(applications)) do.call(rbind, applications) else NULL)
}

.tmaStageCycle <- function(stages, source, rules, day, states, profile = NULL) {
  .tmaCheckEffectStages(rules, names(stages))
  probabilities <- source
  applications <- list()
  for (name in names(stages)) {
    altered <- .tmaEffectMatrix(stages[[name]], rules, day, states, stage = name,
      source = probabilities, profile = profile)
    stages[[name]] <- altered$matrix
    occupied <- which(probabilities > 0)
    if (anyNA(altered$matrix[occupied, , drop = FALSE])) {
      stop("Unestimated probabilities in reachable stage '", name, "' at day ", day, ".", call. = FALSE)
    }
    probabilities <- as.numeric(probabilities[occupied] %*% altered$matrix[occupied, , drop = FALSE])
    names(probabilities) <- states
    if (!is.null(altered$applications)) applications[[length(applications) + 1L]] <- altered$applications
  }
  list(stages = stages, probability = probabilities,
    applications = if (length(applications)) do.call(rbind, applications) else NULL)
}

.tmaStageProduct <- function(stages, states) {
  combined <- diag(length(states))
  dimnames(combined) <- list(states, states)
  for (stage in stages) for (origin in seq_along(states)) {
    p <- combined[origin, ]
    if (anyNA(p)) next
    occupied <- which(p > 0)
    combined[origin, ] <- if (anyNA(stage[occupied, , drop = FALSE])) NA_real_ else
      as.numeric(p[occupied] %*% stage[occupied, , drop = FALSE])
  }
  combined
}
