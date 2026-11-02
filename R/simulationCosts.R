# Explicit patient-level charge rules for the sampled simulator. These are
# stipulated per-label amounts, independent of empirical cost estimates and
# transition probabilities. No stage costs, discounting or proration are added.
.tmaSimulationCostRules <- function(costRules, states) {
  if (is.null(costRules)) return(list())
  if (!is.list(costRules) || is.data.frame(costRules) ||
      (length(costRules) && (is.null(names(costRules)) || anyNA(names(costRules)) ||
        any(!nzchar(trimws(names(costRules)))) || anyDuplicated(names(costRules))))) {
    stop("costRules must be a list with unique, nonempty component names.", call. = FALSE)
  }

  fields <- c("type", "amount", "states", "eligible", "startDay", "endDay", "from", "to")
  for (component in names(costRules)) {
    rule <- costRules[[component]]
    label <- paste0("costRules[['", component, "']]")
    if (!is.list(rule) || is.data.frame(rule) || is.null(names(rule)) ||
        anyNA(names(rule)) || anyDuplicated(names(rule)) ||
        !all(c("type", "amount") %in% names(rule)) || any(!names(rule) %in% fields)) {
      stop(label, " must be a named list containing type and amount, with only ",
           "states, eligible, startDay, endDay, from and to as optional fields.", call. = FALSE)
    }
    if (!is.character(rule$type) || length(rule$type) != 1L || is.na(rule$type) ||
        !rule$type %in% c("once", "recurring", "transition")) {
      stop(label, "$type must be 'once', 'recurring' or 'transition'.", call. = FALSE)
    }
    if (!is.numeric(rule$amount) || is.complex(rule$amount) ||
        inherits(rule$amount, "integer64") || !is.null(dim(rule$amount)) ||
        length(rule$amount) != 1L || !is.finite(rule$amount)) {
      stop(label, "$amount must be one finite numeric amount; credits may be negative.",
           call. = FALSE)
    }
    rule$amount <- unname(rule$amount)
    if (!is.null(rule$states) &&
        (!is.character(rule$states) || !is.null(dim(rule$states)) || !length(rule$states) ||
         anyNA(rule$states) || any(!rule$states %in% states) || anyDuplicated(rule$states))) {
      stop(label, "$states must contain unique, known state labels.", call. = FALSE)
    }
    if (!is.null(rule$eligible) && !is.function(rule$eligible)) {
      stop(label, "$eligible must be a function of the destination population.", call. = FALSE)
    }
    if (is.null(rule$startDay)) rule$startDay <- 0
    if (is.null(rule$endDay)) rule$endDay <- Inf
    for (field in c("startDay", "endDay")) {
      day <- rule[[field]]
      if (!is.numeric(day) || is.complex(day) || inherits(day, "integer64") ||
          !is.null(dim(day)) || length(day) != 1L || is.na(day) || day < 0 ||
          (field == "startDay" && !is.finite(day))) {
        stop(label, "$", field, " must be one nonnegative numeric day; only endDay may be Inf.",
             call. = FALSE)
      }
      rule[[field]] <- unname(day)
    }
    if (rule$endDay <= rule$startDay) {
      stop(label, "$endDay must be greater than startDay; the upper boundary is exclusive.",
           call. = FALSE)
    }
    if (rule$type == "transition") {
      for (field in c("from", "to")) {
        value <- rule[[field]]
        if (!is.character(value) || length(value) != 1L || is.na(value) ||
            !value %in% states) {
          stop(label, "$", field, " must name one known state for a transition rule.",
               call. = FALSE)
        }
      }
      if (rule$from == rule$to) {
        stop(label, " cannot charge a transition self-pair; use a recurring rule for residency.",
             call. = FALSE)
      }
    } else if (any(c("from", "to") %in% names(rule))) {
      stop(label, " may use from and to only with type = 'transition'.", call. = FALSE)
    }
    costRules[[component]] <- rule
  }
  costRules
}

# The caller passes one population snapshot per new labelled cycle, including
# cycle zero. Preserve charged between calls/resume so that a once rule applies
# only at each simulation_id's first eligible label. Eligibility callbacks see
# destination rows that pass the state, time, type and prior-charge restrictions,
# with all current covariates. Do not invoke callbacks when no candidates remain.
.tmaSimulationCharge <- function(population, previous = NULL, rules,
                                  charged = list(), terminalStates = character()) {
  ledger <- data.frame(simulation_id = character(), cycle = numeric(),
                       elapsed_day = numeric(), component = character(),
                       type = character(), state_label = character(), amount = numeric(),
                       stringsAsFactors = FALSE)
  if (is.null(charged)) charged <- list()
  if (!length(rules)) return(list(ledger = ledger, charged = charged))
  ids <- as.character(population$simulation_id)
  previousState <- NULL
  if (!is.null(previous)) {
    previousIndex <- match(ids, as.character(previous$simulation_id))
    if (anyNA(previousIndex) || anyDuplicated(previous$simulation_id)) {
      stop("The previous cost snapshot must contain each current simulation_id exactly once.",
           call. = FALSE)
    }
    previousState <- as.character(previous$state_label[previousIndex])
  }
  pieces <- vector("list", length(rules))
  for (i in seq_along(rules)) {
    component <- names(rules)[i]
    rule <- rules[[i]]
    eligible <- population$elapsed_day >= rule$startDay & population$elapsed_day < rule$endDay
    if (!is.null(rule$states)) eligible <- eligible & population$state_label %in% rule$states
    if (rule$type == "transition") {
      if (is.null(previousState)) {
        eligible[] <- FALSE
      } else {
        eligible <- eligible & previousState == rule$from & population$state_label == rule$to &
          previousState != population$state_label & !previousState %in% terminalStates
      }
    } else {
      eligible <- eligible & !population$state_label %in% terminalStates
    }
    if (rule$type == "once") {
      if (is.null(charged[[component]])) charged[[component]] <- character()
      eligible <- eligible & !ids %in% charged[[component]]
    }
    selected <- which(eligible)
    if (length(selected) && !is.null(rule$eligible)) {
      callback <- tryCatch(rule$eligible(population[selected, , drop = FALSE]), error = function(e) {
        stop("costRules[['", component, "']]$eligible failed: ", conditionMessage(e),
             call. = FALSE)
      })
      if (!is.logical(callback) || !is.null(dim(callback)) ||
          length(callback) != length(selected) || anyNA(callback)) {
        stop("costRules[['", component,
             "']]$eligible must return one logical value per candidate population row, without NA.",
             call. = FALSE)
      }
      selected <- selected[callback]
    }
    if (rule$type == "once") {
      charged[[component]] <- c(charged[[component]], ids[selected])
    }
    if (length(selected)) {
      pieces[[i]] <- data.frame(
        simulation_id = ids[selected], cycle = as.numeric(population$cycle[selected]),
        elapsed_day = as.numeric(population$elapsed_day[selected]), component = component,
        type = rule$type, state_label = as.character(population$state_label[selected]),
        amount = rule$amount, stringsAsFactors = FALSE)
    }
  }
  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces)) ledger <- do.call(rbind, pieces)
  rownames(ledger) <- NULL
  list(ledger = ledger, charged = charged)
}
