#' Simulate patient snapshots through a model bundle
#'
#' @param bundle A model bundle from createModelBundle(). An explicit cost model
#'   created with createCostModel() is charged automatically when retained in
#'   bundle$costModel. Raw descriptive bundle$costEstimates are not charged.
#' @param population Optional data frame with one current snapshot per patient.
#'   Supply state_label unless initial overrides states. Optional subject_id must
#'   be unique and is retained as source_subject_id. Generated simulation_id
#'   values identify draws, including repeated draws of the same source patient.
#'   elapsed_day and cycle default to zero. elapsed_day must align with the
#'   bundle's cycleLength since index_date; cycle may be a relative counter.
#'   Dates must have class Date.
#' @param n Number of simulated patients. With population, sample n profiles,
#'   using replace. Without population, n and initial are required.
#' @param initial Optional named state probability vector, or, without
#'   population, a joint distribution data frame containing state_label,
#'   probability and required covariates. Probabilities must sum to one.
#'   A named vector deliberately replaces sampled patients' states independently
#'   of their covariates. Without population it is allowed only for an
#'   unstratified bundle. No training population is used automatically.
#'   All positive-probability joint profiles, and all population profiles whose
#'   states are not overridden, must have supported initial nonterminal strata
#'   before sampling, including profiles that are not drawn.
#' @param nSteps Maximum number of additional transition steps, including zero.
#' @param updateCovariates Optional function(current, previous), called after
#'   advancing time on nonterminal patients only. Return a data frame containing
#'   simulation_id and only covariate columns to change; a subset of these IDs
#'   is allowed. IDs, states, dates defining age, cycle, elapsed_day and derived
#'   stratum IDs cannot be changed. Grouping outputs and age derived from
#'   birth_date are owned by the bundle; update raw grouping inputs instead.
#'   current omits stratum_id until updates finish; previous retains the source
#'   stratum_id. Use named covariate values to define eligibility. Callbacks
#'   receive current snapshots, so any
#'   dependence on earlier history must be represented explicitly in covariates.
#' @param costRules Optional named list of rules with type (entry, once, recurring
#'   or transition), a finite amount, optional states, eligible function of candidate
#'   rows, and startDay/endDay (lower inclusive, upper
#'   exclusive). Transition rules additionally require from and to state labels
#'   and charge actual state changes. Entry fees require an entry model and use
#'   pre-entry nonterminal profiles, before outcomes are drawn. Other rules use
#'   destination rows. See \link{simulation-cost-rules}. Rules are
#'   additional to an explicit bundle cost model. The component name healthcare
#'   is reserved when a cost model is present. No mean-cost calibration is fitted.
#' @param currency Currency label required when costRules is nonempty. Defaults
#'   to the bundle cost model's currency when present; conflicting labels fail.
#' @param priceYear Positive integer price year required with costRules. Defaults
#'   to the bundle cost model's price year when present; conflicts fail. Amounts
#'   are summed as supplied, without inflation adjustment or discounting.
#' @param terminalStates Explicit subset of the bundle's absorbing states.
#'   Terminal patients stop moving and changing covariates. Mere self-transitions
#'   and absorbing states outside this set do not stop simulation. Every source
#'   state in an explicit cost model's terminal mapping must be included.
#' @param stopWhen Optional function(population, step) returning one nonmissing
#'   logical value, checked at initialization and after each completed step.
#' @param seed Optional nonnegative integer seed. Simulation and callbacks use
#'   private RNG state and restore the caller's RNG kind and state on exit.
#'   NULL starts a fresh private stream, independently of the caller's set.seed;
#'   pass an explicit seed for reproducible initialization.
#'   Box-Muller normal generation is unsupported because its cached normal draw
#'   cannot be saved in an R checkpoint. It is rejected before changing the
#'   caller's RNG. User-supplied RNG kinds are also unsupported because their
#'   state cannot be restored reliably. Callbacks must not change RNGkind().
#' @param replace Whether sampling population profiles is with replacement.
#' @param transitionEffects Optional named list of source-transition rules. Each
#'   rule supplies from, to, multiplier and redistributeTo, with optional
#'   eligible(sourceCandidates), startDay, endDay and applyAt. applyAt is
#'   "transitions" by default, or "entry" or "both" with an entry model.
#'   Eligibility uses current
#'   source snapshots and a lower-inclusive, upper-exclusive source-day window.
#'   Multiply the probability of to and move the exact difference to
#'   redistributeTo. At most one rule may apply per patient per step. These are
#'   analyst-specified effects on discrete full-cycle probabilities, not hazards
#'   or effects estimated from the fitted trajectories. Invalid probability rows
#'   fail rather than being clipped or normalized.
#' @param entry Optional result from estimateEntryTransitions(), applied once
#'   to explicitly supplied baseline profiles before the cycle-zero snapshot.
#'   Supply population or n and initial as usual; no training population or
#'   training baseline mix is used automatically. All supplied profiles must
#'   start at elapsed_day = 0 and cycle = 0. Every positive-probability baseline
#'   state must have an estimated entry row, including profiles not sampled;
#'   states replaced by a named initial vector are ignored. A stratified entry
#'   model selects its matrix from each patient's baseline covariate combination.
#'   Supply these covariates in population or a joint initial distribution. Entry
#'   strata can differ from the bundle's later transition and cost strata. Bundle
#'   grouping rules are applied first. Entry-only groups must be supplied as
#'   columns, for example using addCovariateGroups() before simulation. No pooled
#'   fallback is used for unsupported baseline combinations or unestimated rows.
#'   Without entry strata, probabilities remain pooled within entry state.
#' @details Every step selects the model using current covariates and the period
#'   containing each patient's source elapsed_day. It then samples a new state,
#'   advances elapsed_day by cycleLength and cycle by one, updates covariates,
#'   recomputes groups and charges the resulting cycle label. Unavailable
#'   source probability rows or source strata cause an error. A destination may
#'   have no matching model: its stratum_id is NA and its completed step and
#'   costs are retained. The next step fails if that snapshot is used as a
#'   source. metadata$unsupported_current counts such nonterminal patients.
#'
#'   If birth_date and index_date are supplied, completed age is recomputed at
#'   index_date + elapsed_day for nonterminal patients. Without birth_date,
#'   numeric age stays unchanged unless updateCovariates changes it; no birthday
#'   is inferred from a baseline age. Grouping rules are reapplied after updates.
#'   Terminal covariates remain fixed; their cycle/day labels advance while other
#'   patients continue, but their later rows incur no costs. Transitions record
#'   actual draws only. Terminal destinations retain their last stratum and have
#'   no destination period because no further model is needed.
#'
#'   k completed steps produce k + 1 labelled snapshots per patient, including
#'   initial charges at cycle zero (or the supplied starting cycle), consistent
#'   with projectCosts(). Estimated healthcare charges select the destination
#'   label's clinical or mapped cost state, current cost covariates and cost
#'   period. Daily models split a labelled cycle across cost-period boundaries;
#'   cycle models apply one estimated cycle amount. Cost periods can differ from
#'   transition periods. Initial terminal patients incur no estimated healthcare
#'   charge. With the cost model's terminal = "entry", a newly terminal label is
#'   charged once using its retained source covariates and destination elapsed
#'   day; terminal = "none" suppresses this charge. Explicit transition rules
#'   can also charge terminal entry. Persistent terminal rows incur no further
#'   charges. All-terminal or custom stopping is checked after charges. No files,
#'   plots, database queries or connections are created.
#'
#'   With entry, baseline covariates and groups are derived before drawing one
#'   entry outcome per nonterminal patient. Baseline terminal patients remain
#'   unchanged. Entry adds no elapsed time, snapshot or transition step, calls
#'   no updateCovariates callback, and is not repeated by advanceSimulation().
#'   Entry outcomes label cycle zero; they do not reconstruct event times or
#'   intermediate states. Stratified entry selects its model only at baseline,
#'   without changing later dynamic model selection. Independent initial states
#'   are checked against every supplied profile before sampling. Baseline terminal
#'   patients do not need entry covariates because no entry model is applied.
#'   Initial charges use the post-entry outcome and the
#'   pre-entry source, so explicit transition rules and estimated terminal-entry
#'   charges can apply. The baseline is not charged an additional cycle. Once
#'   and recurring rules still apply only to nonterminal outcome labels.
#'   A type = "entry" fee is assessed on pre-entry nonterminal profiles before
#'   the entry draw. It is retained even when the patient becomes terminal
#'   during entry, and is not charged again on resume. Its ledger state_label
#'   records the pre-entry state, not the outcome. Fee and effect eligibility
#'   are separate rules; use a shared baseline covariate to link them.
#'
#'   Transition effects apply after selecting the source stratum and period and
#'   before drawing the next state. Their callbacks run in a batch before the
#'   step's draws and receive candidate source snapshots. Effects do not modify
#'   the saved bundle, initial distribution or cost rules. For composed models,
#'   effects act on the combined full-cycle row, not on an individual stage.
#'   Identity multipliers preserve draws and RNG state when eligibility is
#'   deterministic. This does not establish paired random draws across scenarios
#'   whose state-dependent paths or callbacks consume different random numbers.
#'   Entry effects require applyAt = "entry" or "both" and use baseline
#'   covariates and the entry matrix. Entry and the first adjacent step both
#'   have source day zero. In a weekly model, applyAt = "both", endDay = 28
#'   therefore affects entry and four adjacent steps, not four outcome labels.
#'   For four affected labels, use a separate entry rule and a transitions rule
#'   ending at source day 21. Saved matrices remain unchanged.
#' @return A tma_simulation list with final population, trajectories,
#'   transitions, costs (charge ledger), totals by component, and metadata.
#'   Internal configuration and RNG checkpoints support advanceSimulation().
#'   With nonempty transitionEffects, effects records each eligible application;
#'   transitions additionally records baseline_transition_probability for its
#'   sampled destination and effect (NA when none applies). The existing
#'   transition_probability is the probability actually used for sampling.
#'   With entry, entry retains the sampled pre-entry population, actual entry
#'   transitions, validated matrix and aggregate estimation metadata. It does
#'   not retain training subject identifiers. The main trajectories start with
#'   post-entry outcomes at cycle zero. Stratified entry instead retains matrices,
#'   the baseline strata registry and entry_stratum_id on each actual entry draw.
#'   Entry effects retain their own audit
#'   under entry$effects and rules under entry$effectRules. Entry transitions
#'   record actual and baseline sampled probabilities when entry effects exist.
#'   Stratified entry effect audits include entry_stratum_id separately from the
#'   later bundle's source_stratum.
#' @seealso \link{transition-effects} for rule fields and probability handling.
#'   summariseSimulation() for costs, life-years, QALYs and explicit discounting.
#' @export
simulateTrajectories <- function(bundle, population = NULL, n = NULL,
                                 initial = NULL, nSteps = 1,
                                 updateCovariates = NULL, costRules = NULL,
                                 currency = NULL, priceYear = NULL,
                                 terminalStates = character(), stopWhen = NULL,
                                 seed = NULL, replace = FALSE,
                                 transitionEffects = NULL, entry = NULL) {
  bundle <- .tmaValidateModelBundle(bundle)
  entry <- .tmaSimulationEntryModel(entry, bundle)
  nSteps <- .tmaProjectionInteger(nSteps, "nSteps", 0)
  if (!is.null(n)) n <- .tmaProjectionInteger(n, "n", 1)
  if (!is.logical(replace) || length(replace) != 1L || is.na(replace)) {
    stop("replace must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(updateCovariates) && !is.function(updateCovariates)) {
    stop("updateCovariates must be a function or NULL.", call. = FALSE)
  }
  if (!is.null(stopWhen) && !is.function(stopWhen)) {
    stop("stopWhen must be a function or NULL.", call. = FALSE)
  }
  if (!is.character(terminalStates) || anyNA(terminalStates) ||
      anyDuplicated(terminalStates) ||
      any(!terminalStates %in% bundle$metadata$absorbingStates)) {
    stop("terminalStates must be unique declared absorbing states.", call. = FALSE)
  }
  rules <- .tmaSimulationCostRules(costRules, bundle$metadata$states)
  effects <- .tmaTransitionEffects(transitionEffects, bundle$metadata$states,
    bundle$metadata$absorbingStates, bundle$metadata$cycleLength)
  needsEntry <- any(vapply(rules, function(rule) rule$type == "entry", logical(1))) ||
    any(vapply(effects, function(rule) rule$applyAt %in% c("entry", "both"), logical(1)))
  if (needsEntry && is.null(entry)) {
    stop("Entry fees and entry/both transition effects require an explicit entry model.", call. = FALSE)
  }
  costModel <- bundle$costModel
  if (!is.null(costModel)) {
    if (any(!names(costModel$metadata$terminal_cost_map) %in% terminalStates)) {
      stop("Include every cost model terminal-cost mapping source in terminalStates.", call. = FALSE)
    }
    if ("healthcare" %in% names(rules)) {
      stop("The costRules name 'healthcare' is reserved when the bundle has a cost model.", call. = FALSE)
    }
    if (is.null(currency)) currency <- costModel$metadata$currency
    if (is.null(priceYear)) priceYear <- costModel$metadata$priceYear
  }
  if (length(rules) || !is.null(currency)) {
    if (!is.character(currency) || length(currency) != 1L || is.na(currency) ||
        !nzchar(trimws(currency))) {
      stop("Supply a nonempty currency label when using costRules.", call. = FALSE)
    }
  }
  if (length(rules) || !is.null(priceYear)) {
    priceYear <- .tmaProjectionInteger(priceYear, "priceYear", 1)
  }
  if (!is.null(costModel) && (currency != costModel$metadata$currency ||
      priceYear != costModel$metadata$priceYear)) {
    stop("currency and priceYear must match the bundle cost model; explicit rules use those same labels.",
      call. = FALSE)
  }
  .tmaSimulationRng(seed = seed, run = function() {
    population <- .tmaSimulationInitial(bundle, population, n, initial, replace,
      terminalStates, entry)
    population$stratum_id <- rep(NA_character_, nrow(population))
    active <- which(!population$state_label %in% terminalStates)
    if (length(active)) {
      population <- .tmaSimulationMergeRows(population, active,
        .tmaSimulationCovariates(bundle, population[active, , drop = FALSE]))
      population$stratum_id[active] <- .tmaBundleModel(bundle,
        population[active, , drop = FALSE])
    }
    baseline <- NULL
    entryResult <- NULL
    prepaid <- NULL
    if (!is.null(entry)) {
      baseline <- population
      prepaid <- .tmaSimulationCharge(baseline, rules = rules,
        terminalStates = terminalStates, phase = "entry")
      entryResult <- .tmaSimulationApplyEntry(bundle, baseline, entry, terminalStates, effects)
      population <- entryResult$population
      entryResult <- entryResult$entry
    }
    initialCharge <- .tmaSimulationCharge(population, baseline, rules,
      if (is.null(prepaid)) list() else prepaid$charged, terminalStates)
    if (!is.null(prepaid)) {
      initialCharge$ledger <- .tmaSimulationBind(list(prepaid$ledger, initialCharge$ledger))
    }
    if (!is.null(costModel)) {
      initialCharge$ledger <- .tmaSimulationBind(list(initialCharge$ledger,
        .tmaModelCostCharge(costModel, population, previous = baseline,
          terminalStates = terminalStates, cycleLength = bundle$metadata$cycleLength)))
    }
    initialisation <- if (is.data.frame(initial)) "joint_distribution" else if (
      !is.null(initial)) "independent_state_distribution" else "population_states"
    if (!is.null(entryResult)) entryResult$metadata$population_source <- initialisation
    simulation <- structure(list(
      population = population, trajectories = population,
      transitions = .tmaSimulationEmptyTransitions(), costs = initialCharge$ledger,
      totals = .tmaSimulationTotals(initialCharge$ledger, rules, costModel),
      metadata = list(n = nrow(population), steps = 0, cycle = max(population$cycle),
        cycleLength = bundle$metadata$cycleLength, terminalStates = terminalStates,
        stop_reason = "max_steps", unsupported_current = 0L,
        seed = seed, currency = currency, priceYear = priceYear,
        initialisation = if (is.null(entryResult)) initialisation else "entry_model",
        cost_timing = if (any(vapply(rules, function(rule) rule$type == "entry", logical(1))))
          "explicit pre-entry fees, initial label and each completed destination label" else
          "initial label and each completed destination label",
        period_assignment = "source elapsed_day"),
      .config = list(bundle = bundle, updateCovariates = updateCovariates,
        costRules = rules, currency = currency, priceYear = priceYear,
        terminalStates = terminalStates, stopWhen = stopWhen),
      .state = list(charged = initialCharge$charged, rng = NULL)
    ), class = "tma_simulation")
    if (!is.null(entryResult)) simulation$entry <- entryResult
    if (!is.null(costModel)) {
      simulation$metadata$cost_model <- costModel$metadata
    }
    if (length(effects)) {
      simulation$.config$transitionEffects <- effects
      simulation$effects <- .tmaSimulationEmptyEffects()
      simulation$transitions$baseline_transition_probability <- numeric()
      simulation$transitions$effect <- character()
      simulation$metadata$transition_effects <- list(
        rules = names(effects),
        applyAt = vapply(effects, function(rule) rule$applyAt, character(1)),
        estimand = "specified multiplier on a discrete full-cycle transition probability",
        application = "source covariates and source-day window, after model selection and before drawing",
        redistribution = "exact probability difference assigned to the declared redistribution state",
        stages = "combined full-cycle probabilities; stage models remain unchanged")
    }
    .tmaSimulationRun(simulation, nSteps, checkInitial = TRUE)
  })
}

#' Continue a saved patient simulation
#'
#' @param simulation A result from simulateTrajectories() or advanceSimulation().
#' @param nSteps Maximum additional steps, a finite nonnegative integer.
#' @details Resumes the saved cost and transition-effect rules, callbacks,
#'   charged-component memory and
#'   private RNG checkpoint. The starting snapshot is not charged again.
#'   Splitting a run across calls gives the same draws as an uninterrupted run,
#'   provided callbacks depend on their arguments and saved environments rather
#'   than mutable external state. An already checked checkpoint is not checked
#'   again before its next step. A simulation
#'   completed by terminal or custom stopping is returned unchanged. The input
#'   object and caller RNG state are not modified. Box-Muller RNG callers and
#'   checkpoints are unsupported; callbacks must not change RNGkind().
#' @return An updated tma_simulation with appended histories and charge ledger.
#' @export
advanceSimulation <- function(simulation, nSteps = 1) {
  nSteps <- .tmaProjectionInteger(nSteps, "nSteps", 0)
  if (!inherits(simulation, "tma_simulation") || !is.list(simulation$.config) ||
      !is.list(simulation$.state) || !is.list(simulation$.state$rng)) {
    stop("simulation must be a saved result from simulateTrajectories().", call. = FALSE)
  }
  simulation$.config$bundle <- .tmaValidateModelBundle(simulation$.config$bundle)
  if (!is.null(simulation$.config$transitionEffects)) {
    model <- simulation$.config$bundle$metadata
    simulation$.config$transitionEffects <- .tmaTransitionEffects(
      simulation$.config$transitionEffects, model$states, model$absorbingStates,
      model$cycleLength)
  }
  if (simulation$metadata$stop_reason %in% c("all_terminal", "stop_when")) return(simulation)
  .tmaSimulationRng(state = simulation$.state$rng, run = function() {
    .tmaSimulationRun(simulation, nSteps, checkInitial = FALSE)
  })
}

.tmaSimulationInitial <- function(bundle, population, n, initial, replace,
                                  terminalStates, entry = NULL) {
  states <- bundle$metadata$states
  joint <- is.data.frame(initial)
  if (!is.null(population) && joint) {
    stop("A joint initial distribution cannot be combined with population.", call. = FALSE)
  }
  if (!joint && !is.null(initial)) {
    .tmaSimulationProbabilities(initial, states)
  }
  if (is.null(population)) {
    if (is.null(n) || is.null(initial)) {
      stop("Without population, supply n and an explicit initial distribution.", call. = FALSE)
    }
    if (n > .Machine$integer.max) stop("n exceeds supported population indexing.", call. = FALSE)
    if (joint) {
      population <- .tmaSimulationData(initial, "initial")
      if (!all(c("state_label", "probability") %in% names(population))) {
        stop("Joint initial must contain state_label, covariates and probability.", call. = FALSE)
      }
      .tmaSimulationProbabilities(population$probability)
      if (!is.character(population$state_label) || anyNA(population$state_label) ||
          any(!population$state_label %in% states)) {
        stop("Joint initial state_label must contain known states, including zero-probability rows.", call. = FALSE)
      }
      if ("subject_id" %in% names(population)) {
        stop("Joint initial must describe covariate profiles without subject_id identifiers.", call. = FALSE)
      }
      population <- .tmaSimulationTiming(population, bundle$metadata$cycleLength)
      .tmaSimulationEntryProfiles(entry, population, bundle = bundle,
        terminalStates = terminalStates, probability = population$probability)
      .tmaSimulationValidateProfiles(bundle,
        population[population$probability > 0, , drop = FALSE], terminalStates)
      population <- population[sample.int(nrow(population), n, replace = TRUE,
        prob = population$probability), , drop = FALSE]
      population$probability <- NULL
    } else {
      .tmaSimulationEntryProfiles(entry, states = names(initial)[initial > 0],
        terminalStates = terminalStates)
      if (length(union(bundle$metadata$strata, bundle$costModel$metadata$strata))) {
        stop("A stratified bundle needs population covariates or a joint initial distribution.", call. = FALSE)
      }
      population <- data.frame(state_label = rep(NA_character_, n), stringsAsFactors = FALSE)
    }
  } else {
    population <- .tmaSimulationData(population, "population")
    if (is.null(initial) && (!"state_label" %in% names(population) ||
        !is.character(population$state_label) || anyNA(population$state_label) ||
        any(!population$state_label %in% states))) {
      stop("population state_label must contain known, nonmissing state labels.", call. = FALSE)
    }
    population <- .tmaSimulationTiming(population, bundle$metadata$cycleLength)
    .tmaSimulationEntryProfiles(entry, population,
      states = if (is.null(initial)) NULL else names(initial)[initial > 0],
      bundle = bundle, terminalStates = terminalStates)
    if ("subject_id" %in% names(population)) {
      ids <- .tmaSubjectIds(population$subject_id, "population$subject_id")
      if (anyDuplicated(ids)) {
        stop("population must contain one snapshot per unique subject_id.", call. = FALSE)
      }
      population$subject_id <- ids
    }
    if (is.null(initial)) .tmaSimulationValidateProfiles(bundle, population, terminalStates)
    if (!is.null(n)) {
      if (n > .Machine$integer.max || (!replace && n > nrow(population))) {
        stop("n exceeds the available population; use replace = TRUE for repeated draws.", call. = FALSE)
      }
      population <- population[sample.int(nrow(population), n, replace = replace), , drop = FALSE]
    }
  }
  if (any(c("simulation_id", "source_subject_id", "stratum_id") %in% names(population))) {
    stop("population must not supply generated simulation_id, source_subject_id or stratum_id fields.", call. = FALSE)
  }
  if (!joint && !is.null(initial)) {
    population$state_label <- names(initial)[sample.int(length(initial), nrow(population),
      replace = TRUE, prob = initial)]
  }
  if (!"state_label" %in% names(population) || !is.character(population$state_label) ||
      anyNA(population$state_label) || any(!population$state_label %in% states)) {
    stop("population state_label must contain known, nonmissing state labels.", call. = FALSE)
  }
  population <- .tmaSimulationTiming(population, bundle$metadata$cycleLength)
  if ("subject_id" %in% names(population)) {
    population$source_subject_id <- population$subject_id
    population$subject_id <- NULL
  }
  population$simulation_id <- paste0("sim_", seq_len(nrow(population)))
  population <- population[c("simulation_id", setdiff(names(population), "simulation_id"))]
  rownames(population) <- NULL
  population
}

.tmaSimulationValidateProfiles <- function(bundle, population, terminalStates) {
  active <- which(!population$state_label %in% terminalStates)
  if (length(active)) {
    current <- .tmaSimulationCovariates(bundle, population[active, , drop = FALSE])
    .tmaBundleModel(bundle, current)
  }
  invisible(NULL)
}

.tmaSimulationTiming <- function(population, cycleLength) {
  for (field in c("elapsed_day", "cycle")) {
    if (!field %in% names(population)) population[[field]] <- rep(0, nrow(population))
    values <- population[[field]]
    if (!is.numeric(values) || is.complex(values) || inherits(values, "integer64") ||
        anyNA(values) || any(!is.finite(values) | values < 0 | values != trunc(values) | values >= 2^53)) {
      stop("population$", field, " must contain exact nonnegative integers.", call. = FALSE)
    }
  }
  if (any(population$elapsed_day %% cycleLength != 0)) {
    stop("population$elapsed_day must align with the bundle cycleLength since index_date.", call. = FALSE)
  }
  for (field in intersect(c("index_date", "birth_date"), names(population))) {
    if (!inherits(population[[field]], "Date") || anyNA(population[[field]]) ||
        any(!is.finite(as.numeric(population[[field]]))) ||
        any(as.numeric(population[[field]]) != trunc(as.numeric(population[[field]])))) {
      stop("population$", field, " must contain finite, nonmissing Date values.", call. = FALSE)
    }
  }
  if ("birth_date" %in% names(population) && !"index_date" %in% names(population)) {
    stop("birth_date requires index_date to update age.", call. = FALSE)
  }
  if ("birth_date" %in% names(population) && any(population$birth_date > population$index_date)) {
    stop("birth_date cannot be after index_date.", call. = FALSE)
  }
  population
}

.tmaSimulationProbabilities <- function(probability, states = NULL) {
  if (!is.numeric(probability) || is.complex(probability) ||
      inherits(probability, "integer64") || !is.null(dim(probability)) ||
      !length(probability) || anyNA(probability) ||
      any(!is.finite(probability) | probability < 0 | probability > 1) ||
      abs(sum(probability) - 1) > 1e-10) {
    stop("initial probabilities must be finite in [0, 1] and sum to 1 within 1e-10.", call. = FALSE)
  }
  if (!is.null(states) && (is.null(names(probability)) || anyNA(names(probability)) ||
      anyDuplicated(names(probability)) || any(!names(probability) %in% states))) {
    stop("initial must name each supplied known state uniquely.", call. = FALSE)
  }
  invisible(probability)
}

.tmaSimulationData <- function(data, name, allowEmpty = FALSE) {
  if (!is.data.frame(data) || (!allowEmpty && !nrow(data)) ||
      !length(names(data)) || anyNA(names(data)) || anyDuplicated(names(data)) ||
      any(!nzchar(trimws(names(data))))) {
    stop(name, " must be a data frame with unique column names and rows.", call. = FALSE)
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  for (field in names(data)) {
    if (is.factor(data[[field]])) data[[field]] <- as.character(data[[field]])
    if (!is.atomic(data[[field]]) || !is.null(dim(data[[field]]))) {
      stop(name, " columns must be atomic vectors; invalid column: ", field, ".", call. = FALSE)
    }
  }
  data
}

.tmaSimulationRun <- function(simulation, nSteps, checkInitial) {
  configuration <- simulation$.config
  bundle <- configuration$bundle
  cycleLength <- bundle$metadata$cycleLength
  effects <- configuration$transitionEffects
  population <- simulation$population
  if (nrow(simulation$trajectories) + nrow(population) * nSteps > .Machine$integer.max ||
      max(population$elapsed_day) + nSteps * cycleLength >= 2^53 ||
      max(population$cycle) + nSteps >= 2^53) {
    stop("The requested horizon exceeds supported output indexing or exact elapsed days.", call. = FALSE)
  }
  reason <- if (checkInitial) .tmaSimulationStop(population, simulation$metadata$steps,
    configuration) else NULL
  trajectoryRows <- list()
  transitionRows <- list()
  effectRows <- list()
  costRows <- list()
  if (is.null(reason)) for (i in seq_len(nSteps)) {
    previous <- population
    active <- which(!previous$state_label %in% configuration$terminalStates)
    sourceStratum <- .tmaBundleModel(bundle, previous[active, , drop = FALSE])
    sourcePeriod <- character(length(active))
    drawProbability <- numeric(length(active))
    if (length(effects)) {
      sourcePeriod <- bundle$metadata$periods$period[
        .tmaPeriodIndex(previous$elapsed_day[active], bundle$metadata$periods)]
      effectIds <- .tmaEligibleEffects(effects, previous[active, , drop = FALSE])
      baselineDraw <- numeric(length(active))
      baselineTarget <- modifiedTarget <- baselineRedistributed <- modifiedRedistributed <-
        rep(NA_real_, length(active))
    }
    for (j in seq_along(active)) {
      row <- active[j]
      model <- bundle$models[[sourceStratum[j]]]
      period <- .tmaPeriodIndex(previous$elapsed_day[row], model$periods)
      sourcePeriod[j] <- model$periods$period[period]
      probability <- model$matrices[[sourcePeriod[j]]][previous$state_label[row], ]
      if (anyNA(probability)) {
        stop("Unestimated transition row for state '", previous$state_label[row],
          "' in stratum '", sourceStratum[j], "', period '", sourcePeriod[j],
          "' for ", previous$simulation_id[row], ".", call. = FALSE)
      }
      if (length(effects)) {
        baseline <- probability
        if (!is.na(effectIds[j])) {
          rule <- effects[[effectIds[j]]]
          probability <- tryCatch(
            .tmaApplyTransitionEffect(baseline, rule, bundle$metadata$states),
            error = function(error) stop("Transition effect '", effectIds[j], "' failed for ",
              previous$simulation_id[row], " in source stratum '", sourceStratum[j],
              "', period '", sourcePeriod[j], "' at day ", previous$elapsed_day[row],
              ": ", conditionMessage(error), call. = FALSE))
          baselineTarget[j] <- baseline[rule$to]
          modifiedTarget[j] <- probability[rule$to]
          baselineRedistributed[j] <- baseline[rule$redistributeTo]
          modifiedRedistributed[j] <- probability[rule$redistributeTo]
        }
      }
      destination <- sample.int(length(probability), 1L, prob = probability)
      population$state_label[row] <- bundle$metadata$states[destination]
      drawProbability[j] <- probability[destination]
      if (length(effects)) baselineDraw[j] <- baseline[destination]
    }
    if (length(effects)) {
      applied <- which(!is.na(effectIds))
      if (length(applied)) {
        effectRules <- effects[effectIds[applied]]
        rows <- active[applied]
        effectRows[[length(effectRows) + 1L]] <- data.frame(
          simulation_id = previous$simulation_id[rows], source_cycle = previous$cycle[rows],
          source_day = previous$elapsed_day[rows], source_state = previous$state_label[rows],
          source_stratum = sourceStratum[applied], source_period = sourcePeriod[applied],
          effect = unname(effectIds[applied]),
          to_state = vapply(effectRules, function(rule) rule$to, character(1)),
          redistribute_to = vapply(effectRules, function(rule) rule$redistributeTo, character(1)),
          multiplier = vapply(effectRules, function(rule) rule$multiplier, numeric(1)),
          baseline_probability = baselineTarget[applied], modified_probability = modifiedTarget[applied],
          baseline_redistributed_probability = baselineRedistributed[applied],
          modified_redistributed_probability = modifiedRedistributed[applied],
          row.names = NULL, stringsAsFactors = FALSE)
      }
    }
    population$elapsed_day <- previous$elapsed_day + cycleLength
    population$cycle <- previous$cycle + 1
    alive <- which(!population$state_label %in% configuration$terminalStates)
    if (length(alive)) {
      population <- .tmaSimulationMergeRows(population, alive,
        .tmaSimulationCovariates(bundle, population[alive, , drop = FALSE]))
      if (!is.null(configuration$updateCovariates)) {
        population <- .tmaSimulationUpdate(population, previous, alive, configuration)
      }
      population <- .tmaSimulationMergeRows(population, alive,
        .tmaSimulationCovariates(bundle, population[alive, , drop = FALSE]))
      population$stratum_id[alive] <- .tmaBundleModel(bundle,
        population[alive, , drop = FALSE], allowUnmatched = TRUE)
    }
    destinationPeriod <- rep(NA_character_, length(active))
    for (j in which(active %in% alive)) {
      row <- active[j]
      destinationPeriod[j] <- bundle$metadata$periods$period[
        .tmaPeriodIndex(population$elapsed_day[row], bundle$metadata$periods)]
    }
    transitionRows[[length(transitionRows) + 1L]] <- data.frame(
      simulation_id = population$simulation_id[active],
      source_cycle = previous$cycle[active], destination_cycle = population$cycle[active],
      source_day = previous$elapsed_day[active], destination_day = population$elapsed_day[active],
      source_state = previous$state_label[active], destination_state = population$state_label[active],
      source_stratum = sourceStratum, destination_stratum = population$stratum_id[active],
      source_period = sourcePeriod, destination_period = destinationPeriod,
      transition_probability = drawProbability, stringsAsFactors = FALSE)
    if (length(effects)) {
      currentStep <- length(transitionRows)
      transitionRows[[currentStep]]$baseline_transition_probability <- baselineDraw
      transitionRows[[currentStep]]$effect <- unname(effectIds)
    }
    charge <- .tmaSimulationCharge(population, previous, configuration$costRules,
      simulation$.state$charged, configuration$terminalStates)
    if (!is.null(bundle$costModel)) {
      charge$ledger <- .tmaSimulationBind(list(charge$ledger,
        .tmaModelCostCharge(bundle$costModel, population, previous,
          terminalStates = configuration$terminalStates, cycleLength = cycleLength)))
    }
    simulation$.state$charged <- charge$charged
    costRows[[length(costRows) + 1L]] <- charge$ledger
    trajectoryRows[[length(trajectoryRows) + 1L]] <- population
    simulation$metadata$steps <- simulation$metadata$steps + 1
    reason <- .tmaSimulationStop(population, simulation$metadata$steps, configuration)
    if (!is.null(reason)) break
  }
  simulation$population <- population
  simulation$trajectories <- .tmaSimulationBind(c(list(simulation$trajectories), trajectoryRows))
  simulation$transitions <- .tmaSimulationBind(c(list(simulation$transitions), transitionRows))
  if (length(effects)) {
    simulation$effects <- .tmaSimulationBind(c(list(simulation$effects), effectRows))
  }
  simulation$costs <- .tmaSimulationBind(c(list(simulation$costs), costRows))
  simulation$totals <- .tmaSimulationTotals(simulation$costs, configuration$costRules, bundle$costModel)
  simulation$metadata$stop_reason <- if (is.null(reason)) "max_steps" else reason
  simulation$metadata$unsupported_current <- sum(is.na(population$stratum_id) &
    !population$state_label %in% configuration$terminalStates)
  simulation$metadata$cycle <- max(population$cycle)
  simulation
}

.tmaSimulationUpdate <- function(population, previous, alive, configuration) {
  current <- population[alive, , drop = FALSE]
  current$stratum_id <- NULL
  patch <- configuration$updateCovariates(current,
    previous[alive, , drop = FALSE])
  patch <- .tmaSimulationData(patch, "updateCovariates result", allowEmpty = TRUE)
  if (!"simulation_id" %in% names(patch) || !is.character(patch$simulation_id) ||
      anyNA(patch$simulation_id) || anyDuplicated(patch$simulation_id) ||
      any(!patch$simulation_id %in% population$simulation_id[alive])) {
    stop("updateCovariates must return unique, known nonterminal simulation_id keys.", call. = FALSE)
  }
  protected <- c("subject_id", "source_subject_id", "state_label", "cycle",
    "elapsed_day", "stratum_id", "index_date", "birth_date",
    names(configuration$bundle$groups))
  if ("birth_date" %in% names(population)) protected <- c(protected, "age")
  if (any(setdiff(names(patch), "simulation_id") %in% protected)) {
    stop("updateCovariates cannot change protected IDs, states, time, automatic age or derived grouping fields.", call. = FALSE)
  }
  rows <- match(patch$simulation_id, population$simulation_id)
  .tmaSimulationMergeRows(population, rows, patch)
}

.tmaSimulationMergeRows <- function(population, rows, patch) {
  for (field in names(patch)) {
    if (!field %in% names(population)) {
      population[[field]] <- patch[[field]][rep(NA_integer_, nrow(population))]
    }
    population[[field]][rows] <- patch[[field]]
  }
  population
}

.tmaSimulationStop <- function(population, step, configuration) {
  if (all(population$state_label %in% configuration$terminalStates)) return("all_terminal")
  if (!is.null(configuration$stopWhen)) {
    stopNow <- configuration$stopWhen(population, step)
    if (!is.logical(stopNow) || length(stopNow) != 1L || is.na(stopNow)) {
      stop("stopWhen must return one nonmissing logical value.", call. = FALSE)
    }
    if (stopNow) return("stop_when")
  }
  NULL
}

.tmaSimulationEmptyTransitions <- function() {
  data.frame(simulation_id = character(), source_cycle = numeric(),
    destination_cycle = numeric(), source_day = numeric(), destination_day = numeric(),
    source_state = character(), destination_state = character(),
    source_stratum = character(), destination_stratum = character(),
    source_period = character(), destination_period = character(),
    transition_probability = numeric(), stringsAsFactors = FALSE)
}

.tmaSimulationEmptyEffects <- function() {
  data.frame(simulation_id = character(), source_cycle = numeric(), source_day = numeric(),
    source_state = character(), source_stratum = character(), source_period = character(),
    effect = character(), to_state = character(), redistribute_to = character(),
    multiplier = numeric(), baseline_probability = numeric(), modified_probability = numeric(),
    baseline_redistributed_probability = numeric(), modified_redistributed_probability = numeric(),
    stringsAsFactors = FALSE)
}

.tmaSimulationBind <- function(frames) {
  fields <- unique(unlist(lapply(frames, names), use.names = FALSE))
  for (field in fields) {
    exemplar <- frames[[which(vapply(frames, function(x) field %in% names(x), logical(1)))[1L]]][[field]]
    for (i in seq_along(frames)) if (!field %in% names(frames[[i]])) {
      frames[[i]][[field]] <- exemplar[rep(NA_integer_, nrow(frames[[i]]))]
    }
  }
  result <- do.call(rbind, lapply(frames, function(x) x[fields]))
  rownames(result) <- NULL
  result
}

.tmaSimulationTotals <- function(ledger, rules, costModel = NULL) {
  components <- names(rules)
  if (is.null(components)) components <- character()
  if (!is.null(costModel)) components <- c("healthcare", components)
  amounts <- vapply(components,
    function(component) sum(ledger$amount[ledger$component == component]), numeric(1))
  if (any(!is.finite(amounts)) || !is.finite(sum(amounts))) {
    stop("Simulated cost totals exceed finite numeric amounts.", call. = FALSE)
  }
  data.frame(component = components, amount = amounts, row.names = NULL,
    stringsAsFactors = FALSE)
}

.tmaSimulationRng <- function(seed = NULL, state = NULL, run) {
  kind <- RNGkind()
  # R keeps Box-Muller's spare normal variate outside .Random.seed. Even calling
  # RNGkind to restore that kind would discard a caller's cached next draw.
  unsupported <- c("Box-Muller", "user-supplied")
  if (any(kind %in% unsupported) ||
      (!is.null(state) && any(state$kind %in% unsupported))) {
    stop("Box-Muller and user-supplied RNG kinds are unsupported: cached or external state cannot be saved and restored. Select supported RNG kinds before simulating.",
      call. = FALSE)
  }
  hadSeed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  oldSeed <- if (hadSeed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    do.call(RNGkind, as.list(kind))
    if (hadSeed) assign(".Random.seed", oldSeed, envir = .GlobalEnv) else if (
      exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  if (!is.null(state)) {
    do.call(RNGkind, as.list(state$kind))
    assign(".Random.seed", state$seed, envir = .GlobalEnv)
  } else if (!is.null(seed)) {
    seed <- .tmaProjectionInteger(seed, "seed", 0)
    if (seed > .Machine$integer.max) stop("seed must be at most .Machine$integer.max.", call. = FALSE)
    set.seed(seed)
  } else {
    set.seed(NULL)
  }
  privateKind <- RNGkind()
  result <- run()
  if (!identical(RNGkind(), privateKind)) {
    stop("Simulation callbacks must not change RNGkind(); the caller RNG has been restored.", call. = FALSE)
  }
  result$.state$rng <- list(kind = privateKind, seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  result
}
