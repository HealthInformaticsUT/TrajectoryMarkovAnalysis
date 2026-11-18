#' Compare available recorded states with a fixed-cohort projection
#'
#' @param trajectories Current Cohort2Trajectory output or prepared trajectories.
#' @param projection A result from projectCohort().
#' @param population "initial" (default) keeps patients with a known clinical
#'   label in cycle zero throughout the comparison. When projection used an
#'   entry model, it instead keeps that model's exact entry-eligible subjects.
#'   "all" uses every supplied patient, including those whose first label is
#'   missing, unresolved or incomplete.
#' @details Known absorbing states are retained after their recorded cycle.
#'   Living states are never carried beyond follow-up. The observed proportion
#'   uses currently known labels plus retained absorbing labels as its
#'   denominator. This denominator can shrink with censoring and gaps.
#'
#'   The projected probabilities describe a fixed cohort. Differences are
#'   descriptive, not a goodness-of-fit test or a censoring-adjusted estimate.
#'   Retained deaths can make the available-state death proportion increase as
#'   living patients leave observation, even without another death. Compare the
#'   proportions alongside coverage, not in isolation.
#'
#'   Cohort sizes are reported separately and never rescaled. Prepared cycle
#'   lengths and absorbing declarations must agree with the projection. Initial
#'   distribution or source-count mismatches are reported, but aggregate counts
#'   cannot establish that the projection came from these same patients.
#'   Selecting a comparison population does not refit the transition model.
#'   Entry-mode default comparison requires the entry-eligible patient identifiers
#'   to be present, with evaluable first cycles. It is a same-patient descriptive
#'   comparison, not external validation. Use population = "all" explicitly to
#'   describe a different supplied population.
#' @return A list with comparison, coverage, initialComparison, diagnostics and
#'   metadata. No plots, files, hypothesis tests or model refitting are produced.
#' @export
compareOccupancy <- function(trajectories, projection, population = "initial") {
  if (!is.character(population) || length(population) != 1L || is.na(population) ||
      !population %in% c("initial", "all")) {
    stop("population must be 'initial' or 'all'.", call. = FALSE)
  }
  projection <- .tmaComparisonProjection(projection)
  model <- projection$metadata
  if (inherits(trajectories, "tma_trajectories")) {
    if (!is.null(trajectories$cycleLength) && trajectories$cycleLength != model$cycleLength) {
      stop("Prepared cycleLength differs from the projection; do not change cycle definitions during comparison.", call. = FALSE)
    }
    if (!setequal(trajectories$absorbingStates, model$absorbingStates)) {
      stop("Prepared absorbingStates differ from the projection.", call. = FALSE)
    }
  }
  prepared <- prepareTrajectories(trajectories, cycleLength = model$cycleLength,
                                   absorbingStates = model$absorbingStates, states = model$states)
  origins <- prepared$patients$index_date[match(prepared$data$subject_id, prepared$patients$subject_id)]
  initialSubjects <- prepared$data$subject_id[
    prepared$data$state_start_date == origins & prepared$data$state_label != "OUT OF COHORT"]
  nInput <- nrow(prepared$patients)
  nInitial <- length(initialSubjects)
  entryMode <- identical(model$initial_source, "entry_model")
  comparisonSubjects <- initialSubjects
  entrySubjectsPresent <- NA
  if (entryMode) {
    entrySubjects <- projection$entry$eligible_subject_ids
    entrySubjectsPresent <- all(entrySubjects %in% prepared$patients$subject_id)
    if (population == "initial") {
      if (!entrySubjectsPresent) {
        stop("Entry-eligible patients are missing from trajectories. Supply the matching histories or explicitly use population = 'all' for a different population.", call. = FALSE)
      }
      evaluable <- prepared$data$state_start_date == origins &
        prepared$data$state_label != "OUT OF COHORT" &
        (prepared$data$observed_days == model$cycleLength |
           prepared$data$state_label %in% model$absorbingStates)
      if (any(!entrySubjects %in% prepared$data$subject_id[evaluable])) {
        stop("The entry-eligible patients no longer have evaluable first cycles in these histories. Recheck the entry model and trajectories.", call. = FALSE)
      }
      comparisonSubjects <- entrySubjects
    }
  }
  if (population == "initial" && !length(comparisonSubjects)) {
    stop("No patients have a known first-cycle state. Use population = 'all' for an available-state summary of all histories.", call. = FALSE)
  }
  if (entryMode) {
    if (population == "initial" && length(comparisonSubjects) < nInput) {
      warning("Excluding ", nInput - length(comparisonSubjects),
              " patients outside the entry model's evaluable first-cycle population from every comparison cycle.", call. = FALSE)
    } else if (population == "all" && !setequal(projection$entry$eligible_subject_ids, prepared$patients$subject_id)) {
      warning("With population = 'all', the comparison population differs from the entry model's eligible subjects.", call. = FALSE)
    }
  } else if (nInitial < nInput) {
    if (population == "initial") {
      warning("Excluding ", nInput - nInitial,
              " patients without a known first-cycle state from every comparison cycle.", call. = FALSE)
    } else {
      warning(nInput - nInitial, " patients lack a known first-cycle state. With population = 'all', they may enter later observed denominators.", call. = FALSE)
    }
  }
  observed <- .tmaSummariseOccupancy(prepared, nCycles = model$nCycles,
                                    subjectIds = if (population == "initial") comparisonSubjects else NULL)
  comparison <- observed$occupancy
  names(comparison)[names(comparison) == "proportion"] <- "observed_proportion"
  names(comparison)[names(comparison) == "denominator"] <- "observed_denominator"
  predicted <- projection$occupancy[c("cycle", "elapsed_day", "state", "probability", "expected_count")]
  names(predicted)[4:5] <- c("projected_probability", "projected_expected_count")
  comparison <- as.data.frame(dplyr::left_join(comparison, predicted,
                                               by = c("cycle", "elapsed_day", "state")))
  comparison$observed_cohort_size <- observed$metadata$n_subjects
  comparison$projected_cohort_size <- model$cohortSize
  comparison$available_fraction <- comparison$observed_denominator / comparison$observed_cohort_size
  comparison <- comparison[order(comparison$cycle, match(comparison$state, model$states)), , drop = FALSE]
  initialComparison <- comparison[comparison$cycle == 0,
                                  c("state", "observed_proportion", "projected_probability"), drop = FALSE]
  initialMatches <- if (all(is.na(initialComparison$observed_proportion))) NA else {
    all(abs(initialComparison$observed_proportion - initialComparison$projected_probability) <= 1e-8)
  }
  if (isFALSE(initialMatches)) {
    warning("The projected initial distribution differs from the observed first-cycle mix. Treat it as a separate starting assumption.", call. = FALSE)
  }
  sourceMatches <- initialCountMatches <- NA
  if (!is.null(model$source_n_subjects)) {
    sourceMatches <- model$source_n_subjects == nInput
    if (!sourceMatches) warning("The projection's source subject count differs from the supplied trajectories.", call. = FALSE)
  }
  if (!is.null(model$initial_denominator)) {
    initialCountMatches <- model$initial_denominator == nInitial
    if (!initialCountMatches) warning("The projection's entry denominator differs from the supplied known first-cycle count.", call. = FALSE)
  }
  entrySourceMatches <- NA
  if (entryMode) {
    entrySourceMatches <- projection$entry$source_subjects == nInput
    if (!entrySourceMatches) warning("The entry model's source subject count differs from the supplied trajectories.", call. = FALSE)
  }
  list(comparison = comparison, coverage = observed$coverage,
       initialComparison = initialComparison,
       diagnostics = list(input_subjects = nInput, known_initial_subjects = nInitial,
                          compared_subjects = observed$metadata$n_subjects,
                          excluded_initial_subjects = if (population == "initial") nInput - length(comparisonSubjects) else 0L,
                          initial_distribution_matches = initialMatches,
                          source_subject_count_matches = sourceMatches,
                          initial_subject_count_matches = initialCountMatches,
                          entry_source_subject_count_matches = entrySourceMatches,
                          entry_eligible_subjects_present = entrySubjectsPresent,
                          count_scales_match = model$cohortSize == observed$metadata$n_subjects),
       metadata = list(population = population,
                       population_rule = if (population == "all") "all supplied subjects" else if (entryMode) "entry model's eligible subjects" else "known first-cycle subjects",
                       observed = observed$metadata, projection = model))
}

.tmaComparisonProjection <- function(projection) {
  if (!is.list(projection) || !is.list(projection$metadata) ||
      !all(c("states", "absorbingStates", "cycleLength", "nCycles", "cohortSize",
             "initial_distribution") %in% names(projection$metadata))) {
    stop("projection must be a result from projectCohort().", call. = FALSE)
  }
  model <- projection$metadata
  .tmaStateDictionary(character(), model$states, model$absorbingStates)
  if (is.null(model$states) || !length(model$states)) stop("Projection states must be explicit.", call. = FALSE)
  nCycles <- .tmaProjectionInteger(model$nCycles, "projection nCycles", 0)
  cycleLength <- .tmaProjectionInteger(model$cycleLength, "projection cycleLength", 1)
  cohortSize <- .tmaProjectionInteger(model$cohortSize, "projection cohortSize", 1)
  for (field in c("source_n_subjects", "initial_denominator")) {
    if (!is.null(model[[field]])) .tmaProjectionInteger(model[[field]], field, 1)
  }
  values <- projection$occupancy
  required <- c("cycle", "elapsed_day", "state", "probability", "expected_count")
  if (!is.data.frame(values) || !all(required %in% names(values)) ||
      nrow(values) != (nCycles + 1) * length(model$states) ||
      !is.character(values$state) || anyNA(values$state) || any(!values$state %in% model$states) ||
      anyDuplicated(values[c("cycle", "state")])) {
    stop("Projection occupancy must contain each cycle/state pair exactly once.", call. = FALSE)
  }
  for (field in setdiff(required, "state")) {
    x <- values[[field]]
    if (!is.numeric(x) || is.complex(x) || inherits(x, "integer64") || !is.null(dim(x)) ||
        anyNA(x) || any(!is.finite(x))) {
      stop("Projection occupancy ", field, " must contain finite numeric values.", call. = FALSE)
    }
  }
  if (any(values$cycle < 0 | values$cycle > nCycles | values$cycle != trunc(values$cycle)) ||
      any(values$elapsed_day != values$cycle * cycleLength)) {
    stop("Projection cycles and elapsed days do not match its metadata.", call. = FALSE)
  }
  if (any(values$probability < 0 | values$probability > 1 + 1e-8) ||
      any(abs(rowsum(values$probability, values$cycle) - 1) > 1e-8)) {
    stop("Projection probabilities must sum to one in every cycle.", call. = FALSE)
  }
  if (any(abs(values$expected_count - values$probability * cohortSize) >
          1e-8 * pmax(1, abs(values$probability * cohortSize)))) {
    stop("Projection expected counts do not match its cohortSize.", call. = FALSE)
  }
  initial <- model$initial_distribution
  if (!is.numeric(initial) || is.complex(initial) || inherits(initial, "integer64") ||
      !is.null(dim(initial)) || is.null(names(initial)) || anyDuplicated(names(initial)) ||
      !setequal(names(initial), model$states) || anyNA(initial) ||
      any(!is.finite(initial) | initial < 0 | initial > 1) || abs(sum(initial) - 1) > 1e-10) {
    stop("Projection initial_distribution is invalid.", call. = FALSE)
  }
  initialRows <- values[values$cycle == 0, , drop = FALSE]
  if (any(abs(initialRows$probability - initial[initialRows$state]) > 1e-8)) {
    stop("Projection cycle-zero probabilities differ from initial_distribution.", call. = FALSE)
  }
  if (identical(model$initial_source, "entry_model")) {
    entry <- projection$entry
    if (!is.list(entry) || !all(c("eligible_subject_ids", "eligible_subjects", "source_subjects") %in% names(entry))) {
      stop("An entry-mode projection must preserve its eligible subjects and source count.", call. = FALSE)
    }
    ids <- .tmaSubjectIds(entry$eligible_subject_ids, "entry eligible_subject_ids")
    nEligible <- .tmaProjectionInteger(entry$eligible_subjects, "entry eligible_subjects", 1)
    nSource <- .tmaProjectionInteger(entry$source_subjects, "entry source_subjects", 1)
    if (anyDuplicated(ids) || length(ids) != nEligible || nEligible > nSource) {
      stop("Entry-mode projection subject counts and identifiers disagree.", call. = FALSE)
    }
    projection$entry$eligible_subject_ids <- ids
  } else if (!is.null(projection$entry)) {
    stop("Projection entry information requires initial_source = 'entry_model'.", call. = FALSE)
  }
  projection
}
