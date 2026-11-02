#' Collect transition models and their covariate definitions
#'
#' @param transitions A result from estimateTransitions(), with or without
#'   strata, or an ordinary model from composeTransitionStages().
#' @param costs Optional result from estimateStateCosts(). Only its estimates,
#'   periods and metadata are retained. These descriptive daily estimates are
#'   not automatically converted into simulation charges.
#' @param groups Optional named list of numeric grouping rules. Each name is an
#'   output column and each rule contains column, breaks and optionally labels.
#'   For example, list(age_group = list(column = "age",
#'   breaks = c(0, 65, 75, Inf))). Use addCovariateGroups() with the same rules
#'   before fitting. Without rules, supply the strata columns directly.
#' @details Models share one clinical state dictionary, cycle length and set of
#'   follow-up periods. Covariate groups are separate from clinical states.
#'   Selection uses the complete combination of source-row covariates. Missing
#'   groups and unavailable probability rows are not replaced by another model.
#'
#'   The bundle keeps estimated counts and model definitions, not training
#'   patient histories or a starting population. Supply a population or a joint
#'   starting distribution separately when simulating. Composed models retain
#'   their stage definitions, but patient simulation samples their combined
#'   cycle outcomes, not unobserved intermediate patient events.
#' @return A tma_model_bundle with models, strata, groups, optional costEstimates
#'   and metadata. No database connection or patient-level cost records are kept.
#' @export
createModelBundle <- function(transitions, costs = NULL, groups = NULL) {
  if (inherits(transitions, "tma_stratified_transitions")) {
    models <- transitions$models
    registry <- transitions$strata
    columns <- transitions$metadata$strata
  } else {
    models <- list(s1 = transitions)
    registry <- data.frame(stratum_id = "s1", stringsAsFactors = FALSE)
    columns <- character()
  }
  costEstimates <- NULL
  if (!is.null(costs)) {
    if (!inherits(costs, "tma_state_costs") || !is.data.frame(costs$estimates)) {
      stop("costs must be a result from estimateStateCosts().", call. = FALSE)
    }
    costEstimates <- costs[c("estimates", "periods", "metadata")]
  }
  bundle <- structure(list(models = models, strata = registry, groups = groups,
                           costEstimates = costEstimates,
                           metadata = list(strata = columns)),
                      class = "tma_model_bundle")
  .tmaValidateModelBundle(bundle)
}

.tmaValidateModelBundle <- function(bundle) {
  if (!inherits(bundle, "tma_model_bundle") || !is.list(bundle$models) ||
      !length(bundle$models) || is.null(names(bundle$models)) ||
      anyNA(names(bundle$models)) || anyDuplicated(names(bundle$models)) ||
      any(!nzchar(names(bundle$models))) || !is.list(bundle$metadata)) {
    stop("Supply a model bundle created by createModelBundle().", call. = FALSE)
  }
  columns <- bundle$metadata$strata
  if (is.null(columns)) columns <- character()
  if (!is.character(columns) || anyNA(columns) || anyDuplicated(columns)) {
    stop("Bundle strata must be unique column names.", call. = FALSE)
  }
  registry <- bundle$strata
  if (!is.data.frame(registry) || anyDuplicated(names(registry)) ||
      !setequal(names(registry), c("stratum_id", columns)) ||
      !nrow(registry) || !is.character(registry$stratum_id) ||
      anyNA(registry$stratum_id) || anyDuplicated(registry$stratum_id) ||
      !setequal(registry$stratum_id, names(bundle$models))) {
    stop("Bundle strata must identify every model exactly once.", call. = FALSE)
  }
  if (!length(columns) && nrow(registry) != 1L) {
    stop("An unstratified bundle must contain exactly one model.", call. = FALSE)
  }
  if (length(columns)) {
    .tmaStrata(registry, columns)
    if (anyDuplicated(registry[columns])) {
      stop("Bundle strata contain duplicate covariate combinations.", call. = FALSE)
    }
  }
  first <- bundle$models[[1L]]
  if (!is.list(first$metadata) || !length(first$metadata$states)) {
    stop("Bundle models must declare their states and cycle length.", call. = FALSE)
  }
  states <- first$metadata$states
  absorbing <- first$metadata$absorbingStates
  cycleLength <- first$metadata$cycleLength
  commonPeriods <- NULL
  for (id in names(bundle$models)) {
    model <- bundle$models[[id]]
    if (!is.list(model$metadata) ||
        !identical(model$metadata$states, states) ||
        !setequal(model$metadata$absorbingStates, absorbing) ||
        !identical(model$metadata$cycleLength, cycleLength)) {
      stop("Bundle models must share states, absorbing states and cycleLength.", call. = FALSE)
    }
    # Reuse the existing checks for periods, matrices and optional stage products.
    checked <- projectCohort(model, nCycles = 0,
                             initial = stats::setNames(1, states[1L]))
    if (is.null(commonPeriods)) commonPeriods <- checked$metadata$periods
    if (!identical(checked$metadata$periods, commonPeriods)) {
      stop("Bundle models must use the same follow-up periods.", call. = FALSE)
    }
    model$matrices <- lapply(model$matrices[commonPeriods$period], function(x) x[states, states, drop = FALSE])
    model$periods <- commonPeriods
    model$matrix <- if (nrow(commonPeriods) == 1L) model$matrices[[1L]] else NULL
    # Preparation diagnostics can contain original patient IDs. Keep aggregates
    # and model inputs only, not the training cohort's patient-level diagnostics.
    keep <- c("matrices", "matrix", "periods", "entryCounts", "estimates", "metadata", "stageMatrices")
    bundle$models[[id]] <- model[intersect(keep, names(model))]
  }
  groups <- .tmaCovariateGroups(bundle$groups)
  if (length(groups) && any(!names(groups) %in% columns)) {
    stop("Each grouping rule must create a declared transition stratum column.", call. = FALSE)
  }
  for (name in names(groups)) {
    if (!(is.character(registry[[name]]) || is.factor(registry[[name]])) ||
        any(!as.character(registry[[name]]) %in% groups[[name]]$labels)) {
      stop("Grouping labels for '", name,
           "' must match the fitted stratum values. Apply the same grouping rules before fitting.",
           call. = FALSE)
    }
  }
  bundle$strata <- registry[c("stratum_id", columns)]
  bundle$groups <- groups
  bundle$metadata <- list(states = states, absorbingStates = absorbing,
                          cycleLength = cycleLength, strata = columns,
                          periods = commonPeriods,
                          selection = "source covariates and source elapsed day",
                          costs = "descriptive estimates retained; explicit simulation costRules required",
                          age = "completed years from birth_date and index_date plus elapsed_day when supplied")
  bundle
}

.tmaBundleModel <- function(bundle, population, allowUnmatched = FALSE) {
  columns <- bundle$metadata$strata
  if (!length(columns)) return(rep(bundle$strata$stratum_id[1L], nrow(population)))
  if (!all(columns %in% names(population))) {
    stop("Population is missing model covariates: ",
         paste(setdiff(columns, names(population)), collapse = ", "), ".", call. = FALSE)
  }
  .tmaStrata(population, columns)
  values <- as.data.frame(population[columns])
  registry <- bundle$strata
  for (column in columns) {
    if (is.factor(values[[column]])) values[[column]] <- as.character(values[[column]])
    if (is.factor(registry[[column]])) registry[[column]] <- as.character(registry[[column]])
  }
  matched <- dplyr::left_join(values, registry, by = columns)
  if (nrow(matched) != nrow(population) || (!allowUnmatched && anyNA(matched$stratum_id))) {
    stop("No transition model supports one or more current covariate combinations. Supply supported profiles or an explicit model; no fallback is applied.",
         call. = FALSE)
  }
  matched$stratum_id
}

#' Apply explicit numeric covariate groups to trajectory rows or patient profiles
#'
#' @param data A data frame. Original columns are retained.
#' @param groups Named list of grouping rules, as in createModelBundle(). Each
#'   rule names a numeric source column, increasing breaks and optional labels.
#' @param dateColumn Optional column containing the date at which age should be
#'   calculated. Requires birth_date and creates or replaces age with completed
#'   years. Leave NULL when supplying age or other covariates yourself.
#' @details Groups include their lower boundary and exclude their upper boundary.
#'   Values outside the supplied breaks or missing values are errors. No score,
#'   diagnosis or future measurement is inferred. Each trajectory row's values
#'   apply to that complete row. The user is responsible for their availability
#'   at the start of that interval.
#'
#'   Age uses calendar birthdays. A February 29 birthday is reached on March 1
#'   in a non-leap year. No birthday is invented from a birth year alone. The
#'   simulator uses the same calculation when profiles contain birth_date and
#'   index_date. Otherwise age changes only through the user's update callback.
#' @return A data frame retaining the input rows and raw covariates, with one
#'   character grouping column per rule. This function does not split intervals.
#' @export
addCovariateGroups <- function(data, groups, dateColumn = NULL) {
  if (!is.data.frame(data) || anyDuplicated(names(data))) {
    stop("data must be a data frame with unique column names.", call. = FALSE)
  }
  data <- as.data.frame(data)
  if (!is.null(dateColumn)) {
    if (!is.character(dateColumn) || length(dateColumn) != 1L || is.na(dateColumn) ||
        !all(c("birth_date", dateColumn) %in% names(data))) {
      stop("dateColumn must name a date column and data must include birth_date.", call. = FALSE)
    }
    data$age <- .tmaAgeAtDate(data$birth_date, data[[dateColumn]])
  }
  groups <- .tmaCovariateGroups(groups)
  for (name in names(groups)) {
    rule <- groups[[name]]
    if (!rule$column %in% names(data)) {
      stop("Grouping rule '", name, "' needs column '", rule$column, "'.", call. = FALSE)
    }
    x <- data[[rule$column]]
    if (!is.numeric(x) || is.complex(x) || inherits(x, "integer64") ||
        !is.null(dim(x)) || anyNA(x) || any(!is.finite(x))) {
      stop("Grouping column '", rule$column, "' must contain finite numeric values.", call. = FALSE)
    }
    grouped <- as.character(cut(x, breaks = rule$breaks, labels = rule$labels, right = FALSE))
    if (anyNA(grouped)) {
      stop("Grouping breaks for '", name, "' do not cover all values.", call. = FALSE)
    }
    data[[name]] <- grouped
  }
  data
}

.tmaCovariateGroups <- function(groups) {
  if (is.null(groups) || identical(groups, list())) return(list())
  if (!is.list(groups) || is.data.frame(groups) || is.null(names(groups)) ||
      anyNA(names(groups)) || anyDuplicated(names(groups)) || any(!nzchar(names(groups)))) {
    stop("groups must be a named list of covariate grouping rules.", call. = FALSE)
  }
  reserved <- c("subject_id", "simulation_id", "source_subject_id", "state_label",
                 "state_start_date", "state_end_date", "index_date", "birth_date",
                 "cycle", "elapsed_day", "stratum_id", "period", "age")
  if (any(names(groups) %in% reserved)) {
    stop("Grouping outputs must not replace identifiers, states, dates, age or simulation bookkeeping.", call. = FALSE)
  }
  for (name in names(groups)) {
    rule <- groups[[name]]
    if (!is.list(rule) || is.data.frame(rule) || is.null(names(rule)) ||
        anyDuplicated(names(rule)) || !all(c("column", "breaks") %in% names(rule)) ||
        any(!names(rule) %in% c("column", "breaks", "labels")) ||
        !is.character(rule$column) || length(rule$column) != 1L ||
        is.na(rule$column) || !nzchar(rule$column) || rule$column %in% names(groups)) {
      stop("Each group needs column and breaks, with optional labels. Source columns must remain separate from group outputs.", call. = FALSE)
    }
    breaks <- rule$breaks
    if (!is.numeric(breaks) || is.complex(breaks) || inherits(breaks, "integer64") ||
        !is.null(dim(breaks)) || length(breaks) < 2L || anyNA(breaks) ||
        anyNA(diff(breaks)) || any(diff(breaks) <= 0) ||
        any(!is.finite(breaks[-c(1L, length(breaks))]))) {
      stop("Group breaks must be increasing numeric boundaries, with infinity only at the ends.", call. = FALSE)
    }
    if (is.null(rule$labels)) {
      rule$labels <- paste0("[", head(breaks, -1L), ",", tail(breaks, -1L), ")")
    }
    if (!is.character(rule$labels) || length(rule$labels) != length(breaks) - 1L ||
        anyNA(rule$labels) || anyDuplicated(rule$labels) || any(!nzchar(rule$labels))) {
      stop("Group labels must uniquely name every interval.", call. = FALSE)
    }
    groups[[name]] <- rule
  }
  groups
}

.tmaAgeAtDate <- function(birth, date) {
  birth <- .tmaDates(birth, "birth_date")
  date <- .tmaDates(date, "age reference date")
  if (length(birth) != length(date) || any(birth > date)) {
    stop("birth_date must cover every row and cannot follow its reference date.", call. = FALSE)
  }
  as.integer(format(date, "%Y")) - as.integer(format(birth, "%Y")) -
    as.integer(format(date, "%m%d") < format(birth, "%m%d"))
}

.tmaSimulationCovariates <- function(bundle, population) {
  if ("birth_date" %in% names(population)) {
    if (!"index_date" %in% names(population)) {
      stop("Profiles with birth_date need index_date to advance calendar age.", call. = FALSE)
    }
    population$index_date <- .tmaDates(population$index_date, "index_date")
    population$birth_date <- .tmaDates(population$birth_date, "birth_date")
    population$age <- .tmaAgeAtDate(population$birth_date,
                                    population$index_date + population$elapsed_day)
  }
  addCovariateGroups(population, bundle$groups)
}
