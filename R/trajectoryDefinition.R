# Read only the compact trajectory contract; never retain the source object.
.tmaTrajectoryDefinition <- function(source, preparedFields = FALSE) {
  prepared <- inherits(source, "tma_trajectories")
  if (!prepared && !is.data.frame(source)) {
    stop("definition must be prepared trajectories or a data frame carrying trajectory_definition.",
         call. = FALSE)
  }
  definition <- attr(source, "trajectory_definition", exact = TRUE)
  if (is.null(definition) && prepared && preparedFields) {
    definition <- list(version = 1L,
      trajectoryType = if (is.null(source$cycleLength)) "Continuous" else "Discrete",
      cycleLength = unname(source$cycleLength), states = unname(source$states),
      absorbingStates = unname(source$absorbingStates))
  }
  if (is.null(definition)) return(NULL)
  .tmaValidateTrajectoryDefinition(definition)
  if (prepared) {
    .tmaResolveTrajectoryDefinition(definition, source$cycleLength, source$states,
      source$absorbingStates, supplied = c(cycleLength = TRUE, states = TRUE,
                                         absorbingStates = TRUE))
  }
  definition
}

.tmaValidateTrajectoryDefinition <- function(definition) {
  fields <- c("version", "trajectoryType", "cycleLength", "states", "absorbingStates")
  if (!is.list(definition) || is.object(definition) ||
      !identical(names(attributes(definition)), "names") ||
      is.null(names(definition)) || anyDuplicated(names(definition)) ||
      !setequal(names(definition), fields)) {
    stop("trajectory_definition must be a versioned list containing only version, trajectoryType, cycleLength, states and absorbingStates.",
         call. = FALSE)
  }
  version <- definition$version
  if (!is.integer(version) || length(version) != 1L || is.na(version) ||
      !is.null(attributes(version))) {
    stop("trajectory_definition version must be a single integer.", call. = FALSE)
  }
  if (version != 1L) {
    stop("Unsupported trajectory_definition version: ", version, ".", call. = FALSE)
  }
  type <- definition$trajectoryType
  if (!is.character(type) || length(type) != 1L || is.na(type) ||
      !is.null(attributes(type)) || !type %in% c("Discrete", "Continuous")) {
    stop("trajectory_definition trajectoryType must be Discrete or Continuous.", call. = FALSE)
  }
  cycle <- definition$cycleLength
  if (type == "Discrete" &&
      (is.null(cycle) || !is.numeric(cycle) || is.complex(cycle) ||
       length(cycle) != 1L || is.na(cycle) || !is.finite(cycle) ||
       cycle <= 0 || cycle != trunc(cycle) || !is.null(attributes(cycle)))) {
    stop("Discrete trajectory_definition cycleLength must be a positive integer number of days.",
         call. = FALSE)
  }
  if (type == "Continuous" && !is.null(cycle)) {
    stop("Continuous trajectory_definition cycleLength must be NULL.", call. = FALSE)
  }
  for (field in c("states", "absorbingStates")) {
    value <- definition[[field]]
    if (!is.character(value) || !is.null(attributes(value)) || anyNA(value) ||
        anyDuplicated(value) || any(!nzchar(trimws(value))) ||
        any(value %in% c("0", "START", "EXIT", "OUT OF COHORT"))) {
      stop("trajectory_definition ", field,
           " must contain unique, nonempty clinical state labels without attributes or boundary markers.",
           call. = FALSE)
    }
  }
  if (length(setdiff(definition$absorbingStates, definition$states))) {
    stop("trajectory_definition absorbingStates must belong to states.", call. = FALSE)
  }
  invisible(NULL)
}

.tmaResolveTrajectoryDefinition <- function(definition, cycleLength, states,
                                            absorbingStates, supplied,
                                            allowUndeclaredCycle = FALSE) {
  values <- list(cycleLength = cycleLength, states = states, absorbingStates = absorbingStates)
  if (is.null(definition)) return(values)
  for (field in names(values)) {
    declared <- definition[[field]]
    if (!supplied[[field]]) {
      values[field] <- list(declared)
      next
    }
    value <- values[[field]]
    same <- if (field == "cycleLength") {
      if (allowUndeclaredCycle && is.null(declared)) {
        TRUE
      } else if (is.null(value) || is.null(declared)) {
        is.null(value) && is.null(declared)
      } else {
        is.numeric(value) && !is.complex(value) && length(value) == 1L &&
          is.null(dim(value)) && !is.na(value) && isTRUE(value == declared)
      }
    } else {
      is.character(value) && is.null(dim(value)) && !anyNA(value) &&
        !anyDuplicated(value) && if (field == "states") {
          identical(unname(value), declared)
        } else {
          setequal(value, declared)
        }
    }
    if (!same) {
      stop(field, " conflicts with trajectory_definition",
           if (field == "states") "; states must retain the declared order." else ".",
           call. = FALSE)
    }
  }
  values
}
