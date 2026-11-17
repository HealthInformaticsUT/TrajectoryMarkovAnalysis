# Calibration needs a background-only model, not a fitted all-cause transition
# matrix. Reconstruct its probabilities from the retained inputs so an edited
# matrix or stale provenance cannot silently change the reference calculation.
.tmaBackgroundMortalityBundle <- function(background, cycleLength) {
  if (!inherits(background, "tma_model_bundle")) {
    stop("background must be a bundle from createModelBundle(createMortalityTransitions(...)).",
         call. = FALSE)
  }
  if (!is.null(background$extrapolation) || !is.null(background$costModel) ||
      !is.null(background$costEstimates)) {
    stop("background must contain mortality-only models without costs or an extrapolation.",
         call. = FALSE)
  }
  background <- .tmaValidateModelBundle(background)
  if (background$metadata$cycleLength != cycleLength) {
    stop("background and trajectories must use the same cycleLength.", call. = FALSE)
  }
  for (id in names(background$models)) {
    model <- background$models[[id]]
    evidence <- model$metadata$mortality
    if (!identical(model$metadata$estimator, "specified_mortality") ||
        !is.list(evidence) || anyDuplicated(names(evidence)) ||
        !all(c("source", "probabilityDays", "death_probability", "cycle_probability") %in%
             names(evidence)) || nrow(model$periods) != 1L ||
        length(model$metadata$absorbingStates) != 1L) {
      stop("Each background model must come from createMortalityTransitions() with its mortality inputs retained.",
           call. = FALSE)
    }
    supplied <- evidence$death_probability
    if (!is.numeric(supplied) || length(supplied) != 1L || !is.null(dim(supplied))) {
      stop("Each background model must retain one death_probability.", call. = FALSE)
    }
    expected <- createMortalityTransitions(data.frame(death_probability = supplied),
      states = model$metadata$states, deathState = model$metadata$absorbingStates,
      cycleLength = cycleLength, probabilityDays = evidence$probabilityDays,
      source = evidence$source)
    if (!isTRUE(all.equal(model$matrices[[1L]], expected$matrix, tolerance = 0)) ||
        !isTRUE(all.equal(evidence$cycle_probability,
                         expected$metadata$mortality$cycle_probability, tolerance = 0))) {
      stop("Background probabilities do not match their retained mortality inputs. Recreate the model with createMortalityTransitions().",
           call. = FALSE)
    }
  }
  background
}
