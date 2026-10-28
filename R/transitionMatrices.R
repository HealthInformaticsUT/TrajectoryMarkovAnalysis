# Shared validation for supplied transition probabilities and flow ledgers.
.tmaTransitionMatrix <- function(x, states, absorbingStates, name) {
  if (!is.matrix(x) || !is.numeric(x) || is.complex(x) || inherits(x, "integer64") ||
      !identical(dim(x), rep(length(states), 2L)) ||
      is.null(rownames(x)) || is.null(colnames(x)) ||
      anyDuplicated(rownames(x)) || anyDuplicated(colnames(x)) ||
      !setequal(rownames(x), states) || !setequal(colnames(x), states)) {
    stop(name, " must have the declared states on both axes.", call. = FALSE)
  }
  x <- x[states, states, drop = FALSE]
  missing <- rowSums(is.na(x))
  complete <- missing == 0L
  values <- x[!is.na(x)]
  if (any(is.nan(x)) || any(missing > 0L & missing < length(states)) ||
      any(!is.finite(values) | values < 0 | values > 1) ||
      any(abs(rowSums(x[complete, , drop = FALSE]) - 1) > 1e-10)) {
    stop(name, " must contain stochastic probability rows or entire NA rows.", call. = FALSE)
  }
  for (state in absorbingStates) {
    if (anyNA(x[state, ]) || any(x[state, ] != as.numeric(states == state))) {
      stop(name, " must preserve the exact identity row for absorbing state '", state, "'.", call. = FALSE)
    }
  }
  x
}
