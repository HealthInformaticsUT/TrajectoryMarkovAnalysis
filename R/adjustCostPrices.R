#' Express dated charges in a common price year
#'
#' Adjust individual records before pooling them with estimateStateCosts().
#' The supplied data are not changed. No index is downloaded or chosen for you.
#'
#' @param costData A local data frame with amount, currency_concept_id and the
#'   sourceYear column. It can be the result of extractCosts().
#' @param priceIndex A data frame with year, index and currency_concept_id.
#'   Years must be unique within each currency and indices finite and positive.
#'   Supply one comparable index series per currency, including every source
#'   year and priceYear. Indices need not be based at 100.
#' @param priceYear The common target price year, as a positive integer.
#' @param sourceYear Name of a column in costData containing Date values or
#'   numeric integer years. For example, "event_date" explicitly uses the
#'   original event-start year for the entire charge, even across New Year.
#'   To use a separately known accounting year, supply that year column instead.
#' @param source One nonempty provenance label describing the index's geography,
#'   series, version and any provisional or partial-year coverage. Choosing a
#'   suitable index and its applicability to the costs is the analyst's decision.
#' @return A data frame with adjusted amount and four audit columns:
#'   nominal_amount, source_price_year, price_factor and target_price_year.
#'   Existing extraction attributes and event dates are retained. The
#'   price_adjustment attribute stores the supplied index and its definitions,
#'   without patient records. It is carried through cost estimation and modelling.
#' @details Each amount is multiplied by the target-year index divided by its
#'   source-year index, matched within the recorded currency code. Missing
#'   amounts remain missing and negative credits retain their sign. Unknown
#'   currencies, missing years, duplicate index keys and repeated adjustments
#'   cause errors. Keep the nominal input if you want to try another price basis.
#'
#'   No currency conversion, discounting, calendar-year splitting or duration
#'   allocation occurs here. Duration allocation subsequently divides the full
#'   adjusted amount across the original event duration. In its ledgers,
#'   original_amount is the full adjusted charge and nominal_amount is the full
#'   unadjusted charge. Both repeat on fragments and must not be summed as shares.
#'   Adjusting pooled state means cannot recover these record-specific factors.
#' @seealso estimateStateCosts(), createCostModel()
#' @export
adjustCostPrices <- function(costData, priceIndex, priceYear, sourceYear, source) {
  if (missing(priceIndex) || missing(priceYear) || missing(sourceYear) || missing(source)) {
    stop("Supply priceIndex, priceYear, sourceYear and source explicitly.", call. = FALSE)
  }
  if (!is.data.frame(costData) || anyDuplicated(names(costData)) ||
      !all(c("amount", "currency_concept_id") %in% names(costData))) {
    stop("costData must contain amount and currency_concept_id in uniquely named columns.", call. = FALSE)
  }
  if (!is.null(attr(costData, "price_adjustment", exact = TRUE)) ||
      any(.tmaPriceColumns() %in% names(costData))) {
    stop("These records already contain a price adjustment. Start again from the nominal cost records.", call. = FALSE)
  }
  .tmaPriceText(sourceYear, "sourceYear")
  if (!sourceYear %in% names(costData)) {
    stop("sourceYear column is missing: ", sourceYear, ".", call. = FALSE)
  }
  priceYear <- .tmaProjectionInteger(priceYear, "priceYear", 1)
  .tmaPriceText(source, "source")
  index <- .tmaPriceIndex(priceIndex)
  values <- costData[[sourceYear]]
  if (inherits(values, "Date")) {
    years <- as.integer(format(.tmaDates(values, sourceYear), "%Y"))
    yearType <- "date"
  } else {
    years <- .tmaPriceYears(values, "sourceYear column")
    yearType <- "year"
  }
  currency <- .tmaPriceCurrencies(costData$currency_concept_id)
  factor <- .tmaPriceFactors(index, priceYear, years, currency)
  amount <- costData$amount
  .tmaPriceAmounts(amount)
  adjusted <- amount * factor
  if (any(!is.na(amount) & (!is.finite(adjusted) | (amount != 0 & adjusted == 0)))) {
    stop("Price adjustment produced a nonfinite amount or underflowed to zero.", call. = FALSE)
  }
  out <- as.data.frame(costData)
  for (name in setdiff(names(attributes(costData)), c("names", "class", "row.names"))) {
    attr(out, name) <- attr(costData, name, exact = TRUE)
  }
  out$nominal_amount <- amount
  out$source_price_year <- years
  out$price_factor <- factor
  out$target_price_year <- rep(priceYear, nrow(out))
  out$currency_concept_id <- currency
  out$amount <- adjusted
  attr(out, "price_adjustment") <- list(price_year = priceYear, source = source,
    source_year_column = sourceYear, source_year_type = yearType, index = index,
    method = "amount * target-year index / source-year index within currency",
    currency_conversion = "none")
  out
}

.tmaPriceColumns <- function() {
  c("nominal_amount", "source_price_year", "price_factor", "target_price_year")
}

.tmaPriceText <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || !is.null(dim(value)) ||
      is.na(value) || !nzchar(trimws(value))) {
    stop(name, " must be one nonempty character value.", call. = FALSE)
  }
}

.tmaPriceYears <- function(value, name) {
  if (!is.numeric(value) || is.object(value) || !is.null(dim(value)) ||
      is.complex(value) || anyNA(value) || any(!is.finite(value) |
        value < 1 | value > .Machine$integer.max | value != trunc(value))) {
    stop(name, " must contain nonmissing positive integer years.", call. = FALSE)
  }
  as.integer(value)
}

.tmaPriceCurrencies <- function(value) {
  currency <- .tmaSubjectIds(value, "currency_concept_id")
  if (any(trimws(currency) == "0")) {
    stop("Price adjustment requires known currency_concept_id values, not 0.", call. = FALSE)
  }
  currency
}

.tmaPriceAmounts <- function(value) {
  if (!is.numeric(value) || is.object(value) || !is.null(dim(value)) ||
      is.complex(value) || any(is.infinite(value))) {
    stop("amount must be numeric, with NA for missing amounts.", call. = FALSE)
  }
}

.tmaPriceIndex <- function(index) {
  if (!is.data.frame(index) || !nrow(index) || anyDuplicated(names(index)) ||
      !all(c("year", "index", "currency_concept_id") %in% names(index))) {
    stop("priceIndex must contain year, index and currency_concept_id.", call. = FALSE)
  }
  years <- .tmaPriceYears(index$year, "priceIndex year")
  values <- index$index
  if (!is.numeric(values) || is.object(values) || !is.null(dim(values)) ||
      is.complex(values) || anyNA(values) || any(!is.finite(values) | values <= 0)) {
    stop("priceIndex index values must be finite and positive.", call. = FALSE)
  }
  # Retain only aggregate index values, never caller-supplied extra fields.
  out <- data.frame(year = years, index = as.numeric(values),
                    currency_concept_id = .tmaPriceCurrencies(index$currency_concept_id))
  if (anyDuplicated(out[c("currency_concept_id", "year")])) {
    stop("priceIndex contains duplicate currency/year keys.", call. = FALSE)
  }
  out
}

.tmaPriceFactors <- function(index, priceYear, years, currency) {
  if (!length(currency)) return(numeric())
  keys <- paste(index$currency_concept_id, index$year, sep = "\r")
  sourceRows <- match(paste(currency, years, sep = "\r"), keys)
  targetRows <- match(paste(currency, priceYear, sep = "\r"), keys)
  missing <- character()
  if (anyNA(sourceRows)) {
    missing <- paste(currency[is.na(sourceRows)], years[is.na(sourceRows)], sep = "/")
  }
  if (anyNA(targetRows)) {
    missing <- c(missing, paste(currency[is.na(targetRows)], priceYear, sep = "/"))
  }
  if (length(missing)) {
    stop("priceIndex is missing required currency/year values: ",
         paste(head(unique(missing), 20L), collapse = ", "), ".", call. = FALSE)
  }
  factors <- index$index[targetRows] / index$index[sourceRows]
  if (any(!is.finite(factors) | factors <= 0)) {
    stop("Price adjustment factors must be finite and positive.", call. = FALSE)
  }
  factors
}

# Validate the aggregate price basis on construction and when loading a bundle.
.tmaValidatePriceBasis <- function(meta, priceYear = NULL, currencies = NULL) {
  if (is.null(meta)) return(invisible(NULL))
  if (!is.list(meta)) stop("Invalid price_adjustment metadata.", call. = FALSE)
  year <- .tmaProjectionInteger(meta$price_year, "Adjusted price year", 1)
  .tmaPriceText(meta$source, "Price index source")
  .tmaPriceText(meta$source_year_column, "Source-year column")
  if (!identical(meta$source_year_type, "date") && !identical(meta$source_year_type, "year")) {
    stop("Invalid source-year convention in price_adjustment metadata.", call. = FALSE)
  }
  index <- .tmaPriceIndex(meta$index)
  if (!is.null(priceYear) && priceYear != year) {
    stop("priceYear must match the adjusted price year: ", year, ".", call. = FALSE)
  }
  if (!is.null(currencies)) {
    currency <- .tmaPriceCurrencies(currencies)
    .tmaPriceFactors(index, year, rep(year, length(currency)), currency)
  }
  invisible(NULL)
}

# Binding or editing rows must not silently mix price bases before estimation.
.tmaCostPriceAdjustment <- function(costData) {
  meta <- attr(costData, "price_adjustment", exact = TRUE)
  columns <- .tmaPriceColumns()
  if (is.null(meta) && !any(columns %in% names(costData))) return(NULL)
  if (is.null(meta) || !all(c(columns, "currency_concept_id") %in% names(costData))) {
    stop("Incomplete price adjustment. Keep its metadata and audit columns with the cost records.", call. = FALSE)
  }
  .tmaValidatePriceBasis(meta)
  years <- .tmaPriceYears(costData$source_price_year, "source_price_year")
  if (!meta$source_year_column %in% names(costData)) {
    stop("The original sourceYear column is missing from the adjusted records.", call. = FALSE)
  }
  sourceValues <- costData[[meta$source_year_column]]
  if (meta$source_year_type == "date") {
    if (!inherits(sourceValues, "Date")) {
      stop("The adjusted sourceYear column must still contain Date values.", call. = FALSE)
    }
    sourceYears <- as.integer(format(.tmaDates(sourceValues, meta$source_year_column), "%Y"))
  } else {
    sourceYears <- .tmaPriceYears(sourceValues, "sourceYear column")
  }
  if (any(years != sourceYears)) {
    stop("Cost rows do not match their sourceYear convention. Recreate them from the nominal records.", call. = FALSE)
  }
  target <- .tmaPriceYears(costData$target_price_year, "target_price_year")
  currency <- .tmaPriceCurrencies(costData$currency_concept_id)
  factors <- .tmaPriceFactors(meta$index, meta$price_year, years, currency)
  .tmaPriceAmounts(costData$nominal_amount)
  .tmaPriceAmounts(costData$amount)
  recorded <- costData$price_factor
  if (!is.numeric(recorded) || is.object(recorded) || !is.null(dim(recorded)) ||
      is.complex(recorded) || anyNA(recorded) || any(!is.finite(recorded) | recorded <= 0)) {
    stop("price_factor must contain finite positive adjustment factors.", call. = FALSE)
  }
  expected <- costData$nominal_amount * factors
  known <- !is.na(expected)
  if (any(target != meta$price_year) ||
      any(abs(recorded - factors) > 1e-12 * abs(factors)) ||
      any(is.na(costData$amount) != is.na(expected)) ||
      any(!is.finite(expected[known])) ||
      any(abs(costData$amount[known] - expected[known]) > 1e-12 * pmax(1, abs(expected[known])))) {
    stop("Cost rows do not match their price adjustment. Recreate them from the nominal records.", call. = FALSE)
  }
  meta
}
