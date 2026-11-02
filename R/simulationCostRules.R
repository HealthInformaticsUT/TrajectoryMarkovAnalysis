#' Cost rules for sampled patient histories
#'
#' @name simulation-cost-rules
#' @description Supply costRules to simulateTrajectories() as a named list.
#'   Each name identifies a separate cost component, such as initiation,
#'   maintenance or ordinary care. All amounts must use the supplied currency
#'   and priceYear. No conversion, inflation adjustment or discounting is done.
#' @details Each rule contains type and amount. type is one of:
#' \itemize{
#'   \item once: charge at the first eligible labelled cycle for each simulated
#'     patient. The simulator remembers this across later cycles and resumed runs.
#'   \item recurring: charge at every eligible labelled cycle.
#'   \item transition: charge the specified from-to state change. from and to
#'     must be different known state labels. Remaining in a state is not a new
#'     event. Initial snapshots do not imply a preceding transition.
#' }
#' amount is a finite cost per matching patient. Negative amounts represent
#' credits. Optional states restrict the destination labels. startDay (default
#' zero, inclusive) and endDay (default infinity, exclusive) restrict elapsed
#' follow-up days. Windows follow the label's day, not calendar year or a newly
#' reset time since treatment. For a duration measured from treatment initiation,
#' retain that initiation time in patient covariates and use eligible.
#'
#' An optional eligible function receives candidate patient rows and returns one
#' nonmissing logical value per supplied row. It can inspect covariates such as
#' treatment_active or treatment_start_day. It does not change the patient data.
#' Clinical state, time-window and prior-payment restrictions are checked before
#' this callback. Missing treatment information is not inferred.
#'
#' Initial and subsequent labelled snapshots can incur charges. nSteps = k
#' therefore describes up to k + 1 charged labels, consistent with projectCosts().
#' Amounts are per complete label, not prorated daily rates. A simulation resumed
#' with advanceSimulation() does not charge its saved starting label again.
#' Once and recurring rules never charge explicitly terminal patients. A
#' transition rule can charge entry into a terminal state once. Absorbing states
#' not declared terminal can continue to incur costs.
#'
#' All components add together. Keep overlapping amounts out of different rules.
#' These rules can represent intervention or ordinary healthcare expenditure,
#' but do not change transition probabilities or establish intervention effects.
#' Raw daily estimates retained in a model bundle are not automatically charged.
#' Use createCostModel() to choose their units and terminal policy explicitly,
#' then attach that model with createModelBundle(costs = ...). Its healthcare
#' component is additional to these rules. That component name is reserved, and
#' all rules must use the cost model's currency and price year. Avoid adding
#' manual care charges that are already included in the fitted healthcare rate.
#'
#' For example, a setup fee and a four-week recurring cost can be supplied as
#' list(setup = list(type = "once", amount = 1500),
#' maintenance = list(type = "recurring", amount = 20, endDay = 28)).
#' With weekly labels starting at day zero, maintenance is charged at days
#' 0, 7, 14 and 21, provided the patient has not entered a terminal state.
NULL
