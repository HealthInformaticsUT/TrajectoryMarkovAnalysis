# TrajectoryMarkovAnalysis

TrajectoryMarkovAnalysis estimates transitions and recorded costs from patient
histories. The two workflows are independent. Transition estimation needs only
trajectories. Cost estimation can use a local expenditure table or records read
from a database through CDMConnector.

## Refactor status

The new interface accepts the current lowercase Cohort2Trajectory output. It
supports adjacent-cycle transition probabilities and descriptive costs
by state, either pooled or separated into follow-up periods. It returns R objects
without opening plots, writing files, dropping
database tables or closing the caller's connection.

This is not yet a release. Terminal-cycle cost allocation and within-cycle
models remain to be implemented. The older GUI,
continuous-time model, survival functions and simulation code have not been
migrated or validated against the new interface. Do not use those functions to
validate the new estimates.

## Prepare the histories once

Use the data frame returned by `Cohort2Trajectory::createTrajectories()`.
Only four columns are required: `subject_id`, `state_label`, `state_start_date`
and `state_end_date`. Other columns are preserved. Patient identifiers retain
their values, including leading zeros and large identifiers supplied as text.

```r
library(TrajectoryMarkovAnalysis)

histories <- prepareTrajectories(
  trajectories,
  cycleLength = 7,
  absorbingStates = c(
    "Death, no pneumonia history",
    "Death, pneumonia history"
  )
)
```

State labels must describe the analyst's definitions. The package does not
infer that a label means death or that pneumonia history persists. Specify
absorbing states explicitly. You can supply `states` to include states that
are defined but absent from the current data.

`START` and `EXIT` mark observation boundaries. They are not clinical states.
`OUT OF COHORT` is an unresolved observed interval, not death or loss of
observation. Dates include both endpoints. Only the final weekly cycle may
be shorter than the declared cycle length. Overlapping intervals and duplicate
rows must be resolved before estimation.

Each patient must have at least one observed interval. Boundary-only continuous
histories are not supported by this first version of the preparation step.

For interval-based costing without a discrete model, leave `cycleLength` unset.
If boundary markers are absent, the package uses the observed interval bounds
and reports that the original observation boundaries could not be checked.

## Estimate transitions without a database

```r
transitions <- estimateTransitions(histories)

transitions$estimates
transitions$matrix
transitions$entryCounts
transitions$diagnostics
```

The estimator counts transitions from a full cycle to the immediately following
observed cycle. It never connects states across a missing or unresolved cycle.
A final living cycle without an observed next state does not imply another
living cycle or a death. `seq_ordinal` is not used as a cycle number.

Each estimate includes the transition count, source denominator and probability.
Unobserved nonabsorbing rows have `NA` probabilities. Explicit absorbing states
have structural identity rows, labelled as structural rather than observed
transitions. Calculations retain their precision until you format the results.

`entryCounts` describes known labels in the first cycle, not a risk estimated
from an assumed pre-entry state. A death in the first cycle stays in these
counts. The estimator does not invent a preceding alive-to-death transition.

These are transitions between cycle labels. They are not yet the
two-step pneumonia-onset and mortality probabilities used in the thesis.
Weekly labels cannot recover the exact order or dates of events within a week.

## Separate early and later follow-up

Use the same period boundaries for transition and cost analyses. Boundaries are
days since each patient's index date, not calendar dates or row numbers.

```r
period_breaks <- 7 * c(0, 1, 5, 13, Inf)
period_labels <- c("Week 0", "Weeks 1-4", "Weeks 5-12", "Week 13 onward")

transitions <- estimateTransitions(
  histories,
  periodBreaks = period_breaks,
  periodLabels = period_labels
)

transitions$estimates
transitions$matrices[["Weeks 1-4"]]
transitions$periods
```

Each period includes its first day and stops before the next boundary. In this
example, Week 0 covers days 0 through 6. All follow-up must be covered, so breaks
start at 0 and end at `Inf`. The last period includes any remaining observed
follow-up. It does not extrapolate beyond the data. Omit both arguments for one
pooled estimate.

A transition belongs to the period in which its source cycle starts. For example,
the pair from days 0-6 to days 7-13 belongs to Week 0. Crossing a period boundary
does not discard a pair. Transition boundaries must align with the cycle length.
The denominator counts observed outgoing pairs in that period, not everyone
alive at its start. This does not correct for informative censoring.

Every requested period has a matrix with the same state order. Without observed
transitions, a nonabsorbing row stays `NA`. Absorbing rows remain explicitly
structural. `matrices` is always a named list. `matrix` is available only for a
single period and is `NULL` when several periods are requested. Baseline
`entryCounts` are reported once, not repeated for each period.

## Extract costs through CDMConnector

Pass an existing database-backed CDMConnector reference:

```r
cost_events <- extractCosts(
  cdm,
  trajectories = histories,
  measure = "total_charge",
  domains = c("Visit", "Drug", "Procedure")
)

attr(cost_events, "diagnostics")
```

Choose `total_charge`, `total_cost` or `total_paid`. They are different measures,
not interchangeable estimates of expenditure. With `domains = NULL`, all
observed supported cost domains are selected. Unsupported domains and missing
source tables raise errors so that omissions require an explicit choice.

Costs are linked to events within their domain. A drug and a visit can share
an event ID without their costs being mixed. Patient and event identifiers are
kept as text when collected. The event's source date is used for allocation.
It is not necessarily a payment date, and a visit's amount is not automatically
spread over the visit's duration.

The extractor reads only. It does not create scratch tables or disconnect the
CDM. Patients and their follow-up dates are filtered in SQL before records are
collected. Queries use patient batches, so database latency can matter for large
cohorts. Server benchmarking is still needed.

Diagnostics report missing amounts, missing event dates and records outside the
selected patients' windows. Unmatched event references are counted across the
database for the selected domains because their patients cannot be identified.
An empty extraction is not proof that the source captures all expenditure.

## Estimate costs without a transition model

```r
costs <- estimateStateCosts(histories, cost_events)

costs$estimates
costs$intervalCosts
costs$unallocatedCosts
costs$diagnostics
```

For costs over the same follow-up periods:

```r
costs <- estimateStateCosts(
  histories,
  cost_events,
  periodBreaks = period_breaks,
  periodLabels = period_labels
)
```

Costs follow the event date, not the source-cycle rule used for transitions.
An interval that spans a boundary is split so each period receives only its
observed days. An event's amount stays on its recorded date and is not prorated.
For cost-only analyses, boundaries can fall inside an interval without requiring
a cycle length. A state with no observed days in a period has no estimate there.
This is different from an observed interval with no recorded cost.

`intervalCosts` retains the original `.row_id` alongside the period and clipped
dates. A split interval appears in more than one period, so interval counts
across periods are not counts of unique original intervals. Patient counts are
also not additive across periods.

You can supply a local data frame instead of using `extractCosts()`. It needs
`subject_id`, `event_date` and `amount`. Include `currency_concept_id`, `measure`
and unique `cost_id` values when available. Currency is kept separate in every
summary. Missing currency remains unknown. No conversion is performed.

Costs are assigned to the supplied non-overlapping intervals. An interval with
no matching event has zero **recorded** cost. A missing monetary amount remains
missing and makes the corresponding state total and daily mean `NA`.
`known_recorded_cost` separately reports the available amounts. The daily mean
uses total observed days, including intervals with no recorded cost.

Costs falling in unresolved intervals or outside the supplied intervals are
returned for review, not silently reassigned. For weekly trajectories, the
selected label describes the entire cycle. At this stage, a death-labelled
cycle retains its own costs. The function does not infer the last living state
or distribute that cycle's costs between clinical events.

## Optional combined call

```r
results <- TrajectoryMarkovAnalysis(
  histories,
  analyses = c("transitions", "costs"),
  costData = cost_events,
  periodBreaks = period_breaks,
  periodLabels = period_labels
)
```

The default `analyses = "transitions"` does not use a database. For costing,
supply either `costData` or `cdm`, not both. The result contains `transitions`,
`costs` and the prepared `trajectories`. No connection is stored in it.

## Publication

Haug M, Oja M, Pajusalu M, et al. Markov modeling for cost-effectiveness using
federated health data network. Journal of the American Medical Informatics
Association. 2024. [doi:10.1093/jamia/ocae044](https://doi.org/10.1093/jamia/ocae044).

The publication describes the earlier implementation. See `DESCRIPTION` for
the package's licence declaration. This refactor has not changed that declaration.
