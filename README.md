# TrajectoryMarkovAnalysis

TrajectoryMarkovAnalysis estimates transitions and recorded costs from patient
histories. The two workflows are independent. Transition estimation needs only
trajectories. Cost estimation can use a local expenditure table or records read
from a database through CDMConnector.

## Refactor status

The new interface accepts the current lowercase Cohort2Trajectory output. It
supports adjacent-cycle transition probabilities and descriptive costs
by state, either pooled or separated into follow-up periods. A deterministic
projection follows the resulting matrices over a specified number of cycles.
An optional entry model estimates first-cycle outcomes from starting states
supplied by the analyst.
These functions return R objects without opening plots, writing files, dropping
database tables or closing the caller's connection.
A separate costing function applies explicit cycle charges, state-change
charges and a startup fee to a projection.

This is not yet a release. Within-cycle models remain to be implemented. The older GUI,
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

## Estimate outcomes during the first cycle

The first weekly label can already contain a complication or death. To model
that first week's outcomes from a state defined before the week, estimate a
separate entry matrix:

```r
entry <- estimateEntryTransitions(
  histories,
  entryState = "Alive, no recorded pneumonia"
)

entry$estimates
entry$coverage
entry$subjects
```

The starting label is an explicit analyst definition, not something inferred
from a patient's later outcome. If patients start in different states, supply
an `entryState` data frame with `subject_id` and `entry_state` columns. It must
cover every trajectory subject exactly once. Starting states must belong to the
declared dictionary. Include a state in `prepareTrajectories(states = ...)` if
it is needed at entry but never appears in the recorded histories.

A complete first cycle contributes its known label. A shortened cycle also
contributes if it ends in an explicitly absorbing state, such as death. A
shortened living cycle is not counted as a complete event-free week. Missing
first cycles and unresolved labels are excluded as well. `subjects` records
each patient's inclusion or exclusion and `coverage` gives the counts.

Entry probabilities are conditional on an evaluable first cycle, separately
for each starting state. The default baseline mix uses these same patients.
This does not correct for censoring or establish that excluded patients have
the same risks. The probabilities describe first-cycle outcomes, not the exact
timing or ordering of events within that week. The existing adjacent-cycle
estimates remain separate and unchanged.

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

## Project the expected state distribution

Use a finite number of transition steps to examine the fitted model:

```r
projection <- projectCohort(transitions, nCycles = 52)

projection$occupancy
projection$steps
projection$flows
```

This is a deterministic calculation, not a random sample of patient paths. The
output contains the initial distribution and one distribution after each step.
For weekly data, 52 steps produce labels 0 through 52, with the last label
starting at elapsed day 364. These days locate cycle labels, not exact event
times. Each step uses the matrix for
its source cycle, so the first step uses the period containing day 0. `steps`
records which period was used. `cohortSize` defaults to 1. Set it to a population
size to also report expected patient counts, which can be fractional.

By default, the initial distribution comes from `entryCounts`. These are first
cycle labels, including any first-cycle deaths, not the states before index-week
events. The projection keeps that initial death probability and does not apply
the first-cycle deaths again. A warning identifies a default distribution based
on only the patients with known first-cycle labels. To supply a different starting
assumption, use a named probability vector:

```r
alternative_projection <- projectCohort(
  transitions,
  nCycles = 52,
  initial = c("Alive, no recorded pneumonia" = 1)
)
```

Omitted known states receive zero initial probability. This does not recover the
first-week event risk or turn the model into a pre-event baseline model. The
starting assumption is retained in the result metadata.

No probabilities are rounded or filled in. An unestimated transition row can
remain unused while its state has exactly zero probability. If the projection
needs that row, it stops and identifies the state, period and source cycle.
Invalid row sums and nonabsorbing behavior in declared absorbing states are also
rejected. There is no automatic normalization or replacement with a later matrix.

The last period's matrix continues for the requested horizon. This is a modelling
assumption, not evidence that the matrix remains appropriate indefinitely.
Projection does not add observational censoring or remove patients when their
original follow-up ended. Observed proportions with changing follow-up
denominators are therefore not a direct validation of this fixed-cohort trace.
Costs and utilities are not applied by this function.

`flows` reports the expected number taking each represented transition. Its
`transition_probability` is conditional on the source state, while
`probability_mass` is the share of the whole projected cohort taking that step.
`expected_count` scales that share by `cohortSize`. Absorbing self-transitions
retain patients in their state. They are not new deaths. Unoccupied unestimated
rows keep their `NA` conditional probabilities and have zero flow.

To include the entry matrix, supply it separately:

```r
projection_from_entry <- projectCohort(
  transitions,
  nCycles = 51,
  entry = entry,
  cohortSize = entry$diagnostics$eligible_subjects
)

projection_from_entry$entry$baseline_distribution
projection_from_entry$occupancy
```

The entry matrix is applied once, before the first label. Cycle 0 then contains
the first-cycle outcomes, including any first-cycle deaths. The next 51 steps
use the adjacent-cycle matrices as before. This produces 52 weekly labels,
numbered 0 through 51. With `nCycles = 0`, only the entry step is applied.
The baseline is stored separately and is not an extra week in `occupancy`.

Do not supply `initial` together with `entry`. Entry uses the baseline state mix
among its eligible subjects. The entry matrix, baseline mix and eligible patient
identifiers are retained in the projection. A different intervention effect or
within-cycle event sequence is not inferred by applying this entry model.

## Compare the projection with recorded states

```r
observed <- summariseOccupancy(histories)
check <- compareOccupancy(histories, projection)

check$comparison
check$coverage
check$diagnostics
```

`summariseOccupancy()` counts the supplied labels in each cycle. Once an
absorbing state has been recorded, that patient remains in it for later cycles.
These carried counts are separate from newly observed labels. Living patients
are not carried forward after their follow-up ends. A shortened final cycle
contributes its label once, just like a full cycle.

The observed proportion divides each state's known count by all available known
states in that cycle, including retained absorbing states. `coverage` reports
that denominator and the numbers censored, unresolved or missing within their
observation window. `cohort_fraction` instead divides by the original population
size. Neither measure fills in the unknown states of censored patients.

`compareOccupancy()` adds the projected probabilities and expected counts.
By default, it keeps only patients with a known first-cycle state throughout the
comparison and reports any exclusions. This matches the population used for
default projection entry probabilities. Use `population = "all"` explicitly
to include histories whose first cycle is missing or unresolved. Those patients
may contribute known labels later. The standalone observed summary includes
all supplied patients. Selecting a comparison population does not refit the
transition model.

If the projection used an entry matrix, the default comparison instead keeps
that entry model's exact eligible patients throughout follow-up. In particular,
it does not bring excluded patients with shortened living first cycles back
into the comparison. Their identifiers must be present in the supplied
histories and their first cycles must still be evaluable. Use `population =
"all"` explicitly to describe a different population. Both `entry$subjects`
and the projection's entry-eligible identifiers are patient-level outputs, not
disclosure-controlled summaries for sharing across sites.

The comparison reports different starting distributions and source counts.
Matching counts do not prove that the two objects describe the same patients.
Observed and projected cohort sizes are kept separate, with no automatic
rescaling.

Use this as a descriptive check, not a formal test of fit. The projection keeps
a fixed cohort, while the available-state denominator can shrink. In particular,
the observed death proportion can rise when living patients are censored, even
without new deaths. Always inspect the coverage alongside the proportions.
The functions return tables and do not open plots or write files.

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
The `cost_state` column identifies the state used to group these estimates.

Costs falling in unresolved intervals or outside the supplied intervals are
returned for review, not silently reassigned. For weekly trajectories, the
selected label describes the entire cycle. By default, a death-labelled
cycle retains its own costs. The function does not infer the last living state
or distribute that cycle's costs between clinical events.

## Assign terminal-cycle costs explicitly

A weekly cycle can be labelled as death even though it contains care delivered
before death. To attribute that care to a living state, supply a named mapping:

```r
terminal_cost_map <- c(
  "Death, no pneumonia history" = "Alive, no recorded pneumonia",
  "Death, pneumonia history" = "Alive, pneumonia history"
)

costs <- estimateStateCosts(
  histories,
  cost_events,
  periodBreaks = period_breaks,
  periodLabels = period_labels,
  terminalCostMap = terminal_cost_map
)
```

The names are source states and the values are their destinations for costing.
Sources must have been declared in `prepareTrajectories(absorbingStates = ...)`.
Destinations must be defined nonabsorbing states. If a destination has no observed
intervals, include it in `prepareTrajectories(states = ...)`. More than one terminal
state can map to the same destination. States omitted from the mapping are unchanged.

Both amounts and observed days move to the selected cost state. A three-day
terminal interval contributes three days, not a full week. An interval with no
cost record still contributes its observed days. Event dates, period boundaries,
currency separation and missing amounts retain their usual treatment.

The mapping uses the terminal label, not the preceding week's state. It therefore
also handles a first-cycle death and a terminal pneumonia-history cycle preceded
by a cycle without pneumonia. The attribution is an analyst choice, not evidence
of the exact event order within that week. Supply only the observed care interval
to be attributed, not an extended period of absorbing-state occupancy after death.

`intervalCosts` preserves the original `state_label` and adds `cost_state`.
The original `.row_id`, dates and allocated amounts remain available for review.
`estimates` groups by `cost_state` and reports the terminal intervals and days
included. Overall mapping diagnostics count original intervals and days once,
regardless of period splits or currencies. The exact mapping is retained in
`metadata$terminal_cost_map`.

Clinical trajectories and transition estimates do not change. A simulation must
use a compatible terminal-cycle charging rule. Multiplying these daily rates by
unchanged living-state occupancy alone will not generally reproduce observed
expenditure, because the cost denominator also includes the mapped terminal days.

## Calculate projected costs

`projectCosts()` applies explicit model charges to a projection. It does not
fit a cost model or convert the descriptive daily means above into cycle costs.
Transition and recorded-cost estimation remain independent.

The following amounts are only an arithmetic example, not clinical estimates:

```r
cycle_costs <- c(
  "Alive, no recorded pneumonia" = 100,
  "Alive, pneumonia history" = 150,
  "Death, no pneumonia history" = 0,
  "Death, pneumonia history" = 0
)

terminal_costs <- data.frame(
  from_state = c(
    "Alive, no recorded pneumonia",
    "Alive, no recorded pneumonia",
    "Alive, pneumonia history"
  ),
  to_state = c(
    "Death, no pneumonia history",
    "Death, pneumonia history",
    "Death, pneumonia history"
  ),
  cost = c(500, 800, 800)
)

projected_costs <- projectCosts(
  projection_from_entry,
  stateCosts = cycle_costs,
  transitionCosts = terminal_costs,
  entryCost = 1000,
  currency = "EUR",
  priceYear = 2026
)

projected_costs$byCycle
projected_costs$totals
```

There are three additive components:

- `stateCosts` charges each patient represented by each cycle's state label,
  including cycle zero. Supply every defined state, with explicit zeros where
  appropriate. These are amounts per full labelled cycle, not per day.
- `transitionCosts` adds a charge for each specified state change, including
  changes in the entry step. Omitted pairs have no additional charge. Self-pairs
  are rejected, so a terminal-care charge does not recur merely because a patient
  remains dead. The rule applies only to the represented direct change. A change
  straight to a death-with-history state does not invent an intermediate event.
- `entryCost` charges the projection's `cohortSize` once. It requires an explicit
  entry model. It is a single startup amount, not a baseline-state-specific fee.

Set a component to `NULL` to omit it intentionally. Missing supplied amounts are
errors. Charges for the same cycle add together, so choose recurring and event
amounts that do not count the same care twice. A nonzero recurring cost assigned
to an absorbing state will continue in later cycles. In the example, death-state
recurring costs are zero and terminal care is charged only on entry into death.

Without an entry model, the initial state mix does not create first-cycle state
changes. An initial death label therefore does not receive a transition charge.
With `nCycles = 0`, the function still charges the cycle-zero labels and any
explicit entry changes and startup fee. The separate pre-entry baseline never
receives another recurring cycle charge.

For changing prices or care costs, supply `stateCosts` as a table with `state`,
`cost` and `period`, and optionally add `period` to `transitionCosts`. Define the
cost periods with `periodBreaks` and `periodLabels`, just as for estimation.
State tables must cover every state in every cost period, including zeros.
Without a period column, the supplied charges apply in all cost periods.

Cost periods follow the destination label's start day, not the source period
used to select a transition matrix. Their boundaries must align with cycles.
The transition ledger retains both `probability_period` and `cost_period` so
this distinction can be checked. A rule applies to matching entry and later
changes alike, with the amount selected by its cost period.

These rules assume a full labelled cycle. They do not recover actual care days
from a shortened terminal interval or automatically reconcile with observed
expenditure. Calibrating charges from the recorded data requires compatible
denominators and timing. Currency and price year label the supplied amounts.
No inflation adjustment, currency conversion, discounting or utilities are
applied. `totals` reports expected cohort costs and costs per projected patient.
Detailed ledgers contain aggregate counts, not patient identifiers.

## Optional combined call

```r
results <- TrajectoryMarkovAnalysis(
  histories,
  analyses = c("transitions", "costs"),
  costData = cost_events,
  periodBreaks = period_breaks,
  periodLabels = period_labels,
  terminalCostMap = terminal_cost_map
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
