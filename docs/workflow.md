# Workflow reference

This guide describes the supported discrete modelling interface. Start with the
[quickstart](../README.md) for installation and a runnable example.

## Inspect objects

Printing a model or result gives a compact summary. Its tables, settings and
simulation resume state are still available. Metadata keeps the definitions used
for the run, such as states, cycle length, estimator, grouping, units, currency,
price basis, discounting and selected modelling rules. Counts and diagnostic
tables remain available. Fixed explanations of denominators, event ordering,
cost allocation and comparison limits are in the corresponding function help,
rather than repeated in every result. Timing conventions checked when comparing
summaries and the configuration needed to resume simulations are retained.

The summary distinguishes expected
cohort counts from sampled patients, and completed-interval outcomes from
labelled-cycle costs.

`transitionMatrix(fit)` returns a fitted full-cycle matrix. Add `stage = "onset"`
to select that stage, and `period` or `stratum` when the selection is ambiguous.
For a projection, supply a zero-based source `cycle` to inspect the matrix that
was actually used, including any intervention effect. A projection with no
completed steps has no such matrix. The accessor does not infer a single matrix
for simulations with changing patient profiles or future mortality.

The [C2T-to-ICER example](../inst/examples/c2t-to-icer.R) follows one set of
synthetic patients through both packages. It uses the same completed intervals
for deterministic and sampled economic outcomes.

## Fit observations within a cycle

`estimateTransitions()` uses adjacent recorded labels. When the study defines
separate events within a cycle, supply those observations to
`estimateStageTransitions()`. Each row describes one person's source state and
outcome in one named stage. The analyst defines the order and whether an
outcome was observed.

```r
observations <- data.frame(
  subject_id = rep(c("p1", "p2", "p3"), each = 2),
  source_day = 0,
  stage = rep(c("onset", "mortality"), 3),
  from_state = c("Stable", "Complication", "Stable", "Stable", "Stable", "Stable"),
  to_state = c("Complication", "Dead", "Stable", "Stable", NA, NA),
  observed = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
)
stage_fit <- estimateStageTransitions(
  observations, stageOrder = c("onset", "mortality"), cycleLength = 7,
  states = c("Stable", "Complication", "Dead"), absorbingStates = "Dead",
  identityStates = list(onset = "Complication")
)
stage_fit$estimates
stage_fit$coverage
```

If C2T already defined the states and cycle length, pass its output or prepared
histories as `definition` instead of repeating those settings:

```r
stage_fit <- estimateStageTransitions(
  observations, stageOrder = c("onset", "mortality"), definition = histories,
  identityStates = list(onset = "Complication")
)
```

Here `histories` must use the same clinical labels as the observations. The
stage order and within-stage rules remain explicit because they cannot be
recovered from a weekly label. A continuous trajectory definition supplies
labels and absorbing states, but needs an explicit `cycleLength` for the stage
observations.

These are synthetic observations. The third person's outcomes are unevaluable
and do not enter either denominator. The first person's complication followed
by death contributes to both stages. `identityStates` explicitly holds an
existing complication history unchanged during onset. Unsupported rows remain
unknown unless a structural identity was supplied.

Periods and covariate groups work as in the adjacent-cycle estimator. The fit
retains its observation rows for calibration. Keep that object in the permitted
patient-data environment. `createModelBundle()` retains aggregate models for
simulation.

For mortality calibration, use your study's fitted observations, with the
covariates needed to match the supplied background table, and name the mortality
stage. The small synthetic example above is for demonstrating the row format.

```r
calibration <- calibrateExcessMortality(
  study_stage_fit, background = mortality_bundle, deathStates = "Dead",
  stage = "mortality"
)
```

This uses the source observations behind that stage's estimates, including
observed first-cycle outcomes. A stage-model mortality extension replaces that
mortality stage and preserves the other declared stages. Background sources,
reference periods and future assumptions remain explicit inputs.

A cell with mortality probability one has no finite excess hazard. The default
stops for review. An explicitly supplied `probabilityCeiling` caps the probability
used in the hazard calculation and retains the raw estimate and reconstruction
difference. It does not change the observed event count.

## Account for completed intervals

Use `createRewardModel()` and `summariseOutcomes()` when the intended horizon is
a number of completed intervals. A 52-step weekly projection or simulation then
contributes 52 weeks. Its final endpoint adds no extra living time or recurring
cost. Upfront costs are charged once from the supplied initial population.

```r
rewards <- createRewardModel(
  stateCosts = c(Stable = 100, Complication = 200, Dead = 0),
  upfrontCosts = c(programme = 50),
  aliveStates = c("Stable", "Complication"),
  utilities = c(Stable = 0.8, Complication = 0.6, Dead = 0),
  costTiming = "onset", healthTiming = "start",
  discountTiming = "midpoint",
  costDiscountRate = 0.035, healthDiscountRate = 0.035,
  currency = "EUR", priceYear = 2026
)
projected <- projectCohort(
  stage_fit, nCycles = 4, initial = c(Stable = 1), cohortSize = 100
)
expected_outcomes <- summariseOutcomes(projected, rewards)
outcomeTotals(expected_outcomes)  # discounted cohort totals
outcomeTotals(expected_outcomes, perPatient = TRUE)
expected_outcomes$byCycle

sampled <- simulateTrajectories(
  createModelBundle(stage_fit), n = 100, initial = c(Stable = 1),
  nSteps = 4, terminalStates = "Dead", seed = 42
)
sampled_outcomes <- summariseOutcomes(sampled, rewards)
```

Use `updateRewardModel()` to vary an assumption without copying the definition
or editing the object's fields:

```r
higher_fee <- updateRewardModel(rewards, upfrontCosts = c(programme = 100))
undiscounted <- updateRewardModel(
  rewards, costDiscountRate = 0, healthDiscountRate = 0
)
```

Only named arguments to `createRewardModel()` may be changed. Each replaces the
whole field. `NULL` removes an optional field, for example `upfrontCosts = NULL`.
Other choices, including cost and health timing, stay as defined. To change
period boundaries, provide matching labels or `periodLabels = NULL` for default
labels. `periodBreaks = NULL` returns to one period. Update any period-specific
cost tables to match. The original rewards are unchanged.

`outcomeTotals()` returns three named numbers: `cost`, `life_years` and `qalys`.
By default these are discounted cohort totals. Use `discounted = FALSE` for
undiscounted amounts or `perPatient = TRUE` for means. It reads the stored
results, performs no new calculations and leaves uncomputed outcomes as `NA`.
It also accepts `summariseSimulation()` output, whose labelled-cycle timing
remains unchanged. To compare scenarios, pass the full summaries to
`compareScenarios()` so their population, horizon and accounting definitions
can be checked. Printing the comparison shows discounted cohort totals and
differences, followed by per-patient increments and ICERs.

These amounts and utilities illustrate the interface. Costs can use the state
at interval start, interval end, or immediately after a named stage. Health
outcomes make the same choice independently. Passing through a stage adds no
duration. Transition-cost tables can target either the net movement or a named
stage. Discount timing is separate from the choice of state.

State costs are amounts per cycle. You can supply an observed weekly mean
without converting it to a daily rate. `createCostModel()` instead converts
observed daily rates into modelled charges. Choose the cost denominator before
modelling, especially when observed final intervals are shorter than a week.
The package does not choose a pooled long-term cost assumption for you.

Use uncosted simulations with shared rewards. `summariseOutcomes()` rejects a
simulation that already has a cost model or `costRules`, so that existing charges
cannot be silently discarded or counted twice. `projectCosts()` and
`summariseSimulation()` retain their labelled-cycle accounting for those
workflows. Their initial and final labels each represent a full labelled cycle.
Do not mix the two time conventions in one scenario comparison.

The [ordered-stage economic example](../inst/examples/ordered-stage-economics.R)
uses the shared definitions for deterministic and sampled outcomes.

## Prepare the histories once

Use the data frame returned by `Cohort2Trajectory::createTrajectories()`.
Only four columns are required: `subject_id`, `state_label`, `state_start_date`
and `state_end_date`. Other columns are preserved. Patient identifiers retain
their values, including leading zeros and large identifiers supplied as text.

```r
library(TrajectoryMarkovAnalysis)

histories <- prepareTrajectories(trajectories)
```

Current C2T output carries its cycle length, ordered state labels and absorbing
states, including states absent from the current data and resolved merged
labels. TMA inherits these choices. It does not infer death or persistent
history from a label. Explicit arguments must agree with the attached
definition, including when preparing an already prepared object again.

`saveRDS()` and `readRDS()` preserve the definition. CSV does not. For plain data
frames, older C2T output or CSV imports, define the settings yourself:

```r
histories <- prepareTrajectories(
  rows, cycleLength = 7,
  states = c("Stable", "Complication", "Dead"), absorbingStates = "Dead"
)
```

Patient subsets keep the full state dictionary. If you deliberately change
state meanings or cycle lengths, construct new trajectories with matching
definitions rather than overriding the attached settings.

`START` and `EXIT` mark observation boundaries. They are not clinical states.
`OUT OF COHORT` is an unresolved observed interval, not death or loss of
observation. Dates include both endpoints. Only the final weekly cycle may
be shorter than the declared cycle length. Overlapping intervals and duplicate
rows must be resolved before estimation.

Each patient must have at least one observed interval. Boundary-only continuous
histories are not supported by the preparation step.

For interval-based costing of a plain data frame, leave `cycleLength` unset.
Continuous C2T output already declares no discrete cycle length.
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

These are transitions between cycle labels, not separate probabilities for
events within a cycle. Weekly labels cannot recover the exact order or dates
of events within a week. The optional stage composer below combines explicitly
supplied probabilities. It does not infer intermediate states from these labels.

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

### First-cycle outcomes by baseline group

Entry probabilities can also differ by patient characteristics. Add the baseline
group columns to the entry-state table and name them in `strata`:

```r
# baseline has one row per observed subject, with subject_id, entry_state,
# age_group and pgs_group measured before the first-cycle outcome.
entry <- estimateEntryTransitions(
  histories, entryState = baseline, strata = c("age_group", "pgs_group")
)
entry$strata
entry$estimates
entry$diagnostics$by_stratum
```

These values come only from the supplied baseline table, not from first-cycle
outcomes or later trajectory rows. Use `addCovariateGroups()` to apply the same
age and score cutoffs to baseline profiles and trajectory rows. Numeric strata
still define exact-value groups, so define bands explicitly where appropriate.
Every supplied subject needs baseline values, including subjects whose first
cycle is excluded. The package cannot verify when those measurements were taken.

Each group's counts, denominators and exclusions are retained. A group with no
evaluable outcomes remains in the output with unestimated nonabsorbing rows.
It is not replaced by a pooled or neighbouring group. Omit `strata` to estimate
the original pooled entry model.

Pass the stratified result to `simulateTrajectories(entry = entry)`. The simulator
matches baseline values once, before the first outcome. Later models can use
different covariates and continue to update as patients age. `projectCohort()`
does not mix entry groups automatically. For a group-specific deterministic
projection, select its model explicitly from `entry$models` and supply the
appropriate subsequent transition model.

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
For an explicit switch in patient simulation, use `extendModelBundle()` as
described below. This does not change how `projectCohort()` works.
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

## Compose stages within a cycle

Use `composeTransitionStages()` when a model has explicitly defined steps within
each cycle. The package applies stages in list order. It does not infer stages,
clinical meaning or event dates from the state names.

For example, a model could allow an episode to begin and then resolve in the
same cycle. These probabilities are illustrative, not estimates:

```r
states <- c("Stable", "Episode")
onset <- matrix(
  c(0.8, 0.2,
    0.0, 1.0),
  nrow = 2, byrow = TRUE, dimnames = list(states, states)
)
recovery <- matrix(
  c(1.0, 0.0,
    0.5, 0.5),
  nrow = 2, byrow = TRUE, dimnames = list(states, states)
)

staged_model <- composeTransitionStages(
  stages = list(onset = onset, recovery = recovery),
  cycleLength = 7
)
staged_projection <- projectCohort(
  staged_model, nCycles = 1,
  initial = c(Stable = 1), cohortSize = 100
)

staged_model$matrix
staged_projection$occupancy
staged_projection$stageFlows
```

Among 100 initially stable patients, the first stage gives 20 expected episodes.
The second gives 10 expected recoveries. The cycle therefore ends with 90 stable
patients and 10 in the episode state. The net cycle outcomes alone would hide
the episodes that resolved. Reversing the stages changes the result, so their
order is part of the model definition. No absorbing state is required.

Every stage uses the same state dictionary. Rows and columns are matched by
name, not by their supplied order. An identity row leaves a state unchanged in
that stage. If a state must remain absorbing throughout the model, declare it
in `absorbingStates` and supply identity rows for it in every stage. If a later
step depends on earlier history, that history must remain in the state
definitions. The package does not reconstruct it after states have been merged.

Use `periodBreaks` and `periodLabels` for changing stage probabilities. Each stage
can supply one matrix for all periods or a named list with one matrix per period.
All stages use the period containing the source cycle's start day. One pass
through all stages advances one cycle, not one cycle per stage.

The composer retains the supplied stage matrices and their combined products.
Projection checks they still agree. To change a stage, compose a new model rather
than editing only its combined matrix. Entire `NA` rows remain unavailable.
A combined starting-state row is unavailable if its paths reach such a row at
any stage. Unknown rows on paths with exactly zero probability do not affect it.

`flows` still describes net cycle changes. `stageFlows` separately reports the
conditional probabilities, whole-cohort probability masses and expected counts
for each stage. These are expected movements, not reconstructed patient paths
or distinct patient counts across stages. Stages add no person-time. Existing
`projectCosts()` rules use net cycle outcomes and do not charge intermediate
events or add recurring charges for stages. Use `createRewardModel()` and
`summariseOutcomes()` to account for a named stage explicitly.

These inputs specify a model. They do not estimate the stages from C2T output or
show that component probabilities apply to the same population. Ordinary
full-cycle estimates must not be reinterpreted as stage probabilities. Changing
one stage while retaining later probabilities is also an explicit assumption,
not an automatically estimated intervention effect.

Supply `initial` explicitly, or use a separately estimated `entry` model. No
starting population is inferred from the stage matrices. Entry still occurs
once before cycle zero and is not decomposed into the recurring stages. With
`nCycles = 0`, a staged projection has no stage movements. Ordinary models need
no stage specification and continue to work as before.

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
kept as text when collected. By default, the full amount is assigned to the
event's source date. This is not necessarily a payment date. To spread charges
over an event's duration, select `allocation = "duration"` explicitly.

For charges linked to visit details, use `domains = "Visit Detail"`. The
extractor links each charge to `visit_detail_id`, keeps its parent
`visit_occurrence_id` and uses `visit_detail_start_date` as the event date.
Duration allocation also uses `visit_detail_end_date`. Visit filters apply to the parent visit,
so `visitConceptIds` selects `visit_concept_id`, not `visit_detail_concept_id`.

### Choose when a cost is assigned

The two allocation modes answer different questions. Start-date allocation
assigns the full recorded amount when the event begins. Duration allocation
spreads that amount evenly over the event's inclusive calendar days:

```r
start_events <- extractCosts(
  cdm, histories, measure = "total_charge", domains = "Visit Detail",
  allocation = "start"
)
start_costs <- estimateStateCosts(histories, start_events)

duration_events <- extractCosts(
  cdm, histories, measure = "total_charge", domains = "Visit Detail",
  allocation = "duration"
)
duration_costs <- estimateStateCosts(histories, duration_events)
```

The estimator reads the extraction's allocation setting, so it need not be
repeated. Duration extraction retains the original start date, end date and
full amount. It includes events that start before follow-up but overlap it.
Start-date extraction excludes those records. A start-date extraction therefore
cannot be switched to duration allocation later. Extract again with the chosen
mode to avoid missing overlapping events.

Visit, Visit Detail, Drug and Device use their source end-date fields. The other
supported cost domains are treated as one-day events. Missing end dates or end
dates before the start cause an error in duration mode. They are not silently
replaced with the start date.

The daily share always uses the **full event duration**, not just its observed
portion. A seven-day event costing 700 contributes 100 per observed day. If only
five days fall in resolved trajectories, 500 is assigned and 200 remains
unallocated. State and period changes split the assigned shares. This differs
from clipping the event first and spreading its entire amount over the remaining
days. Analyses that used that approach will not necessarily reproduce the same
state-cost estimates.

Uniform allocation is an explicit accounting assumption, not evidence that care
was delivered evenly. Neither mode changes the measure, converts currencies or
adjusts prices for inflation. The examples use `total_charge` deliberately.
Choose the appropriate measure, then use `adjustCostPrices()` for an explicit
price-year adjustment before allocation.

The extractor reads only. It does not create scratch tables or disconnect the
CDM. Patients and their follow-up dates are filtered in SQL before records are
collected. Queries use patient batches, so database latency can matter for large
cohorts. Server benchmarking is still needed.

Diagnostics report missing amounts, missing event dates and records outside the
selected patients' windows. Unmatched event references are counted across the
database for the selected domains because their patients cannot be identified.
An empty extraction is not proof that the source captures all expenditure.

### Choose all costs or costs linked to selected visits

By default, `extractCosts()` includes all recorded costs from the selected
domains within each patient's follow-up. To restrict them to particular visits,
add `visitFilter`:

```r
all_costs <- extractCosts(cdm, histories)

visit_costs <- extractCosts(
  cdm, histories,
  domains = c("Visit", "Drug", "Procedure"),
  visitFilter = list(visitIds = c("123", "456"))
)
```

The visit IDs above are illustrative. You can instead select visits containing
particular clinical concepts. Here `condition_concepts` is your vector of
resolved standard concept IDs:

```r
visit_costs <- extractCosts(
  cdm, histories,
  domains = c("Visit", "Drug", "Procedure"),
  visitFilter = list(eventConcepts = list(Condition = condition_concepts))
)
selected_costs <- estimateStateCosts(histories, visit_costs)
```

A matching condition selects the visit. All recorded costs linked to that visit
in the chosen cost domains are then included, even when a drug or procedure has
a different concept. Repeated diagnoses do not duplicate those costs. This is
expenditure associated with the selected visits, not an estimate of expenditure
caused by the qualifying condition.

Three filter fields are available:

- `visitIds` identifies specific visit records.
- `visitConceptIds` selects values of `visit_concept_id`, such as the concepts
  defining inpatient or outpatient visits. It does not use the provenance field
  `visit_type_concept_id`.
- `eventConcepts` selects standard concepts recorded in Condition, Drug,
  Procedure, Device, Measurement or Observation. Supply a named list of ID
  vectors, such as `list(Condition = condition_concepts, Procedure = procedure_concepts)`.

Any ID within a vector can match. Any of the supplied event domains can qualify
a visit. When several filter fields are supplied, the visit must satisfy all
of them. Concepts match exactly, without automatic descendant expansion. An
explicitly empty ID vector selects nothing. Omit unused fields rather than
setting them to `NULL`.

The qualifying clinical record must occur within the patient's follow-up.
In start mode the cost's event date must also be within follow-up. In duration
mode its event interval must overlap follow-up, and only the observed portion
is allocated. A visit can begin before follow-up and still have eligible events
during it. Selecting a visit does not by itself include its whole bill. A
qualifying record later in follow-up can select earlier costs from that same
visit, so this is retrospective visit selection, not a rule for prospective
treatment eligibility.

Linkage follows recorded patient and `visit_occurrence_id` values. It never
assigns visits using overlapping dates. Costs without a usable visit link are
excluded in filtered mode and reported in diagnostics. A selected source table
without a visit-link field, such as the standard Specimen table, raises an error.
Exclude that cost domain explicitly or use all-cost mode. No visit link is
required for all-cost extraction.

The output retains `visit_occurrence_id` as text, including large identifiers.
Inspect `attr(visit_costs, "visit_filter")` and `attr(visit_costs, "diagnostics")`
to review the selection. Within-window cost rows are separated into extracted
rows, missing visit links, unmatched or wrong-patient visit references, and
valid visits that did not meet the filter. These visit checks are not performed
in all-cost mode. Diagnostics describe cost rows, not unique visits or the
completeness of the clinical records used to qualify them.

Cost selection does not remove patients or days from `estimateStateCosts()`.
It changes the cost numerator while keeping all observed state-days in the
denominator, including days without selected visits. The resulting daily mean
is not the average cost per selected visit. The result retains the extraction
scope and criteria in its metadata.

Check whether visit-level charges already include itemised event costs before
combining domains. The package preserves distinct cost records and does not
attempt to remove overlapping charges by guessing how the source bills care.

### Adjust recorded amounts to a common price year

Call `adjustCostPrices()` on the full event records before `estimateStateCosts()`.
Supply your own index table and an explicit source-year column; no index is
built in or downloaded. This demonstration assumes the records use currency
concept `44818568` and source years covered by the table. The index values are
wholly synthetic, not published prices:

```r
price_index <- data.frame(
  year = c(2020L, 2025L),
  index = c(100, 120),
  currency_concept_id = "44818568"
)
priced_events <- adjustCostPrices(
  cost_events, priceIndex = price_index, priceYear = 2025L,
  sourceYear = "event_date",
  source = paste(
    "Synthetic geography and demonstration series; version 1;",
    "annual values; no partial-year observations"
  )
)
observed_costs <- estimateStateCosts(histories, priced_events)
healthcare <- createCostModel(
  observed_costs, unit = "day", currency = "EUR", priceYear = 2025L,
  terminal = "none"
)
```

The table needs `year` as numeric integer years, `index` as positive finite
values, and `currency_concept_id` as exact source codes, with one row per year
and currency. It must cover every source year and the positive integer target
`priceYear` for each currency in the records. Codes are matched exactly; missing
or unknown currency codes must be resolved before adjustment. The `source`
string records the geography, series, version and any partial-year limitation.
Select an index appropriate to the cost measure and document that choice.

`sourceYear` names a column containing `Date` values or numeric integer years.
Choosing `"event_date"` explicitly uses the original event start year for the
whole charge, including events that cross a calendar year or start before
follow-up. It does not use the year of an allocated fragment. A different
accounting convention requires a source-year column prepared by the analyst.

The returned data frame replaces `amount` with
`nominal_amount * index[target year] / index[source year]`, retaining the input
amount in `nominal_amount` and adding `source_price_year`, `price_factor` and
`target_price_year`. Missing amounts stay missing and credits retain their sign.
The original input and its extraction attributes remain unchanged; those
attributes, including allocation and visit selection, are retained on the
returned data. Duplicate index keys, missing coverage, invalid index values,
nonfinite adjusted amounts and repeated adjustment cause errors.

Provenance is stored in `attr(priced_events, "price_adjustment")` and carried
into `observed_costs$metadata$price_adjustment` and
`healthcare$metadata$price_adjustment`. A cost model's `priceYear` must match the
recorded target year. For unadjusted inputs it remains the analyst's assertion.
The currency label in `createCostModel()` is also an assertion about the source
code; it performs no currency conversion. Price adjustment and outcome
discounting are separate operations.

See [`inst/examples/price-adjustment.R`](../inst/examples/price-adjustment.R) for a
runnable synthetic example with an event spanning two calendar years.

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

Costs follow the chosen allocation rule, not the source-cycle rule used for
transitions. An interval that spans a period boundary is split so each period
receives only its observed days. In start mode the amount stays on the event
date. In duration mode each period receives the share of the event covered by
its resolved observed days.
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
For local duration records, also supply `event_end_date` and call
`estimateStateCosts(histories, costData, allocation = "duration")`. Plain local
records otherwise use start-date allocation. Keep the original event dates and
full amount, including any portion outside follow-up.

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

For duration allocation, `allocatedCosts` and `unallocatedCosts` provide the
allocation audit. Their `amount` is the assigned or unassigned share, while
`original_amount` retains the full source amount and `.cost_row_id` identifies
the input row. `allocation_days` and `allocation_fraction` explain the share.
After `adjustCostPrices()`, `original_amount` is the full adjusted charge and
`nominal_amount` is the full unadjusted charge. Both repeat on fragments and
must not be summed across them; sum `amount` to reconcile adjusted shares.
Original event dates remain available alongside fragment dates where applicable.
An outside remainder can combine days in gaps and outside follow-up, so its
original dates do not describe a single contiguous unallocated interval.

A partially observed record can appear in both allocated and unallocated
diagnostics. Those record counts are not mutually exclusive. Within state and
period summaries, `cost_records` counts allocated fragments, not distinct source
records across the whole result. Use `.cost_row_id` to identify the original
records and reconcile their shares. Missing amounts remain missing in every
affected fragment.

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

For composed models, `transitionCosts` still charges net cycle changes, not
`stageFlows`. A path that leaves a state and returns within the cycle has no
net change, even though the stage ledger records both movements. Stage-specific
charges are not applied automatically.

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

## Estimate models for changing patient groups

Supply `strata` to estimate transitions or costs for combinations of columns
already present on your trajectory rows:

```r
group_columns <- c("age_group", "pgs_group", "score2_group")

transitions <- estimateTransitions(histories, strata = group_columns)
observed_costs <- estimateStateCosts(histories, costData, strata = group_columns)

transitions$strata
transitions$estimates
observed_costs$estimates
```

One patient can contribute to several groups as their values change. Transition
pairs are formed before grouping. Each pair belongs to its source row's group,
including pairs whose destination belongs to a different group. The original
index date still defines follow-up periods. Reaching a new age group does not
restart follow-up or create a new entry observation.

For costs, the row's covariates determine where both expenditure and observed
days are counted. A row with no cost record still contributes days. Splitting
an interval across follow-up periods preserves its covariates. Values apply to
the whole supplied row, not an inferred change partway through it. Clinical
states remain separate from the grouping variables.

Each model retains transition counts and denominators. Stratified estimates
also show distinct contributing patients. A patient can appear in several
groups, so adding those patient counts does not give a unique cohort count.
Unestimated probability rows remain `NA`. No automatic pooling, smoothing or
fallback to another group is performed. Missing clinical-row covariates are an
error. Supply an explicit category if missingness is itself part of your design.

This is subgroup estimation, not regression adjustment. Numeric columns used
directly in `strata` form exact-value groups. Define bands where appropriate,
particularly before combining several continuous scores.

### Reuse the same grouping rules

`addCovariateGroups()` retains raw values and adds the named grouping columns.
For example, the following cutoffs illustrate the interface, not recommended
clinical thresholds. The input already contains `birth_date`, `score2` and a
categorical `pgs_group` supplied by the analyst:

```r
groups <- list(
  age_group = list(
    column = "age", breaks = c(0, 65, 75, Inf),
    labels = c("Under 65", "65 to 74", "75 and older")
  ),
  score2_group = list(
    column = "score2", breaks = c(-Inf, 10, 20, Inf),
    labels = c("Lower", "Middle", "Higher")
  )
)

grouped_rows <- addCovariateGroups(
  trajectories, groups, dateColumn = "state_start_date"
)
histories <- prepareTrajectories(
  grouped_rows, cycleLength = 7, absorbingStates = "Dead"
)
transitions <- estimateTransitions(histories, strata = group_columns)
bundle <- createModelBundle(transitions, groups = groups)
```

The explicit `dateColumn` option calculates completed age from `birth_date`.
Groups include the lower boundary and exclude the upper boundary. In non-leap
years, a February 29 birthday is reached on March 1. Birth-year-only data need an
explicit analyst approximation or update rule, not an invented birthday.

The bundle holds models and grouping definitions, not a training population.
You can retain descriptive cost estimates with `costs = observed_costs`, but
they are not automatically converted to simulation charges. To apply them, first
use `createCostModel()` as described below. Groups are matched
by their actual column values. Internal stratum IDs are local to a bundle and
should not be used as clinical categories.

## Generate and continue patient histories

`projectCohort()` calculates expected cohort counts. `simulateTrajectories()`
instead draws individual next states from a model bundle. It accepts ordinary
and stratified models. For each patient it selects the model from the current
covariates and the period containing the source elapsed day.

For an ordinary model, generate a population from a starting state distribution:

```r
ordinary_bundle <- createModelBundle(estimateTransitions(histories))
simulation <- simulateTrajectories(
  ordinary_bundle, n = 1000,
  initial = c("Stable" = 0.8, "Complication" = 0.2),
  nSteps = 52, terminalStates = "Dead", seed = 42
)
```

Use your actual state labels. Without `entry`, this starts at the supplied state
distribution, not an inferred pre-event state. An entry model is never applied
automatically.

A stratified model also needs starting covariates. Supply `population` as one
current snapshot per source patient, with `state_label` and the required
covariates. It is not the full repeated-row trajectory table. `subject_id` is
optional, but must be unique when supplied. `elapsed_day` defaults to zero and
must align with the cycle length. Set it explicitly when continuing patients
from later follow-up. It does not restart at zero when a patient's group changes.

```r
simulation <- simulateTrajectories(
  bundle, population = initial_patients,
  n = 1000, replace = TRUE,
  nSteps = 52, terminalStates = "Dead", seed = 42
)
```

Omit `n` to use every supplied profile once. Sampling with replacement creates
new `simulation_id` values for repeated draws and retains the original
`source_subject_id`. An `initial` state vector can deliberately override the
sampled states independently of the covariates. Alternatively, omit population
and supply `n` plus a joint `initial` table containing `state_label`, covariates
and `probability`. Sampling complete profiles preserves their joint distribution.
Independent sampling of ages, scores and states is not assumed.

### Start before first-cycle outcomes

If the supplied population describes patients before their first cycle, pass
the separately estimated entry model explicitly:

```r
entry <- estimateEntryTransitions(histories, entryState = "Stable")
simulation <- simulateTrajectories(
  bundle, population = baseline_patients,
  entry = entry, nSteps = 51,
  terminalStates = "Dead", seed = 42
)

simulation$entry$population  # Supplied pre-entry profiles after any sampling
simulation$entry$transitions # Entry outcomes drawn once for nonterminal patients
simulation$trajectories      # First-cycle outcomes, then 51 later steps
```

`baseline_patients` needs one row per person, with `state_label` describing their
pre-entry state and any required covariates. Alternatively, supply `n` and an
explicit `initial` distribution. The simulator does not select the entry
estimator's training patients or baseline mix for you. Its eligibility counts
and matrix or group-specific matrices are retained, but its training patient
identifiers are not copied.

All entry profiles start at cycle zero and elapsed day zero. The entry draw
produces the first outcome label at those same coordinates. It is not an extra
week or an additional `nSteps`. With `nSteps = 0`, the result contains only that
first outcome label. The pre-entry profiles are stored separately and add no
living time. Entry does not call `updateCovariates`, and resuming does not repeat
it. The [entry example](../inst/examples/entry-simulation.R) shows the full workflow.

Without `strata` at estimation, entry probabilities remain pooled within each
starting state. With a stratified entry model, each patient's baseline covariates
select one entry matrix. Bundle grouping rules are applied before matching.
Entry-only group columns must be supplied explicitly, for example by applying
`addCovariateGroups()` to the baseline profiles. They need not be used by later
transition or cost models. A named state distribution alone cannot supply these
covariates. Use patient profiles or a joint `initial` table instead.

Unknown entry groups and unestimated source rows are errors, including profiles
not drawn when sampling. If a named `initial` vector overrides the supplied
states, every positive-probability starting state must be supported for each
profile. Zero-probability joint profiles do not need an entry model, but all
supplied entry clocks must still be zero. Entry selection is not repeated after
age or risk groups change. Later transitions use the current covariates as usual.

For stratified entry, `simulation$entry$matrices` and `simulation$entry$strata`
retain the models and their group meanings. Entry draws and effect records carry
`entry_stratum_id`, which is separate from the later transition model's
`stratum_id`. The [stratified entry example](../inst/examples/stratified-entry-simulation.R)
shows baseline selection, a later age-group change, treatment effects and fees.

Outcome-based costs use the first outcome label. Explicit transition charges can apply to an
entry state change. An estimated cost model with `terminal = "entry"` can charge
a patient who becomes terminal during entry, but not someone already terminal
at baseline. Once-only and recurring rules still apply only to nonterminal
outcome labels. Use a cost rule with `type = "entry"` for a fee paid before the
entry outcome is drawn. It uses baseline state and eligibility, excludes
patients already terminal at baseline, and remains paid if a patient dies in
the first cycle. It adds no extra week of care or survival. A post-entry `once`
charge instead depends on the first eligible nonterminal outcome label.

An effect rule with `applyAt = "entry"` changes the entry probabilities only.
Use `applyAt = "both"` to apply it to entry and later transitions, or leave the
default `applyAt = "transitions"` for later transitions only. Fees and effects
are independent. When they describe the same treatment, use a shared baseline
eligibility flag in their callbacks. The [entry example](../inst/examples/entry-simulation.R)
compares such scenarios from identical baseline profiles. Entry outcomes do
not identify intermediate events, their timing or a causal downstream effect.
Summaries use the pre-entry profiles for scenario comparisons and prepaid fees.
Other costs and health outcomes use the recorded outcome labels. Comparisons reject
mixing a pre-entry starting population with an already-labelled starting
population.

### Update covariates between steps

When profiles contain `birth_date` and `index_date`, the simulator advances age
using `index_date + elapsed_day`. The same bundle grouping rules are then
reapplied. Without birth dates, a supplied age stays fixed unless a custom rule
changes it. Other scores are not automatically recalculated or forecast.

An optional `updateCovariates(current, previous)` function receives nonterminal
patients after a transition and clock advance. Return `simulation_id` plus only
the covariates to change. For example, given profiles with a `treatment_active`
column initially set to TRUE:

```r
update_treatment <- function(current, previous) {
  data.frame(
    simulation_id = current$simulation_id,
    treatment_active = current$elapsed_day < 28
  )
}
```

The updated values affect the next transition, not the one that just occurred.
The callback cannot rewrite identities, clinical outcomes or the clock. Update
raw covariates rather than grouping columns owned by the bundle. The new stratum
ID is resolved only after the callback finishes. Any needed longer-term memory,
such as a treatment-start day, must be retained in patient covariates. Measured
values from the patient's observed future are not a default update rule.

### Apply an intervention effect

Use `transitionEffects` to change a particular transition for eligible patients.
These are scenario assumptions supplied by the analyst, not effects estimated
from the observed associations. For example, with a `treatment_active` flag in
the starting profiles:

```r
effects <- list(
  prevention = list(
    from = "Stable", to = "Complication",
    multiplier = 0.75, redistributeTo = "Stable",
    startDay = 0, endDay = 28,
    eligible = function(p) p$treatment_active
  )
)

simulation <- simulateTrajectories(
  bundle, population = initial_patients,
  transitionEffects = effects,
  nSteps = 52, terminalStates = "Dead", seed = 42
)
```

Use your own state labels. For a source row with probabilities 70% stable,
20% complication and 10% death, this rule produces 75%, 15% and 10%. It reduces
the complication probability by 25% relative to that row, not by 25 percentage
points. It is not a hazard ratio or a multiplier on lifetime risk.

`redistributeTo` names the other probability that changes. A multiplier below
one adds the removed probability to that destination. A multiplier above one
takes probability from it. The two destinations must differ, and the result
must remain a valid probability row. No other probabilities are rescaled.
An absorbing source state cannot be modified. A zero baseline target probability
remains zero, and an unavailable source row cannot be repaired by an effect.

The simulator selects the patient's source model and period, applies an eligible
effect, then draws the next state. The source-day window includes `startDay`
and excludes `endDay`. Finite boundaries must align with the cycle length.
For weekly cycles, the example affects steps starting on days 0, 7, 14 and 21.
By default, it does not change the initial outcome. An eligibility
callback sees only source patients matching the rule's state and time window.
Covariate updates made after a transition affect the following step.

With an entry model, `applyAt = "entry"` changes only the first-cycle outcome,
and `applyAt = "both"` includes entry and adjacent-cycle draws. Both the entry
draw and the first adjacent transition use source day zero. With weekly cycles,
`applyAt = "both", endDay = 28` therefore covers entry plus four adjacent draws,
or five outcome labels. To cover exactly the first four labels, use a separate
entry rule and an `applyAt = "transitions", endDay = 21` rule. These cover entry
and the three later draws starting on days 0, 7 and 14. This is a count of
covered labels, not an inferred biological treatment duration. Rules requiring
entry are rejected when no entry model is supplied.

You can supply several named rules for different patients, states or periods.
For combined-cycle effects, at most one rule may apply to a patient per step.
For stage effects, at most one rule may apply to the same reachable source in
that stage. Rules on separate stages can act in the same cycle. There is no
implicit priority or multiplication of overlapping effects. Cost
rules remain independent. To charge for the intervention, supply `costRules`
separately and use consistent eligibility definitions.

The fitted bundle and entry matrix are unchanged. `simulation$effects` records
effects on adjacent-cycle steps, while `simulation$entry$effects` records entry
effects. Both show the applied rule and the two probabilities before and after
modification. For each phase with configured effects, its transition ledger
also includes the baseline probability of the sampled destination alongside
its actual `transition_probability`. Rules and ledgers are retained across
saved-and-resumed runs. Entry draws and fees are not repeated.

For a composed model, add `stage = "onset"` to target that named stage. Its
modified probabilities apply before the later stages. This preserves paths in
which a complication and death occur in the same cycle. Use the same rules in
`projectCohort(..., transitionEffects = effects)` for expected cohort outcomes.
The deterministic projection accepts rules without eligibility callbacks because
it has no individual patient profiles.

Without `stage`, an effect changes the combined cycle probabilities. Stage and
combined-cycle effects cannot be mixed in one run. Composed simulations without
combined-cycle effects retain each sampled movement in `stageTransitions`.
Combined-cycle effects retain net outcomes only, because the modified row does
not specify the intermediate path. Later outcomes follow the supplied model,
so their response to a prevented transition remains a scenario assumption.

The runnable [intervention example](../inst/examples/intervention-simulation.R)
compares standard care and treatment from the same starting patient states.
An explicit seed makes each run repeatable, but the same seed does not guarantee
matched patient-level random draws after their paths diverge. These simulated
differences are not estimates of treatment effectiveness from the source data.

### Apply estimated healthcare costs

`createCostModel()` turns the estimated daily means into an explicit charging
model. Supply the cost units, currency, price year and terminal policy:

```r
healthcare <- createCostModel(
  observed_costs,
  unit = "day", currency = "EUR", priceYear = 2026,
  terminal = "entry"
)
bundle <- createModelBundle(transitions, costs = healthcare, groups = groups)

simulation <- simulateTrajectories(
  bundle, population = initial_patients,
  nSteps = 52, terminalStates = "Dead", seed = 42
)
```

Use the currency and price year of your actual amounts. These arguments label
the data. They do not adjust historical prices or convert currencies. Apply
`adjustCostPrices()` before estimation when historical amounts need adjustment;
the model retains its provenance and rejects a different `priceYear`. Without
that provenance, the supplied year remains your assertion. If the estimates
contain several source currencies, select one with
`currencyConceptId`. An unknown source currency also requires explicit
selection, such as `currencyConceptId = NA_character_`, and your declaration of
what currency those amounts represent. Unknown and known amounts are not pooled.

For each label, the simulator selects the rate by cost state, current covariates
and cost period. Cost models can use different covariates and periods from the
transition models. The grouping rules in the bundle can serve either model.
Patients must have the values needed by both. Internal transition group IDs
are never used to look up costs.

With `unit = "day"`, each label covers a full cycle of days. When a cost-period
boundary falls inside that cycle, the charge is split between the rates. For
example, three days at EUR 10 and four days at EUR 15 cost EUR 90. The state and
covariates remain fixed within the label. A birthday or score change partway
through it is not reconstructed.

To store fixed cycle charges instead, choose `unit = "cycle"` and supply
`cycleLength = 7` for a weekly model. Each daily mean is multiplied by seven.
This is not an independently estimated mean per visit or observed interval.
The simulation must use the same cycle length, and cost-period boundaries must
align with it. Neither mode adds a discount rate or parameter uncertainty.

The terminal policy is also explicit:

- `terminal = "entry"` charges one full label when a patient enters a declared
  terminal state. The fitted `terminalCostMap`, if any, selects its cost state.
  Otherwise the terminal state's own estimated rate is used.
- `terminal = "none"` excludes terminal labels. If terminal amounts and days were
  pooled into living-state rates during estimation, they remain in those rates.

Neither policy charges later terminal residency or an initially terminal
patient, for whom no entry has been simulated. Mapped terminal states must be
listed in `terminalStates`. Terminal-entry costing uses the retained covariates
from the source snapshot and the destination label's cost period. It does not
invent a recovery state, an exact death date or a within-cycle event order.
A full-cycle terminal charge is an assumption. It does not recover the shorter
observed care time of a patient who died partway through a cycle.

Missing cost combinations cause an error when needed, and selected estimates
with missing amounts cannot become zero rates. A measured zero remains zero.
The ledger records the selected period, cost state, unit rate and quantity so
each charge can be checked. Healthcare has its own `healthcare` component in
`simulation$totals`. The model and its conventions are retained when saving and
resuming a run.

A runnable synthetic example is in
[`inst/examples/estimated-cost-simulation.R`](../inst/examples/estimated-cost-simulation.R).
It covers a changing age group, a cost period inside a weekly cycle, mapped
terminal care and a separate startup fee.

### Add intervention and manually specified charges

Cost rules are named components. They do not alter transition probabilities.
Supply a currency and price year when no fitted cost model provides them:

```r
cost_rules <- list(
  initiation = list(
    type = "once", amount = 1500,
    eligible = function(p) p$treatment_active
  ),
  maintenance = list(
    type = "recurring", amount = 20,
    eligible = function(p) p$treatment_active
  ),
  complication_care = list(
    type = "recurring", amount = 100, states = "Complication"
  )
)

simulation <- simulateTrajectories(
  bundle, population = initial_patients,
  nSteps = 52, updateCovariates = update_treatment,
  costRules = cost_rules, currency = "EUR", priceYear = 2026,
  terminalStates = "Dead", seed = 42
)
```

These amounts illustrate the syntax only. A `once` rule charges each simulated
patient at the first eligible label and remembers that payment. A `recurring`
rule charges every eligible label. A `transition` rule with `from` and `to`
charges a specified state change, not continued residency. An `entry` rule
requires an entry model and pays before its outcome, including for eligible
baseline patients who die during entry. Its callback and state restrictions
use the pre-entry profiles, while other rules use outcome labels. All rules can use
`states`, `startDay` and `endDay` restrictions. Eligibility callbacks inspect
only candidate rows after these basic restrictions and prior-payment checks.
When a fitted cost model is attached, its currency and price year apply to these
rules too. Conflicting labels are rejected. The name `healthcare` is reserved
for the fitted costs in that case. Add only charges not already included in
those estimates. For example, omit the manual `complication_care` component if
the estimated healthcare rate already covers that care.

Ordinary label-based charges use the initial label and each completed destination
label. Therefore `nSteps = k` produces up to `k + 1` charged labels. A prepaid
entry fee is a separate amount, not an additional label. An initial
snapshot does not imply a preceding transition. Amounts are per full label, not
prorated daily means. Use `once` for treatment starting at an eligible labelled
state, or `entry` for treatment paid before the first outcome. Manual rules do
not convert observed daily means.
Keep ordinary care and intervention charges in separate components,
and avoid charging the same expenditure twice.

### Specify transitions beyond fitted follow-up

Attach a separate transition model when the original estimates should no longer
be used. The original bundle and its estimates are left unchanged:

```r
extended <- extendModelBundle(
  bundle,
  fromDay = 52 * 7,
  transitions = supplied_transitions,
  source = "External follow-up model, version 1"
)

simulation <- simulateTrajectories(
  extended, population = baseline_patients,
  nSteps = 104, terminalStates = "Dead", seed = 42
)
simulation$metadata$extrapolation
simulation$transitions[c("source_day", "model_phase", "model_source",
                         "model_day", "source_stratum", "source_period")]
```

`supplied_transitions` uses the same model format as `createModelBundle()`. It
can come from a separate set of histories or from explicit matrices supplied
through `composeTransitionStages()`. The source label records where the model
came from. It does not establish its suitability for the study population.

`fromDay` must be positive and aligned with the cycle length. Selection uses the
source day. With seven-day cycles and a cutoff of 364, the step from day 357 to
364 still uses the original model. The step from day 364 to 371 uses the supplied
model. Its period clock starts at zero at the cutoff. For example, a supplied
period beginning at day 14 starts at global day 378. Patient age, treatment
windows, costs and discounting keep their original clocks.

The supplied model may have different covariate groups and periods. Pass its
numeric grouping rules through `groups`, just as for `createModelBundle()`.
Current patient values select its probabilities after the switch. The simulator
does not require a matching original-model group at that point, but it does
require support in the selected extrapolation model. Missing groups or
probability rows are errors, not a request to reuse the original estimates.
Grouping needed by the original healthcare cost model still applies.
Other derived groups are cleared when their rules stop applying, so they cannot
silently retain an old age or risk category. Raw covariates are retained. A late
rule cannot overwrite a directly supplied cost category or another rule's raw
input. Use distinct names when the definitions differ.

Both models must use the same clinical states, absorbing declarations and cycle
length. There is one handover per bundle. To compare different assumptions,
extend the original bundle separately for each scenario. Nothing infers a
cutoff, adds background mortality, smooths probabilities or converts hazards.
Entry probabilities and the configured costs are not replaced. The last period
of the supplied model continues for the remaining requested horizon, which is
also an analyst assumption.

Generated snapshots identify their current `model_phase`, either `base` or
`extrapolation`. Transition and treatment-effect records identify the phase,
source label and model-relative day used for each draw. Internal stratum IDs
are local to that phase, so read them together with `model_phase`. `base` means
the original bundle, which may itself contain estimated or specified models.
Terminal snapshots retain their last selected phase because no further draw
occurs. Summaries and scenario comparisons retain the extrapolation definitions.

This handover is currently supported by patient simulation and saved/resumed
runs. It is not applied automatically by `projectCohort()`. The
[extrapolation example](../inst/examples/extrapolation-simulation.R) shows fitted
and supplied probabilities, global and model-relative clocks, and a resumed run.

### Use a supplied background-mortality table

`createMortalityTransitions()` converts death probabilities from a stated time
interval into cycle probabilities. You supply the table. The function does not
download mortality data or interpret a source label as a data connection.

```r
# Synthetic annual probabilities, not clinical estimates.
mortality <- data.frame(
  age_group = c("under65", "65plus", "under65", "65plus"),
  sex = c("female", "female", "male", "male"),
  death_probability = c(0.02, 0.08, 0.04, 0.12)
)
age_groups <- list(age_group = list(
  column = "age", breaks = c(0, 65, Inf),
  labels = c("under65", "65plus")
))
mortality_model <- createMortalityTransitions(
  mortality,
  states = c("Stable", "Complication", "Dead"), deathState = "Dead",
  cycleLength = 7, probabilityDays = 365.25,
  strata = c("age_group", "sex"),
  source = "Synthetic mortality table for this example"
)
mortality_bundle <- createModelBundle(mortality_model, groups = age_groups)
```

There must be one row per covariate combination, or exactly one row when
`strata` is omitted. Labels and codes must match the patient profiles. Nothing
maps sex codes, interpolates ages or substitutes a nearby group. Extra columns
are not used. The model keeps the supplied probabilities, interval length,
conversion and source label in each model's `metadata$mortality`.

The conversion is `1 - (1 - death_probability)^(cycleLength / probabilityDays)`.
It assumes a constant hazard over the supplied probability interval. Both time
lengths are explicit and measured in days. Use probabilities between zero and
one, not percentages, deaths per person-year, or death counts. A life-table
probability covering several years needs that full interval length. An
open-ended final age interval with probability one is not an annual mortality
probability. Supplying one here means certain death in every positive cycle.

With `birth_date` and `index_date` in the patient profiles, the simulator updates
completed age at each cycle boundary. The current age group and other covariates
then select the mortality model for that cycle. A birthday within a cycle does
not split it. A supplied numeric `age` alone does not advance automatically.
Using an open-ended age group, such as `65plus` above, explicitly holds that
group's mortality probability at later ages. No future calendar trend is inferred.

Each living state either stays unchanged or enters the declared death state.
All living states use the same mortality probability within a stratum. This
helper has one death state and does not model complications, recovery or excess
mortality. It is not an adjustment to an existing all-cause mortality estimate.
Adding it as a stage after such an estimate would count mortality again.

You can pass `mortality_model` to `extendModelBundle()` with the same age grouping
rules, provided the states, absorbing state and cycle length match. That means
replacing the entire later transition model with mortality-only transitions,
not adding background mortality while keeping other transitions. Choose this
only when it is the intended scenario. Calibrating excess mortality against an
appropriate reference population remains a separate step.

The runnable [mortality example](../inst/examples/background-mortality-simulation.R)
shows the table, conversion, age-dependent selection and an explicit handover.

### Calibrate excess mortality against the supporting cycles

The patients contributing to a later state or period may differ from those at
entry. `calibrateExcessMortality()` therefore uses the original supporting
observations. With trajectories, it matches background mortality to the source
cycles used by `estimateTransitions()`. With `estimateStageTransitions()` output,
name `stage` to use the observations behind that stage's estimates.

```r
calibration <- calibrateExcessMortality(
  histories,
  background = mortality_bundle,
  deathStates = c("Death, no pneumonia history", "Death, pneumonia history"),
  periodBreaks = c(0, 364, Inf),
  periodLabels = c("First year", "Later follow-up")
)
calibration$estimates
calibration$reference
```

`histories` is a prepared trajectory object with its cycle length and absorbing
states declared. Current C2T output carries these settings. Plain rows need
`cycleLength` supplied explicitly. Declare
death states explicitly. An absorbing state such as discharge is not necessarily
death. The background bundle comes from `createMortalityTransitions()` and may
use different state labels because it supplies background probabilities, not
clinical transitions. Both inputs must use the same cycle length.

A contributing pair has a full source cycle and an immediately adjacent known
destination. Gaps, unresolved pairs, final cycles without a successor and deaths
already present in the first cycle do not create extra outgoing transitions.
The source row determines the state, period and optional `strata`. Background
matching uses that same row's covariates, with age calculated at its start date
when `birth_date` is available. Only contributing source rows need background
coverage. A missing combination is an error, not a request to use a pooled rate.

Each contributing cycle has equal weight. A patient can contribute several
cycles, including cycles in different age groups or periods. `reference` reports
the weights and supporting counts by background group. Its
`background_stratum_id` refers to `calibration$background$strata`, while
`stratum_id` refers to `calibration$strata`. Patient counts across cells are not
additive. No patient identifiers or row-level histories are retained.

Within each state, period and estimation group, the function calibrates a common
daily excess hazard so that the average reconstructed death probability matches
the observed proportion of death transitions. It averages background
**probabilities**, not hazards. With observed probability `p`, mean background
probability `b` and cycle length `c`, the excess is
`(log(1 - b) - log(1 - p)) / c`. The corresponding probability for a cycle with
background probability `q` is `1 - (1 - q) * exp(-excess * c)`.

This is count-matching calibration, not a fitted survival regression or a causal
effect. The additive-hazard framework uses individually matched background
mortality, as described by [Sweeting et al. (2023)](https://pmc.ncbi.nlm.nih.gov/articles/PMC10422853/).
The closed-form calculation here follows from the stated equal-length cycle
model. It does not estimate confidence intervals, adjust for informative
censoring or recover event dates within a cycle.

By default, observed mortality below the matched background is an error. An
explicit `negativeExcess = "zero"` instead floors the excess at zero and warns.
The output retains the raw excess, flags the affected cells and shows how their
reconstructed probabilities differ from the observations. Cells without support
remain unavailable. Cells in which every outgoing transition is death, or a
matched background probability is one, cannot supply a finite calibration and
are rejected.

Reconstruction is exact for the reference cycles in a supported, non-floored
cell. It does not guarantee that a simulation reproduces observed mortality,
because simulated patients can have a different survivor and state composition.
The function returns calibration evidence only. It does not choose a late
reference period, assume excess mortality declines, alter existing transitions
or attach a new simulation model.

The [calibration example](../inst/examples/excess-mortality-calibration.R) shows how
the reference mix changes across periods and checks the transition denominators.

### Extend mortality into future cycles

`extrapolateMortality()` selects one fitted reference period and replaces its
mortality with current background mortality plus calibrated excess. The original
bundle still applies before the chosen switch day.

```r
future <- extrapolateMortality(
  bundle,
  calibration,
  fromDay = 10 * 364,
  referencePeriod = "Years 6-10",
  excess = "linear",
  declineDays = 5 * 365.25,
  source = "Selected-period excess declines to zero over five years"
)
simulation <- simulateTrajectories(
  future, population = baseline_patients, nSteps = 40 * 52,
  terminalStates = calibration$metadata$deathStates, seed = 42
)
```

Choose `excess = "constant"` and omit `declineDays` to retain the selected excess
hazard. Neither choice is inferred from the observed data. `fromDay` must align
with the cycle length. `declineDays` need not, because the calculation integrates
the decline over each cycle, including one that crosses its endpoint.

The clinical reference and calibration must use the same states, cycle length,
estimation groups and selected period. The function checks their death counts
and denominators, not just whether their labels match. It does not select a
different reference period for an unsupported state. Strata are matched by
their covariate values, not by local registry IDs. Fit and calibrate from the
same supporting observations. Aggregate checks cannot establish that two different
datasets contained the same patient-cycles.

At every future source cycle, current covariates select the calibrated excess
and the supplied background probability. Background grouping happens on a
private copy of the profile. It cannot overwrite a clinical or cost group that
uses the same column name with a different definition. Calendar ageing follows
the existing simulator rules. No age interpolation, future calendar trend or
missing-group fallback is introduced.

For background cycle probability `b` and calibrated daily excess `e`, the new
death probability is `1 - (1 - b) * exp(-e * A)`. With constant excess, `A` is
the cycle length. With linear decline over `T` days, `A` is the integral of
`max(1 - t/T, 0)` over the cycle, where `t` is days since the switch. Background
mortality remains fixed at its source-profile value within that cycle. After
the decline ends, excess is zero but background mortality continues to update.

This replaces the old death probability. It does not add a second mortality
stage. For a fitted stage model, it replaces only the named mortality stage and
keeps the other stages in their declared order. The remaining probability within
the replaced row is distributed among non-death destinations
in the same proportions as the reference row. For example, if one quarter of
surviving transitions entered a complication state, that share remains one
quarter. The absolute complication probability changes when survival changes.
This is a scenario assumption about future transitions, not a new estimate of
disease progression or an inferred order of events within the cycle.

One death state needs no routing specification. With multiple death-history
states, provide `deathAllocation` explicitly. A named character vector assigns
each nonabsorbing source state to one death outcome:

```r
deathAllocation = c(
  "Alive, no recorded pneumonia" = "Death, no pneumonia history",
  "Alive, pneumonia history" = "Death, pneumonia history"
)
```

For an adjacent-cycle model, this mapping preserves the history present at the
start of the cycle. For a fitted stage model, it uses the source state reached
at the mortality stage, preserving a complication recorded in an earlier stage
of that same cycle. A numeric matrix with source states on rows and death states on
columns can instead specify conditional death-outcome shares. Each row must
sum to one. The package does not infer either choice. Other declared absorbing
outcomes keep their identity rows and are not treated as deaths.

The extension is a data-only recipe, not a matrix for every possible age and
future cycle. Patient simulation evaluates it when a source row is needed.
`projectCohort()` cannot evaluate this profile-dependent tail and rejects it.
Original costs, entry handling, treatment windows and discount clocks are not
reset at the switch. Treatment effects act on the newly calculated probabilities.

For these simulations, the transition ledger also reports
`background_stratum_id`, `background_death_probability`,
`excess_hazard_per_day`, `integrated_excess_hazard` and `mortality_probability`.
`excess_hazard_per_day` is the calibrated reference hazard before decline.
`integrated_excess_hazard` is `e * A`, the dimensionless excess hazard accumulated
over the cycle after applying any decline, not elapsed follow-up. These values
describe the model
**before** any treatment effect changes its probabilities. They are unavailable
on rows before the switch. Source assumptions remain in the extrapolation
metadata, including after saving, resuming and summarising the run.

The [future-mortality example](../inst/examples/future-mortality-simulation.R)
shows a birthday, a decline ending inside a weekly cycle, and a resumed run.

### Stop, inspect or resume

```r
simulation$population     # Current patient snapshots
simulation$trajectories   # All generated snapshots
simulation$transitions   # Actual sampled transitions and selected source models
simulation$costs         # Patient-level charges by component
simulation$totals
simulation$metadata$stop_reason

simulation <- advanceSimulation(simulation, nSteps = 52)
```

The result retains its rules, one-time payment memory and random-number state.
It can be saved with `saveRDS()` and resumed with `readRDS()` and
`advanceSimulation()`. Resuming does not charge the saved starting label again.
An uninterrupted run and a split run use the same random sequence when callbacks
depend only on their inputs and saved configuration. Random draws in callbacks
use that private sequence too. The caller's random-number state is restored.
Pass `seed` for reproducible initialization. With `seed = NULL`, each new run
starts a fresh private stream, so setting the global seed alone does not fix its
draws. Box-Muller normal generation and user-supplied RNG kinds are unsupported
because their full state cannot be saved and restored. Callbacks must not change
the RNG kind.
Callbacks that depend on external connections or mutable global objects are not
portable checkpoints.

Terminal states must be explicitly declared and absorbing. They stop further
transitions and ongoing charges. A transition rule or an explicitly configured
healthcare model may charge their entry.
Other absorbing states can continue to incur costs. Terminal snapshots remain
in the history while other patients continue. These repeated rows do not mean
additional deaths or additional observed care time.

Every run has a finite `nSteps` limit. It can stop earlier when all patients are
terminal, or through `stopWhen(population, step)`. A custom stop receives the
current population after costs for that label. There is no automatic convergence
criterion. A few unchanged states do not establish equilibrium in an ageing
population. Completed terminal or custom-stopped runs remain stopped on resume.

Simulation records are synthetic draws, not reconstructed patient histories.
Sampling variation is not uncertainty in the estimated probabilities, treatment
effects or model structure. A step needs a supported source stratum and an
estimated probability row. Otherwise it stops rather than silently filling them.
A completed step may leave a patient in a new, unsupported covariate group. That
snapshot has `stratum_id = NA` and is counted in `metadata$unsupported_current`.
It can be the final snapshot or meet a custom stopping rule, but another step
cannot be drawn from it without a supporting model.

### Summarise costs and health outcomes

Summarise a completed or paused simulation without running it again:

```r
outcomes <- summariseSimulation(
  simulation,
  aliveStates = c("Stable", "Complication"),
  utilities = c(Stable = 0.8, Complication = 0.6, Dead = 0),
  costDiscountRate = 0.035,
  healthDiscountRate = 0.035
)

outcomeTotals(outcomes)  # Discounted cohort cost, life-years and QALYs
outcomes$totals       # All totals and means, including undiscounted values
outcomes$byPatient    # Individual totals and represented duration
outcomes$costs        # Totals by cost component
outcomes$stateTime    # Full cycle labels represented in each state
```

The utilities and rates above are examples, not recommended values. Use the
states and evidence appropriate to your study. `aliveStates` is required for
life-years, and `utilities` is additionally required for QALYs. Utilities must
cover every model state, with finite values no greater than one and zero for
states outside `aliveStates`. Negative utilities are allowed for living states.
The package does not infer death from
a state name or treat every absorbing state as death. Explicit terminal states
cannot be included in `aliveStates`, because their paths have stopped.

Health outcomes use the same full-label convention as the simulator's recurring
charges. Each living label contributes `cycleLength / daysPerYear` life-years
and that quantity multiplied by its utility in QALYs. This includes the initial
and final label. A continuously living patient simulated for 52 weekly steps
therefore has 53 labels, representing 371 days. Use 51 steps for 52 weekly
labels. Even `nSteps = 0` represents the initial full label under this convention.
No half-cycle correction or exact within-cycle death time is inferred.

Terminal labels contribute no life-years or QALYs. `stateTime` includes their
represented label occupancy so the supplied history remains visible, but this
is not extra observed care or survival time. A custom-stopped run is not extended
to a longer horizon. The individual duration fields and `metadata$stop_reason`
show what was actually summarised.

Both discount rates default to zero. For each label or charge, the factor is
`(1 + rate)^(-(elapsed_day - discountOriginDay) / daysPerYear)`. The default
`daysPerYear` is 365.25 and `discountOriginDay` is zero, meaning the index date.
The discount clock uses elapsed days, not the cycle counter or calendar year.
If a patient's supplied starting snapshot is already at day 365, its outcomes
are discounted from that day rather than restarting at zero. A different common
origin can be supplied, but it cannot be later than any included label or charge.
Health and costs are discounted at the saved label start. Even daily healthcare
charges split across cost periods use that start, not an inferred payment date.

The summary reports both supplied and discounted amounts. It does not adjust
prices for inflation, change the currency or modify any simulation inputs.
Without a cost model or cost rules, cost totals are `NA`, not zero. A configured
component that never charges anyone is reported as zero. Omitted living-state
or utility definitions likewise leave the corresponding health outcomes `NA`.
Per-patient means include every simulated patient, including those with zero
charges or an initially terminal state. Repeated draws of the same original
patient remain separate simulated patients.

Use the full resumed simulation when summarising a split run. Summaries of
overlapping checkpoints should not be added together. The
[intervention example](../inst/examples/intervention-simulation.R) shows summaries
for two scenarios. Before comparing them, check the starting populations,
represented horizons, stopping rules and economic assumptions. This function
does not calculate an ICER, attach confidence intervals or classify a scenario
as cost-effective.

### Compare two scenarios

Pass the results of `summariseSimulation()` in reference-then-intervention order:

```r
comparison <- compareScenarios(
  reference = summaries$standard_care,
  intervention = summaries$intervention,
  scenarioCovariates = "treatment_active"
)

comparison$outcomes          # Intervention minus reference, totals and means
comparison$costs             # The same differences by cost component
comparison$costEffectiveness # Discounted cost per life-year or QALY gained
comparison$metadata
```

The [intervention example](../inst/examples/intervention-simulation.R) runs this
comparison from start to finish. Differences always mean intervention minus
reference. ICERs use the difference in discounted mean cost divided by the
difference in discounted mean health outcome. Raw and discounted differences
remain available in `outcomes`.

The starting profiles must match, including their joint covariate distribution
and the number of repeated draws. Matching only the number of patients or the
marginal age distribution would not establish this. Row order and generated
simulation or stratum IDs do not matter. Source patient IDs, starting states,
elapsed days and supplied baseline covariates do. The relative cycle counter
does not replace elapsed time. Column classes must also match, except for plain
integer and double vectors. A logical `TRUE` is not treated as a numeric `1`.

`scenarioCovariates` explicitly identifies columns that may differ because of
the scenario, such as treatment assignment. There are no automatic exceptions
for names like `treatment_active`. Patient identity, starting state, elapsed day,
index date and birth date cannot be excluded. The result records the columns
checked and the declared exclusions. Without source IDs, matching profiles
establishes the same recorded starting distribution, not the same individuals.
Neither case establishes paired random draws or causal identification.

Model-generated grouping columns are skipped when their raw inputs are present
in both initial populations. The raw values are still compared. This allows
different group definitions, or groups introduced only after extrapolation,
without treating those model choices as different patients. Directly supplied
baseline groups do not receive this exemption. The comparison reports which
generated columns it skipped.

Summaries retain `initialPopulation` for these checks. This table contains
starting patient identifiers and covariates, so it is not an aggregate-only
output for unrestricted sharing. If an older summary lacks these fields,
summarise its saved simulation again. The trajectories do not need to be rerun.
When an entry model was used, `initialPopulation` contains the pre-entry
profiles, not the sampled first-cycle outcomes. Both scenarios must share this
starting interpretation. Metadata records it as `initial_population_basis`.

States, terminal and living-state definitions, utilities, cycle length, year
length, discount rates, discount origin and accounting conventions must agree. Currency and
price year must also agree when both scenarios contain costs. The comparison
does not convert currencies or reconcile incompatible assumptions. Components
may differ, for example when only the intervention has a treatment charge.
An absent component contributes zero only in a scenario with configured costs.
Unconfigured costs or missing health definitions remain unknown, and the
corresponding ICER is unavailable.

The comparison horizon is the larger of the two actual step counts, using the
existing full-label convention. A shorter run is accepted only when it stopped
because every patient reached a declared terminal state. Under the simulator's
rules, its later costs and health outcomes would then be zero. The comparison
records this zero tail without adding patient rows or rescaling totals. A
shorter run with nonterminal patients is rejected, as are custom `stopWhen`
results. These checks establish a common represented horizon, not equal planned
follow-up. They do not estimate outcomes after an unsupported or truncated run.

The cost-effectiveness table describes each health outcome separately:

| Incremental cost | Incremental health | Report |
| --- | --- | --- |
| Positive | Positive | ICER per unit gained |
| Negative | Positive | `dominant`, with no negative ICER |
| Positive | Negative | `dominated`, with no negative ICER |
| Negative | Negative | `lower_cost_less_effective`, with no ICER per unit gained |

Equal costs and positive health gains give an ICER of zero. Equal health outcomes
have no ICER because the denominator is zero. Status labels distinguish these
cases and identical results. No willingness-to-pay threshold, cost-effectiveness
decision, confidence interval or correction for simulation noise is supplied.

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
For database-backed costing, pass the same visit selection as `costVisitFilter`.
For local `costData`, select the records before calling the wrapper. Neither
approach changes the transition analysis or the trajectory population.
Set `costAllocation = "duration"` to request duration allocation. The default
`costAllocation = NULL` uses start-date extraction from `cdm`, or preserves the
allocation attribute on previously extracted `costData`. Plain local records
default to start-date allocation. `costAllocation` is rejected when `analyses`
does not include `"costs"`.

## Publication

Haug M, Oja M, Pajusalu M, et al. Markov modeling for cost-effectiveness using
federated health data network. Journal of the American Medical Informatics
Association. 2024. [doi:10.1093/jamia/ocae044](https://doi.org/10.1093/jamia/ocae044).

The publication describes the earlier implementation. The package is distributed
under Apache License 2.0. See `DESCRIPTION` and `LICENSE` for the licence.
