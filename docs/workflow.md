# Workflow guide

Start with the [README](../README.md) for installation and the
[worked vignette](../vignettes/trajectory-analysis.Rmd) for a complete analysis.
This guide explains how to choose and connect the modelling steps. Function help
contains full argument definitions and output schemas.

The examples below use the objects created by the installed quickstart:

```r
source(system.file("examples", "quickstart.R", package = "TrajectoryMarkovAnalysis"))
```

It creates `histories`, `transitions`, `projection`, `cost_records` and `costs`
from synthetic data. Database examples require your own `cdm` connection.

## Prepare patient histories

`prepareTrajectories()` takes a data frame with `subject_id`, `state_label`,
`state_start_date` and `state_end_date`. Dates are inclusive. Extra columns,
such as age or treatment status, are preserved.

```r
histories <- prepareTrajectories(
  trajectories, cycleLength = 7,
  states = c("Stable", "Complication", "Dead"), absorbingStates = "Dead"
)
```

Cohort2Trajectory output with a `trajectory_definition` attribute supplies
omitted cycle length, state order and absorbing states. Explicit arguments must
agree with that definition. `saveRDS()` preserves it; CSV does not. Include
unobserved states in the dictionary when the model needs them.

`START` and `EXIT` are observation boundaries. `OUT OF COHORT` is an unresolved
interval. None is a clinical state. Without boundary markers, TMA uses the
observed interval bounds and reports that the original observation window could
not be verified. Every patient needs at least one observed interval.

Overlaps and duplicate rows are rejected. With a discrete cycle length, only
the final cycle may be short. Continuous intervals can be prepared without a
cycle length for cost estimation. Preparation inherits C2T state definitions
but does not validate C2T allowed-transition rules.

TMA supports one observation episode per subject. C2T's default
`targetEpisodeHandling = "First"` remains compatible. Its `"All"` output carries
version 2 metadata and episode-specific clocks; TMA rejects it. Attribute-free
rows with multiple `episode_id` values per subject are also rejected to prevent
transitions across episode boundaries. Keep these identifiers when importing CSV.

## Fit adjacent-cycle transitions

```r
transitions <- estimateTransitions(histories)
transitions$estimates
transitionMatrix(transitions)
```

A pair contributes when its source is a complete cycle and the immediately
adjacent destination has a known label. A final partial destination can qualify.
Gaps and unresolved labels never create transitions. A final label without a
successor does not imply that the patient stayed in that state.

The denominator counts eligible outgoing pairs. Unsupported nonabsorbing rows
remain `NA`; declared absorbing states receive structural identity rows. There
is no automatic pooled or neighbouring-group fallback.

Follow-up periods use days since each patient's original index date:

```r
period_fit <- estimateTransitions(
  histories, periodBreaks = c(0, 7, Inf),
  periodLabels = c("First week", "Later follow-up")
)
transitionMatrix(period_fit, period = "First week")
```

Boundaries start at zero, end at `Inf` and align with the cycle length. A pair
belongs to its source period, including when the destination crosses a boundary.
With `strata`, the source row's covariates likewise select the group; crossings
are retained. Numeric covariates create exact-value groups unless you define
bands with `addCovariateGroups()` first. Group IDs are defined by the returned
`strata` registry, and patient counts cannot be added across groups or periods.

`entryCounts` describes known first-cycle labels. A first-cycle death stays in
these counts; it does not create an assumed preceding alive-to-death transition.

## Model ordered events within a cycle

Adjacent labels cannot identify all events within a week. For a study that
specifies onset followed by mortality, supply observations for those stages:

```r
observations <- data.frame(
  subject_id = rep(c("p1", "p2"), each = 2), source_day = 0,
  stage = rep(c("onset", "mortality"), 2),
  from_state = c("Stable", "Complication", "Stable", "Stable"),
  to_state = c("Complication", "Dead", "Stable", "Stable"),
  observed = TRUE
)
stage_fit <- estimateStageTransitions(
  observations, stageOrder = c("onset", "mortality"), definition = histories,
  identityStates = list(onset = "Complication")
)
stage_fit$estimates
stage_fit$coverage
```

The first patient contributes both complication and death. `observed = FALSE`
excludes an unevaluable outcome from its denominator. `identityStates`
explicitly holds a state unchanged in a stage; unsupported rows otherwise stay
unknown. The fit retains observation rows, while a model bundle keeps aggregate
models without the training population.

When C2T supplies aligned trajectory views, `prepareStageObservations()` can
construct these rows. Supply the views in stage order and an explicit
`entryState` for the first cycle. The first stage starts from the preceding
cycle's final outcome; later stages start from the preceding view in the same
cycle. All views must have matching patient-cycle boundaries and compatible
state definitions. A complete final cycle qualifies by default; a short cycle
qualifies when its final view records a declared absorbing outcome. Use
`requireNextCycle = TRUE` to require another cycle for nonabsorbing outcomes.
See the runnable [C2T-to-ICER example](../inst/examples/c2t-to-icer.R).

For supplied probabilities, use `composeTransitionStages()` with a named list
of matrices in the intended order. Every stage uses the same states. One pass
through all stages advances one cycle, and intermediate events add no duration.
The model keeps each stage and its combined matrix. State history needed by a
later stage must remain in the state definitions.

## Estimate first-cycle outcomes separately

Use `estimateEntryTransitions()` when the model needs a transition from a
pre-entry state to the first recorded label:

```r
entry <- estimateEntryTransitions(histories, entryState = "Stable")
entry$estimates
entry$coverage
```

The starting state is an analyst definition. Alternatively, supply one row per
subject with `subject_id` and `entry_state`. For baseline-specific probabilities,
include baseline covariates in that table and name them in `strata`. Values come
from the supplied baseline table, not later trajectory rows.

A known complete first cycle qualifies, as does a short cycle ending in a
declared absorbing state. Short living cycles, missing first cycles and
unresolved labels do not qualify. `subjects` records inclusion and exclusion;
probabilities and the default baseline mix use the eligible subjects. This is
not a correction for censoring. Unsupported entry groups remain unavailable.

Entry is applied only when explicitly passed to a projection or simulation.
It generates the first label at cycle zero and elapsed day zero. It adds no
clock step, baseline living time or recurring baseline charge. The pre-entry
population is stored separately. Entry models do not identify intermediate
events or their timing.

## Project a cohort and inspect occupancy

```r
projection <- projectCohort(
  transitions, nCycles = 4, initial = c(Stable = 1), cohortSize = 100
)
projection$occupancy
projection$flows
transitionMatrix(projection, cycle = 0)
```

Projection returns the initial distribution and one distribution after each
step. Expected counts can be fractional. The matrix is selected by the source
cycle's elapsed day. Its last period continues for the requested horizon; this
is a modelling assumption about follow-up beyond the data.

Without `initial` or `entry`, the starting distribution uses `entryCounts`.
A named `initial` vector supplies a first-label distribution, with omitted
known states assigned zero. It can reproduce the first-label distribution
calculated manually from an entry model.

To apply a fitted entry model instead, supply `entry` and omit `initial`:

```r
projection_from_entry <- projectCohort(
  transitions, nCycles = 3, entry = entry, cohortSize = 100
)
projection_from_entry$entry$baseline_distribution
```

These arguments are mutually exclusive in `projectCohort()`. Entry uses its
eligible baseline mix, then three adjacent steps produce labels 0 through 3.
With `nCycles = 0`, entry alone generates the first label. For stratified entry,
select the relevant `entry$models` element explicitly; projection does not mix
entry groups automatically.

An unsupported row may remain unused while its state has zero probability.
If needed, it causes an error naming the state, period and source cycle.
`flows` distinguishes the conditional transition probability from the mass of
the whole cohort taking that transition. Absorbing self-transitions retain
patients; they are not new deaths. For ordered models, `stageFlows` retains
intermediate movements.

Use `summariseOccupancy(histories)` for observed states, or
`compareOccupancy(histories, projection)` for a descriptive comparison.
Observed summaries retain absorbing outcomes but do not carry living patients
past censoring. Inspect `coverage`: observed proportions use available known
states, whereas a projection keeps a fixed cohort. `cohort_fraction` uses the
original population denominator.

The default comparison uses patients with known first labels, or the exact
eligible patients when projection used an entry model. `population = "all"`
selects all supplied histories explicitly. This does not refit or rescale the
model, and the comparison is not a formal goodness-of-fit test.

## Extract and allocate recorded costs

Cost estimation does not require a transition model. Local records need
`subject_id`, `event_date` and `amount`. Include currency, measure and record
IDs when available:

```r
costs <- estimateStateCosts(histories, cost_records)
costs$estimates
costs$intervalCosts
costs$unallocatedCosts
```

The daily mean divides recorded costs by all observed state-days, including
days with no cost record. No matching record gives zero recorded cost; a missing
amount makes the affected total and mean `NA`. A state without observed days has
no estimate. Currencies are summarised separately, and each analysis accepts
one cost measure.

For a database, pass an existing CDMConnector reference:

```r
cost_events <- extractCosts(
  cdm, histories, measure = "total_charge", domains = c("Visit", "Drug")
)
```

Choose `total_charge`, `total_cost` or `total_paid` deliberately. The extractor
reads without creating scratch tables or closing the connection. Costs link to
events within their domain. For `"Visit Detail"`, visit filters refer to the
parent visit. Check whether visit charges already include itemised costs before
combining domains.

The default `allocation = "start"` assigns the full amount to the event start
date. `allocation = "duration"` spreads it uniformly across its original
inclusive service interval. Local duration records also need `event_end_date`.
The original full duration supplies the denominator: a seven-day charge of 700
with five resolved observed days allocates 500 and leaves 200 unallocated.
Missing or invalid end dates cause an error in duration mode.

Duration extraction includes events starting before follow-up when they overlap
it. Start-date extraction can omit those events, so switching modes requires
extracting again. Allocation attributes are inherited by `estimateStateCosts()`.
Local records without attributes default to start-date allocation.

State and period boundaries split duration shares. `allocatedCosts` and
`unallocatedCosts` retain source-row IDs and allocation fractions. Sum their
`amount` shares to reconcile charges; repeated `original_amount` fields are not
additive. Costs in gaps or unresolved intervals remain unallocated. Periods for
cost-only analyses may split intervals without a discrete cycle length.

### Select visits

`visitFilter` changes which costs are included, while retaining the trajectory
population and observed-day denominator:

```r
visit_costs <- extractCosts(
  cdm, histories, domains = c("Visit", "Drug"),
  visitFilter = list(visitIds = c("123", "456"))
)
```

The IDs are illustrative. Filters also accept `visitConceptIds` or a named
`eventConcepts` list for Condition, Drug, Procedure, Device, Measurement and
Observation. IDs within a vector and event domains match with OR; supplied
filter fields combine with AND. Concepts match exactly, without descendant
expansion. An empty vector selects nothing; omit unused fields.

A qualifying event selects its recorded visit, then costs linked to that visit
in the chosen domains qualify. Dates are not used to invent visit links.
The qualifying record must occur within follow-up, but may select an earlier
cost from that visit: this is retrospective selection. It describes associated
expenditure, not expenditure caused by a condition. Inspect extraction
diagnostics for missing links and unmatched references.

### Adjust prices and assign terminal costs

`adjustCostPrices()` requires a supplied index with `year`, `index` and
`currency_concept_id`, an explicit target `priceYear`, a `sourceYear` column and
a source description. It applies `nominal_amount * target_index / source_index`
before allocation and preserves nominal amounts and provenance. All years and
currencies must be covered; repeated adjustment is rejected. Currency conversion
and outcome discounting are separate operations. See the runnable
[price-adjustment example](../inst/examples/price-adjustment.R).

A `terminalCostMap`, such as `c(Dead = "Stable")`, moves both terminal-cycle
costs and observed days to a declared nonabsorbing cost state. It changes cost grouping,
not clinical histories or transition counts. A three-day terminal interval
still contributes three days. Without a mapping, it keeps its own cost state.

## Bundle models and simulate patients

`createModelBundle()` stores fitted or supplied models and grouping rules. It
does not estimate new probabilities or retain a training population. A raw
`estimateStateCosts()` result is descriptive; attach a `createCostModel()`
result to activate healthcare charges.

```r
bundle <- createModelBundle(transitions)
simulation <- simulateTrajectories(
  bundle, n = 100, initial = c(Stable = 1), nSteps = 4,
  terminalStates = "Dead", seed = 42
)
simulation$population
simulation$trajectories
simulation$transitions
```

For stratified models, supply a `population` with one current snapshot per
patient: `state_label` and the required covariates, with optional unique
`subject_id`. Omit `n` to use every row once; `n` and `replace = TRUE` sample
profiles with replacement. New `simulation_id` values distinguish repeated
draws, while `source_subject_id` retains their origin. A named `initial` vector
can override sampled states. A joint `initial` table with `state_label`,
covariates and `probability` instead supplies complete weighted profiles.

`elapsed_day` and `cycle` default to zero. Explicit days must align with the
cycle length. Each step selects the source model and period, draws the state,
advances time, updates nonterminal covariates and regroups for the next draw.
Missing support is an error when a draw needs it. A final snapshot can enter an
unsupported group, but another draw from it cannot proceed.

Bundle `groups` use the same numeric rules as `addCovariateGroups()`. With
`birth_date` and `index_date` as Date columns, age is completed calendar years at
`index_date + elapsed_day`. Numeric age alone stays fixed unless
`updateCovariates` changes it. The callback receives `current` and `previous`
nonterminal snapshots after the clock advance and returns `simulation_id` plus
changed raw covariates. IDs, states, clocks and derived grouping fields are
protected. Updated values affect the next transition.

Passing `entry = entry` samples first-cycle outcomes once. Supply the baseline
profiles or an explicit starting distribution yourself; simulation does not
adopt the entry estimator's population. With entry, all starting clocks must be
zero. Stratified entry uses baseline covariates once; later grouping remains
dynamic. Entry does not call `updateCovariates` and is not repeated on resume.
See the [entry](../inst/examples/entry-simulation.R) and
[stratified-entry](../inst/examples/stratified-entry-simulation.R) examples.

`advanceSimulation(simulation, nSteps = 4)` continues a saved run without repeating
the last label's charges. `saveRDS()` preserves rules, payment memory and the
private random stream. Supply `seed` for reproducible initialization; a global
seed alone does not fix a new stream. Split and uninterrupted runs agree when
callbacks depend on their inputs and saved configuration.

Every run has a finite step limit. It may stop sooner when all patients reach
explicit absorbing `terminalStates`, or through `stopWhen`. Terminal and
custom-stopped runs remain stopped on resume. Terminal labels can remain in
the history while other patients continue, without further draws or ongoing
charges for those patients.

## Supply intervention effects

A transition effect multiplies one destination probability and transfers the
difference to `redistributeTo`:

```r
effects <- list(prevention = list(
  from = "Stable", to = "Complication", multiplier = 0.75,
  redistributeTo = "Stable", startDay = 0, endDay = 28
))
treated_projection <- projectCohort(
  transitions, nCycles = 4, initial = c(Stable = 1), cohortSize = 100,
  transitionEffects = effects
)
```

A row with 70% stable, 20% complication and 10% death becomes 75%, 15% and 10%.
This is a relative probability change, not a hazard ratio. Other destinations
are unchanged, and the result must remain a valid row.

Simulation also accepts `eligible = function(p) ...` using source profiles.
Time windows include `startDay` and exclude `endDay`. Updates after a transition
affect eligibility for the next draw. Projection has no patient-level callback.

Use `stage = "onset"` for a named-stage effect; otherwise the effect changes the
combined matrix. Stage and combined effects cannot be mixed. Combined effects
retain net outcomes only because they do not define intermediate paths. Rules
cannot overlap on the same applicable source within a stage.

The default `applyAt = "transitions"` leaves entry unchanged. `"entry"` and
`"both"` explicitly include it. Entry and the first adjacent draw both use
source day zero: with weekly cycles, `applyAt = "both", endDay = 28` covers entry
plus four adjacent draws. To cover four outcome labels, use an entry rule plus
an adjacent rule ending at day 21. Effects and fees are configured independently.
See the [intervention example](../inst/examples/intervention-simulation.R).

## Choose an accounting convention

### Completed intervals

Use `createRewardModel()` and `summariseOutcomes()` for a horizon measured in
completed intervals. The same rewards work with projections and simulations:

```r
rewards <- createRewardModel(
  stateCosts = c(Stable = 100, Complication = 200, Dead = 0),
  aliveStates = c("Stable", "Complication"),
  utilities = c(Stable = 0.8, Complication = 0.6, Dead = 0),
  currency = "EUR", priceYear = 2026,
  costDiscountRate = 0.035, healthDiscountRate = 0.035
)
reference_outcomes <- summariseOutcomes(projection, rewards)
intervention_rewards <- updateRewardModel(rewards, upfrontCosts = c(programme = 50))
intervention_outcomes <- summariseOutcomes(treated_projection, intervention_rewards)
outcomeTotals(intervention_outcomes)
outcomeTotals(intervention_outcomes, perPatient = TRUE)
```

Four steps represent four intervals. The final endpoint adds no recurring cost
or living time. State costs are amounts per interval. Choose `costTiming` and
`healthTiming` independently as `"start"`, `"end"` or a named stage; stage timing
uses the state immediately after that stage. Discount timing is separate.
`transitionCosts` can charge net or named-stage events, and `upfrontCosts` apply
once per initial patient.

This path rejects fitted entry models and simulations with a cost model or
cost rules. For an interval-based onset-then-mortality model, use explicit
starting states and stage observations as in the
[ordered-stage example](../inst/examples/ordered-stage-economics.R). The
`entryState` boundary definition in `prepareStageObservations()` is compatible;
it is distinct from passing a fitted `entry` model.

`updateRewardModel()` validates a new definition without changing the original.
Each supplied argument replaces the entire field, including all named cost
components. `outcomeTotals()` reads stored cost, life-year and QALY totals;
`discounted = FALSE` selects undiscounted results. Uncomputed outcomes stay `NA`.

### Full cycle labels

`projectCosts()` applies stipulated state and net-transition costs to a
projection. State charges cover every label, including the initial label.
`entryCost` requires an explicit entry model and adds one fee per projected
patient. It performs no discounting or health-reward calculation.

For simulation charges, `createCostModel()` converts estimated daily means into
an explicit healthcare model. Supply `unit`, `currency`, `priceYear` and
`terminal`. `unit = "day"` applies rates over the full labelled cycle, splitting
cost periods if needed. `unit = "cycle"` multiplies the daily mean by the
specified cycle length, which must match simulation. A `terminal = "entry"`
policy charges one terminal-entry label; `"none"` omits terminal charges.
Initially terminal patients and later terminal residency are not charged.
See the [estimated-cost example](../inst/examples/estimated-cost-simulation.R).

Manual `costRules` can add `once`, `recurring`, `transition` or pre-entry `entry`
fees. Keep components separate and avoid duplicating care already in fitted
rates. A pre-entry fee uses baseline eligibility and remains paid if the first
outcome is death. It adds no extra label or living time.

`summariseSimulation()` reads the saved charges and calculates life-years and
QALYs when `aliveStates` and `utilities` are supplied. Every living label
represents a full cycle, including the initial and final labels. A continuously
living patient with 52 weekly steps therefore represents 53 weeks (371 days);
51 steps represent 52 labels. Terminal labels contribute no life-years or QALYs.
Discounting uses each label's elapsed day from `discountOriginDay` (default zero).
Missing cost or health definitions leave the respective totals `NA`.

Neither accounting path infers exact death times or a half-cycle correction.
Currency and price year describe supplied amounts. Use explicit price adjustment
before estimation when needed; discounting does not adjust historical prices.

## Compare scenarios

```r
comparison <- compareScenarios(reference_outcomes, intervention_outcomes)
comparison$outcomes
comparison$costs
comparison$costEffectiveness
```

Differences are intervention minus reference. Both inputs must use the same
accounting convention, population basis, clinical states, horizon and economic
definitions. For patient simulations, identities and baseline covariates must
match after explicitly excluding treatment-assignment columns through
`scenarioCovariates`. Starting pre-entry profiles cannot be mixed with already
labelled profiles. Labelled summaries accept a zero terminal tail only when the
shorter run stopped because everyone was terminal; a custom stop does not
qualify. Full input checks are in `?compareScenarios`.

ICERs describe cost per life-year or QALY gained when that denominator is
positive. Higher health at lower cost is `dominant`; lower health at higher
cost is `dominated`. Lower cost with lower health is reported separately.
No willingness-to-pay threshold or uncertainty analysis is inferred.

## Extend follow-up with supplied models

`extendModelBundle()` attaches one explicit later model at `fromDay`, with a
source description. It must share states, absorbing declarations and cycle
length, but can use different groups and periods. Selection uses the source
day: a weekly step from 357 to 364 uses the original model at a day-364 handover;
the step from 364 to 371 uses the later one.

The later model's period clock starts at the handover. Patient age, treatment
windows, cost periods and discounting retain their original clocks. Entry and
cost models are not replaced. Missing late support causes an error rather than
a return to the original estimates. This handover works in simulation and
saved/resumed runs, not `projectCohort()`. See the
[extrapolation example](../inst/examples/extrapolation-simulation.R).

### Background and excess mortality

`createMortalityTransitions()` takes a supplied table of death probabilities,
a probability interval in days, a death state, cycle length and source label.
The conversion is `1 - (1 - p)^(cycleLength / probabilityDays)`, assuming a
constant hazard over that interval. Supply probabilities, not percentages or
rates. All living states share the stratum's mortality and otherwise remain
unchanged. This is a mortality-only model, not an automatic addition to existing
all-cause mortality. See the
[background-mortality example](../inst/examples/background-mortality-simulation.R).

`calibrateExcessMortality()` matches background probabilities to the contributing
source cycles from histories, or to a named stage's fitted observations.
Death states are explicit. With observed probability `p`, mean matched background
`b` and cycle length `c`, daily excess is `(log(1 - b) - log(1 - p)) / c`.
It averages background probabilities over the actual supporting observations.

Unsupported cells stay unavailable. Negative excess is rejected unless
`negativeExcess = "zero"` explicitly floors it. Probability one has no finite
excess hazard; an explicit `probabilityCeiling` can cap the observed probability
used for calibration while retaining the raw estimate and reconstruction
difference. This is count-matching calibration, not survival regression or a
causal effect. See the
[calibration example](../inst/examples/excess-mortality-calibration.R).

`extrapolateMortality()` uses a selected fitted reference period, a handover day,
current background probabilities and constant or linearly declining excess.
It replaces mortality rather than adding it again. In an ordered model, only
the mortality stage is replaced. Non-death destinations keep their conditional
shares from the reference row; multiple death states require explicit
`deathAllocation`. The reference and calibration must have matching support.

Current patient covariates select future background and excess. There is no
age interpolation, future trend or missing-group fallback. Original clocks and
costs continue. This profile-dependent tail is supported by simulation, not
cohort projection. See the
[future-mortality example](../inst/examples/future-mortality-simulation.R).

## Combined estimation and output handling

`TrajectoryMarkovAnalysis()` is a convenience wrapper. Its default analysis fits
transitions. For costs, add `analyses = c("transitions", "costs")` and supply
`costData` or `cdm`, not both. `costVisitFilter` selects database visits;
`costAllocation` chooses start or duration allocation. The result contains
prepared trajectories and requested estimates, without a stored connection.

Printing gives a short summary; tables and settings remain on the object.
Inspect counts, coverage and allocation diagnostics before modelling. Simulated
paths reflect fixed supplied inputs; sampling variation does not represent
uncertainty in treatment effects or model structure. Patient-level histories,
entry subjects and baseline comparison tables require appropriate handling
before sharing.
