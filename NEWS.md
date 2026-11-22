# TrajectoryMarkovAnalysis 1.1.0.9000

* `prepareStageObservations()` aligns two or more C2T trajectory views in
  the specified within-cycle order. It checks matching patient-cycle boundaries
  and absorbing states, and passes definitions and stage order to
  `estimateStageTransitions()`. The connected example now uses separate onset
  and mortality views.

* Result metadata no longer repeats explanatory paragraphs from function help.
  Model definitions, input provenance, units, support counts and settings needed
  to validate, compare and resume analyses are retained.

* `updateRewardModel()` changes named reward assumptions with validation while
  preserving the original. Scenario code no longer needs to edit internal fields.
* `outcomeTotals()` reads compact cost, life-year and QALY totals or per-patient
  values from either outcome summary. Comparison printing now shows scenario
  totals and differences alongside ICERs. Calculations and accounting are unchanged.
* `prepareTrajectories()` reads cycle length, state order and absorbing states
  from current C2T output. Conflicting explicit settings raise an error. Plain
  data frames still accept explicit definitions.
* `estimateStageTransitions(definition = histories)` reuses those definitions
  for analyst-supplied stage observations.
* Results print compact summaries. `transitionMatrix()` retrieves fitted or
  applied probabilities without navigating nested lists.
* The connected `c2t-to-icer.R` example runs from dated intervals through C2T,
  TMA stage fitting, costs and an economic comparison.

# TrajectoryMarkovAnalysis 1.1.0

## Ordered stages and shared outcome accounting

- Cost matching uses separate state and period columns, so labels containing
  spaces cannot select another reward. Stage names `start`, `end` and
  `transition` are reserved to keep interval timing unambiguous.
- `estimateStageTransitions()` fits explicitly supplied within-cycle stage
  observations. It retains observed support, excluded outcomes, first-cycle
  events and estimates by period and covariate group. Clinical event definitions
  and ordering remain analyst inputs.
- `calibrateExcessMortality()` accepts these fitted stage observations when a
  mortality stage is named. `extrapolateMortality()` can replace that stage while
  preserving the other stages. An optional probability ceiling is recorded
  separately from the raw mortality estimate.
- Transition effects can target a named stage in both `projectCohort()` and
  `simulateTrajectories()`. Stage simulations retain their intermediate movements
  in `stageTransitions`, including complication followed by death in one cycle.
  Combined-cycle effects remain available but cannot be mixed with stage effects.
- `createRewardModel()` defines state costs, event costs, upfront costs, utilities
  and discount timing. `summariseOutcomes()` applies those same definitions to
  deterministic projections and sampled trajectories. State costs can be
  supplied as amounts per interval without conversion from daily rates.
- `compareScenarios()` accepts the new interval summaries. Their horizon counts
  completed intervals, so 52 weekly steps contribute 52 weeks. The separate
  `projectCosts()` and `summariseSimulation()` functions retain their existing
  full-label convention, which also accounts for the initial label.

## Public interface and examples

- Corrected the licence declaration to Apache 2.0, matching the existing licence
  file and the maintainer's confirmed choice.

- Replaced blanket exports with an explicit list of supported functions. The
  older GUI, continuous-time and database-driven entry points are no longer
  exported. Removed their unused implementation and bundled database drivers.
  The supported workflow uses R only and no longer requires Rcpp or a compiler.
- Replaced the obsolete example script with a synthetic quickstart. Added an
  ordered-stage example covering intervention effects and economic outcomes.
- Shortened the README and moved the detailed guide to `docs/workflow.md`.
  Function help and the maintained examples describe the current interface.

Transition and cost estimation remain independent, and callers retain ownership
of their CDMConnector connections.
