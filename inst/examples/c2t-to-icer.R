# C2T -> TMA: dated events, weekly histories, fitted stages and an ICER.
# Run with installed Cohort2Trajectory and TrajectoryMarkovAnalysis packages:
# source(system.file("examples", "c2t-to-icer.R", package = "TrajectoryMarkovAnalysis"))
# No database is needed. Five synthetic patients illustrate the interfaces;
# costs, utilities and intervention effects below are demonstration assumptions.
# The local function keeps example objects out of the calling environment and
# removes C2T's temporary study files, including if the example fails.

(function() {
  if (!requireNamespace("Cohort2Trajectory", quietly = TRUE) ||
      utils::packageVersion("Cohort2Trajectory") < "1.3.0.9000") {
    stop("This example requires Cohort2Trajectory >= 1.3.0.9000 for the definition handoff. Update it before sourcing c2t-to-icer.R.",
         call. = FALSE)
  }
  if (!requireNamespace("TrajectoryMarkovAnalysis", quietly = TRUE)) {
    stop("Install TrajectoryMarkovAnalysis before sourcing c2t-to-icer.R.", call. = FALSE)
  }

  if (!"prepareStageObservations" %in%
      getNamespaceExports("TrajectoryMarkovAnalysis")) {
    stop("Update TrajectoryMarkovAnalysis and restart R to use prepareStageObservations().",
         call. = FALSE)
  }

  # 1. Supply dated target and state intervals. Dates include both endpoints.
  # "0" marks observation support. 001 has no pneumonia, 002 develops pneumonia,
  # 003 develops pneumonia then dies, 004 dies without pneumonia, and 005 is censored.
  states <- c(
    "Alive, no recorded pneumonia",
    "Alive, pneumonia history",
    "Death, no pneumonia history",
    "Death, pneumonia history"
  )
  alive_states <- states[1:2]
  death_states <- states[3:4]
  cycle_length <- 7

  # Example helper: make one input interval; this is not a package function.
  example_interval <- function(id, state, start, end) {
    data.frame(
      subject_id = id,
      cohort_definition_id = state,
      cohort_start_date = as.Date(start),
      cohort_end_date = as.Date(end)
    )
  }
  input <- do.call(rbind, list(
    example_interval("001", "0", "2020-01-01", "2020-01-21"),
    example_interval("001", states[1], "2020-01-01", "2020-01-21"),
    example_interval("002", "0", "2020-01-01", "2020-01-21"),
    example_interval("002", states[1], "2020-01-01", "2020-01-08"),
    example_interval("002", states[2], "2020-01-09", "2020-01-15"),
    example_interval("002", states[2], "2020-01-12", "2020-01-21"),
    example_interval("003", "0", "2020-01-01", "2020-01-12"),
    example_interval("003", states[1], "2020-01-01", "2020-01-09"),
    example_interval("003", states[2], "2020-01-10", "2020-01-11"),
    example_interval("003", states[4], "2020-01-12", "2020-01-12"),
    example_interval("004", "0", "2020-01-01", "2020-01-04"),
    example_interval("004", states[1], "2020-01-01", "2020-01-03"),
    example_interval("004", states[3], "2020-01-04", "2020-01-04"),
    example_interval("005", "0", "2020-01-01", "2020-01-10"),
    example_interval("005", states[1], "2020-01-01", "2020-01-10")
  ))

  # 2. C2T resolves overlapping events into one label per weekly interval.
  # Death has the highest priority and ends follow-up. History is persistent.
  study_path <- tempfile("c2t-to-icer-")
  dir.create(study_path)
  on.exit(unlink(study_path, recursive = TRUE), add = TRUE)
  allowed <- stats::setNames(list(
    states, states[c(2, 4)], character(0), character(0)
  ), states)
  study <- Cohort2Trajectory::cohort2TrajectoryConfiguration(
    studyName = "WeeklyExample", pathToStudy = study_path,
    useCDM = FALSE, trajectoryDataObject = input,
    stateCohortLabels = states,
    stateCohortPriorityOrder = states[c(4, 3, 2, 1)],
    stateCohortAbsorbing = death_states,
    allowedStatesList = allowed,
    stateSelectionType = "Priority", trajectoryType = "Discrete",
    cycleLength = cycle_length
  )
  Cohort2Trajectory::getDataForStudy(studyEnv = study)
  trajectories <- Cohort2Trajectory::createTrajectories(studyEnv = study)
  clinical <- subset(trajectories, !state_label %in% c("START", "EXIT"))
  print(clinical[c("subject_id", "state_label", "state_start_date", "state_end_date")],
        row.names = FALSE)

  # 003 has a death-with-history label for January 8-12: weekly labels alone
  # cannot recover its earlier pneumonia event. 004 dies in its first interval.
  # 005 has a partial January 8-10 interval followed by EXIT on January 11.
  # Pass the complete C2T output, including START and EXIT, to TMA.
  # The C2T output carries cycle length, state order and absorbing states.
  histories <- TrajectoryMarkovAnalysis::prepareTrajectories(trajectories)
  print(histories)

  # 3. Adjacent-label estimates describe a different question from within-week
  # stages: they count observed outgoing pairs and omit first-interval deaths.
  # They are useful for inspection, but do not drive the stage model below.
  adjacent <- TrajectoryMarkovAnalysis::estimateTransitions(histories)
  print(subset(adjacent$estimates, count > 0,
               select = c(from_state, to_state, count, denominator, probability)),
        row.names = FALSE)

  # 4. Build a second C2T view of the same weeks, retaining pneumonia history.
  # The final view above keeps death as the weekly outcome. The history view
  # omits death labels so they cannot hide pneumonia recorded earlier that week.
  # Both views use the same patients, index dates, weekly grid and follow-up end.
  # C2T selects each view's labels; TMA does not infer clinical events from them.
  final_view <- trajectories
  history_input <- subset(input, !cohort_definition_id %in% death_states)
  final_end <- stats::aggregate(state_end_date ~ subject_id, data = clinical, FUN = max)
  target_rows <- history_input$cohort_definition_id == "0"
  history_input$cohort_end_date[target_rows] <- final_end$state_end_date[
    match(history_input$subject_id[target_rows], final_end$subject_id)
  ]
  history_study <- Cohort2Trajectory::cohort2TrajectoryConfiguration(
    studyName = "WeeklyHistoryExample", pathToStudy = study_path,
    useCDM = FALSE, trajectoryDataObject = history_input,
    stateCohortLabels = alive_states,
    stateCohortPriorityOrder = rev(alive_states),
    stateCohortAbsorbing = character(0),
    allowedStatesList = stats::setNames(list(alive_states, states[2]), alive_states),
    stateSelectionType = "Priority", trajectoryType = "Discrete",
    cycleLength = cycle_length
  )
  Cohort2Trajectory::getDataForStudy(studyEnv = history_study)
  history_view <- Cohort2Trajectory::createTrajectories(studyEnv = history_study)

  # Compare 003's history before death and 005's incomplete censored week.
  view_columns <- c("subject_id", "state_label", "state_start_date", "state_end_date")
  print(subset(history_view, subject_id %in% c("003", "005") &
                 !state_label %in% c("START", "EXIT"), select = view_columns),
        row.names = FALSE)
  print(subset(final_view, subject_id %in% c("003", "005") &
                 !state_label %in% c("START", "EXIT"), select = view_columns),
        row.names = FALSE)

  # Each weekly cycle contains onset followed by mortality. Everyone enters
  # without pneumonia; subsequent cycles start at the preceding final state.
  # A complete last week counts without a later week. Death closes its week;
  # an incomplete censored week is unobserved for both stages and is excluded.
  stage_observations <- TrajectoryMarkovAnalysis::prepareStageObservations(
    trajectories = list(onset = history_view, mortality = final_view),
    entryState = states[1], requireNextCycle = FALSE
  )
  print(subset(stage_observations, subject_id %in% c("003", "005")), row.names = FALSE)
  fitted_stages <- TrajectoryMarkovAnalysis::estimateStageTransitions(
    stage_observations, identityStates = list(onset = states[2])
  )
  print(subset(fitted_stages$estimates, !structural & count > 0,
               select = c(stage, from_state, to_state, count, denominator, probability)),
        row.names = FALSE)
  # Fitted onset risk is 2/9; mortality is 1/7 without history and 1/3 with history.
  # The sample is tiny. Projection below assumes these weekly risks persist.

  # 5. Estimate daily costs from invented charges allocated to the C2T intervals.
  # Death-labelled terminal intervals retain the rate of their history group.
  # The mapping moves BOTH their costs and their observed days to living states.
  daily_charge <- stats::setNames(c(10, 30, 10, 30), states)
  observed_days <- as.integer(clinical$state_end_date - clinical$state_start_date) + 1L
  cost_records <- data.frame(
    subject_id = clinical$subject_id,
    event_date = clinical$state_start_date,
    amount = unname(daily_charge[clinical$state_label]) * observed_days,
    currency_concept_id = "44818568", measure = "total_charge"
  )
  fitted_costs <- TrajectoryMarkovAnalysis::estimateStateCosts(
    histories, cost_records,
    terminalCostMap = stats::setNames(alive_states, death_states)
  )
  print(fitted_costs$estimates[c("cost_state", "observed_days", "total_cost",
                               "mean_cost_per_day")], row.names = FALSE)
  daily_rates <- stats::setNames(fitted_costs$estimates$mean_cost_per_day,
                                fitted_costs$estimates$cost_state)
  weekly_costs <- stats::setNames(
    c(unname(daily_rates[alive_states]) * cycle_length, 0, 0), states
  )

  # 6. Reduce onset probability by 25% during the first 28 days (four intervals).
  # Removed probability stays without pneumonia; subsequent mortality depends
  # on the state reached after onset. There is no separate mortality multiplier.
  prevention <- list(prevention = list(
    stage = "onset", from = states[1], to = states[2], multiplier = 0.75,
    redistributeTo = states[1], endDay = 28
  ))
  initial <- stats::setNames(1, states[1])
  n_cycles <- 8
  cohort_size <- 100
  reference <- TrajectoryMarkovAnalysis::projectCohort(
    fitted_stages, nCycles = n_cycles, initial = initial, cohortSize = cohort_size
  )
  intervention <- TrajectoryMarkovAnalysis::projectCohort(
    fitted_stages, nCycles = n_cycles, initial = initial, cohortSize = cohort_size,
    transitionEffects = prevention
  )

  # 7. Use one completed-interval accounting convention throughout economics.
  # Eight steps mean eight weekly intervals (56 days), with nine boundary snapshots.
  # Costs use the post-onset state: even someone dying later that interval incurs
  # that week's cost. Living time uses the starting state. Stages add no duration.
  # Both living utilities are a toy 0.68; deaths have zero utility and future cost.
  # The program costs EUR 1,500 once per starting patient. No discounting is used.
  reference_rewards <- TrajectoryMarkovAnalysis::createRewardModel(
    stateCosts = weekly_costs, upfrontCosts = c(program = 0),
    aliveStates = alive_states,
    utilities = stats::setNames(c(0.68, 0.68, 0, 0), states),
    costTiming = "onset", healthTiming = "start",
    costDiscountRate = 0, healthDiscountRate = 0,
    currency = "EUR", priceYear = 2026
  )
  intervention_rewards <- TrajectoryMarkovAnalysis::updateRewardModel(
    reference_rewards, upfrontCosts = c(program = 1500)
  )
  reference_outcomes <- TrajectoryMarkovAnalysis::summariseOutcomes(
    reference, reference_rewards
  )
  intervention_outcomes <- TrajectoryMarkovAnalysis::summariseOutcomes(
    intervention, intervention_rewards
  )
  comparison <- TrajectoryMarkovAnalysis::compareScenarios(
    reference_outcomes, intervention_outcomes
  )
  # Compact discounted totals; use perPatient = TRUE for means instead.
  print(TrajectoryMarkovAnalysis::outcomeTotals(reference_outcomes))
  print(comparison)
  # With these assumptions: incremental cost is about EUR 1,480.81 per patient,
  # incremental QALYs 0.002307, and the ICER about EUR 641,940 per QALY.

  # 8. Sample individual paths from the same learned stage model and intervention.
  # Keep the simulation uncosted: summariseOutcomes applies the SAME rewards to
  # completed intervals. nCycles also fixes the horizon if all patients die early.
  # Sampling variation is expected; this is not a parameter-uncertainty analysis.
  sampled <- TrajectoryMarkovAnalysis::simulateTrajectories(
    TrajectoryMarkovAnalysis::createModelBundle(fitted_stages),
    n = cohort_size, initial = initial, nSteps = n_cycles,
    terminalStates = death_states, transitionEffects = prevention, seed = 192
  )
  sampled_outcomes <- TrajectoryMarkovAnalysis::summariseOutcomes(
    sampled, intervention_rewards, nCycles = n_cycles
  )
  print(utils::head(sampled$stageTransitions), row.names = FALSE)
  print(sampled_outcomes)

  # source(... )$value retains these results if you want to inspect them further.
  invisible(list(
    trajectories = trajectories, history_view = history_view, final_view = final_view,
    histories = histories, adjacent = adjacent,
    stage_observations = stage_observations, fitted_stages = fitted_stages,
    fitted_costs = fitted_costs, reference = reference, intervention = intervention,
    reference_outcomes = reference_outcomes, intervention_outcomes = intervention_outcomes,
    comparison = comparison, sampled = sampled, sampled_outcomes = sampled_outcomes
  ))
})()
