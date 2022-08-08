################################################################################
#
# Some functions for visualizing the outputs of the study
#
################################################################################



#' Visualize patient's trajectory inbetween the cohorts
#'
#' This function outputs a patient oriented plot
#'
#' @param patientData A Data.frame object which is resulting from
#' @param patientId Patient's id
#' @param trajectoryStopDays Nr of days which will determine the possible split between trajectories
#' @param theme Plot's theme indicator
#' @param connection Active connection
#' @param dbms Database management system's dialect
#' @param cdmTmpSchema Temp tables' schema
#' @keywords internal
visualisePatient <- function(patientData,
                            patientId,
                            trajectoryStopDays = 183,
                            theme = 1,
                            connection,
                            dbms,
                            cdmTmpSchema = "ohdsi_temp",
                            feature = 0) {
  newPatientData <-  dplyr::filter(patientData, SUBJECT_ID == patientId)
  newPatientData <-  dplyr::arrange(newPatientData, STATE_START_DATE, STATE_END_DATE)
  momentStart <- as.numeric(
    as.Date(newPatientData$STATE_START_DATE) - as.Date(newPatientData$STATE_START_DATE[1])
  )
  momentEnd <- as.numeric(
    as.Date(newPatientData$STATE_END_DATE) - as.Date(newPatientData$STATE_START_DATE[1])
  )
  momentDuration <- as.numeric(
    as.Date(newPatientData$STATE_END_DATE) - as.Date(newPatientData$STATE_START_DATE)
  )
  # Calculate the trajectories between for the given patient
  n <- length(momentStart)
  momentInclusion <- rep(TRUE, n)
  if (theme == 1) {
    # Will create an error if only one data row present
    if (n == 1) {
      momentDifBoolean <- c(TRUE)
    }
    else {
      momentDif <- momentEnd[-length(momentEnd)] - momentStart[-1]
      # Will return TRUE if momentDif element is smaller than trajectoryStopDays
      momentDifBoolean <- -momentDif > trajectoryStopDays
    }
    groups <- rep(1, n)
    
    for (index in 1:length(momentDifBoolean)) {
      if (!momentDifBoolean[index]) {
        groups[index + 1] <- groups[index]
      }
      else {
        groups[index + 1] <- groups[index] + 1
        # Let's crop the trajectories' durations so that every trajectory start's from 0 not some x value
        momentStart[c(index + 1:n)] <- momentStart[c(index + 1:n)] - momentStart[index + 1]
        momentEnd[c(index + 1:n)] <- momentStart[c(index + 1:n)] + momentDuration[c(index +
                                                                                     1:n)]
        
      }
    }
  }
  else if (theme == 2) {
    groups <- rep(1, n)
    group <- 1 # variable for iterating through groups of trajectories
    # Special case: only one data row
    if (n == 1) {
      momentInclusion <- c(TRUE)
    }
    else {
      countOther <- 0
      for (index in  1:n) {
        if (as.character(newPatientData$STATE[index]) == "0" |
            as.character(newPatientData$STATE[index]) == "OUT OF COHORTS")
        {
          countOther <- countOther + momentDuration[index]
          if (countOther > trajectoryStopDays) {
            momentInclusion[index] <- FALSE
            if (!(as.character(newPatientData$STATE[index + 1]) %in% c("0", "OUT OF COHORTS"))) {
              group <- group + 1
              
              # Let's crop the trajectories' durations so that every trajectory start's from 0 not some x value
              momentStart[c(index + 1:n)] <- momentStart[c(index + 1:n)] - momentStart[index + 1]
              momentEnd[c(index + 1:n)] <- momentStart[c(index + 1:n)] + momentDuration[c(index +
                                                                                           1:n)]
              
            }
          }
        }
        else {
          countOther <- 0
        }
        groups[index] <- group
      }
    }
  }
  
  
  # Some Na's may occur
  momentStart <- as.vector(na.exclude(momentStart))
  momentEnd <- as.vector(na.exclude(momentEnd))
  
  # Color for visualisation
  n_states <- length(unique(newPatientData$STATE))
  colors <- NULL
  # Due to RColorBrewer limitations we repeat if > 12 classes
  if (n_states > 12) {
    n <- 12
    colors_vec <- colors <- RColorBrewer::brewer.pal(n = n, name = 'Paired')
    while (n < n_states) {
      colors_vec <- c(colors_vec, colors_vec[n %% 12])
      n <- n + 1
    }
    colors <- colors_vec
  }
  else {
    colors <- RColorBrewer::brewer.pal(n = n_states, name = 'Paired')
  }
  colorTable <- cbind(unique(as.vector(newPatientData$STATE)), colors)
  colnames(colorTable) <- c("STATE", "COLOR")
  
  ##############################################################################
  #
  # Let's add information from cost table under the given date interval
  #
  ##############################################################################
  # DEV value
  # cost = rep(200, n)
  cost <- rep(NA, n)
  #
  for (iteration in 1:n) {

    tmp_cost <- getCost(
      connection = conn,
      dbms = dbms,
      patientId = patientId,
      startDate = as.Date(newPatientData$STATE_START_DATE[iteration]),
      endDate = as.Date(newPatientData$STATE_END_DATE[iteration]),
      cdmTmpSchema = cdmTmpSchema
    )
    cost[iteration] <- ifelse(is.na(as.numeric(tmp_cost[1])), 0, as.numeric(tmp_cost[1]))
  }
  newPatientData <- cbind(newPatientData[,1:4],
                         momentStart,
                         momentEnd,
                         groups,
                         momentInclusion,
                         cost)
  colnames(newPatientData) <- c(
    "SUBJECT_ID",
    "STATE",
    "STATE_START_DATE",
    "STATE_END_DATE",
    "MOMENT_START",
    "MOMENT_END",
    "GROUP",
    "INCLUDE",
    "COST"
  )
  newPatientData <- dplyr::filter(newPatientData, INCLUDE == TRUE)
  newPatientData <- merge(x = newPatientData,
                         y = colorTable,
                         by = "STATE",
                         all.x = TRUE)
  newPatientData <-  dplyr::arrange(newPatientData, STATE_START_DATE, STATE_END_DATE)
  newPatientData <-  dplyr::mutate(newPatientData,
                                  MOMENT_START = ifelse(MOMENT_START == 0 &
                                                          STATE != 'START', 1,
                                                        MOMENT_START))
  blank_data <- data.frame(GROUP = sort(unique(groups)),
                          y = 1,
                          x = as.vector(
                            tapply(newPatientData$MOMENT_END, newPatientData$GROUP, max)
                          ))
  p <- ggplot2::ggplot(newPatientData) +
    # geom_line(aes(x = seq(0, MOMENT_END[nrow(newPatientData)], length.out =  4), y = rep(0, 4))) +
    ggplot2::geom_rect(
      xmin = newPatientData$MOMENT_START,
      xmax = newPatientData$MOMENT_END,
      ymin = rep(-Inf, nrow(newPatientData)),
      ymax = rep(Inf, nrow(newPatientData)),
      fill = newPatientData$COLOR,
      alpha = (0.3)
    ) +  ggplot2::geom_blank(data = blank_data, ggplot2::aes(x = x, y = y)) + ggplot2::facet_wrap(~ GROUP, scales = "free_x")
  # "h" a variable for text height
  h <- rep(1, nrow(newPatientData))
  if (feature == 1) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x = MOMENT_START + (MOMENT_END - MOMENT_START) / 2, y = COST), size = 1)
    h <- rep(max(newPatientData$COST) / 2, nrow(newPatientData))
  }
  else if (feature == 2) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x = MOMENT_START + (MOMENT_END - MOMENT_START) / 2, y = COST), size = 1)
    h <- rep(max(newPatientData$COST) / 2, nrow(newPatientData))
  }
  else if (feature == 3) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x = MOMENT_START + (MOMENT_END - MOMENT_START) / 2, y = COST), size = 1)
    h <- rep(max(newPatientData$COST) / 2, nrow(newPatientData))
  }
  
  p <- p + ggplot2::geom_text(ggplot2::aes(
    x = MOMENT_START + (MOMENT_END - MOMENT_START) / 2,
    y = h,
    #rnorm(nrow(newPatientData)) + rnorm(nrow(newPatientData)) - rnorm(nrow(newPatientData)) / 2,
    label = STATE,
    #    vjust = -1.5,
    angle  = 90
  ),
  size = 4) +  # ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::xlab("Days") + ggplot2::ylab("Measurements") + ggplot2::expand_limits(x = 0) +  ggplot2::theme_bw() + ggplot2::xlim(c(0, max(newPatientData$MOMENT_END)))
  return(p)
}


################################################################################
#
# State cost distribution plots
#
################################################################################
#' Get patients' state statistics
#'
#' @param connection Connection to database
#' @param dbms The database dialect
#' @param cdmTmpSchema Temp tables' schema
#' @keywords internal
#' @return plotplot
getCostDistPlot <- function(connection, dbms, cdmTmpSchema) {
  ParallelLogger::logInfo("Quering information about the distribution of the costs per state")
  # Now let's query a complete table
  data <- DatabaseConnector::querySql(
    connection <- connection,
    sql <- SqlRender::translate(
      targetDialect <- dbms,
      sql = SqlRender::render(sql = "SELECT tma_states.STATE as STATE, SUM(cost_person.total_charge) as TOTAL_CHARGE
FROM @cdmTmpSchema.cost_person
LEFT JOIN tma_states
  ON cost_person.person_id = tma_states.SUBJECT_ID
      WHERE cost_person.date BETWEEN tma_states.STATE_START_DATE AND tma_states.STATE_END_DATE
    GROUP BY cost_person.person_id, tma_states.STATE;", cdmTmpSchema = cdmTmpSchema)
    ))
  
  data[is.na(data)] <- 0
  
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = TOTAL_CHARGE, fill = factor(STATE))) +
    ggplot2::geom_density(alpha = 0.4) + ggplot2::xlim(0,mean(data$TOTAL_CHARGE) + 1.96*2*sd(data$TOTAL_CHARGE)) + ggplot2::labs(fill='State') + ggplot2::xlab('Total charge') + ggplot2::ylab('Density') 
  return(plot)
}



################################################################################
#
#
#
################################################################################


# All credit to HealthInformaticsUT/TrajectoryViz package developers
#' Visualise patients' trajectories with sunburst plot
#'
#' @param pateintData A data.frame object which is output of Cohort2Trajectory package data.frame object which is resulting from
#' @keywords internal
drawSunburst <- function(patientData) {
  tmpInputData <- dplyr::select(patientData,SUBJECT_ID, STATE, STATE_START_DATE, STATE_END_DATE)
  labels <- c(unique(tmpInputData$STATE), "End") # "End" is required for RSunburst package input
  labels <- labels[!labels %in% c("START", "EXIT", "OUT OF COHORT", "End")]
  colors_all <- c("#E69F00", "#56B4E9", "#F0E442", "#009E73",
                  "#0072B2", "#D55E00", "#CC79A7", "#004949",
                  "#b66dff", "#924900", "#ff6db6", "#490092", "#601A4A" )
  colors <- colors_all[1:length(labels)]
  colorsDef <- stats::setNames(c(colors,"#cccccc"), c(labels, "OUT OF COHORT"))
  

  patStateLevel <-
    dplyr::mutate(
      dplyr::select(
        dplyr::ungroup(dplyr::mutate(
          dplyr::group_by(dplyr::ungroup(
            dplyr::mutate(
              dplyr::arrange(
                dplyr::group_by(
                  dplyr::filter(tmpInputData, STATE != "START" & STATE != "EXIT"),
                  SUBJECT_ID
                ),
                SUBJECT_ID,
                STATE_START_DATE
              ),
              TIME = 1:dplyr::n()
            )
          ), SUBJECT_ID),
          OtherLEVEL = with(rle(STATE), rep(seq_along(lengths), lengths))
        )),
        -STATE_START_DATE,
        -STATE_END_DATE
      ), STATEOtherLEVEL = paste(STATE, OtherLEVEL, sep = "-")
      )
  
  data1 <-
    dplyr::mutate(dplyr::ungroup(dplyr::mutate(
      dplyr::group_by(
        dplyr::filter(patStateLevel, STATE != "OUT OF COHORT"),
        SUBJECT_ID
      ),
      LEVEL = with(rle(STATE), rep(seq_along(lengths), lengths))
    )),
    STATELEVEL = paste(STATE, LEVEL, sep = "-"))
  
  patStateLevel <- dplyr::left_join(patStateLevel, data1)
  
  patStateLevel1 <-
    dplyr::select(dplyr::arrange(dplyr::ungroup(
      dplyr::mutate(
        dplyr::group_by(
          dplyr::arrange(unique(
            dplyr::select(patStateLevel, SUBJECT_ID, STATE, STATEOtherLEVEL)
          ), STATE),
          SUBJECT_ID,
          grp = with(rle(STATE), rep(seq_along(lengths), lengths))
        ),
        counts = 1:dplyr::n()
      )
    ), SUBJECT_ID),
    -grp,-STATE)
  
  patStateLevel <- dplyr::left_join(patStateLevel, patStateLevel1)
  
  patStateLevel <-
    dplyr::select(
      dplyr::mutate(patStateLevel,DrugLEVEL = paste(STATE, counts, sep = "-")),
      SUBJECT_ID,
      TIME,
      STATE,
      OtherLEVEL,
      STATEOtherLEVEL,
      LEVEL,
      STATELEVEL,
      DrugLEVEL
    )
  
  ### Data prep for "patPaths" as an input to plot patient-level pathways   ----
  patStateLens <-
    tidyr::replace_na(
      tidyr::separate(
        dplyr::ungroup(dplyr::summarise(
          dplyr::group_by(dplyr::distinct(
            dplyr::select(dplyr::ungroup(
              dplyr::mutate(
                dplyr::group_by(dplyr::select(dplyr::ungroup(
                  dplyr::mutate(
                    dplyr::group_by(
                      dplyr::filter(
                        dplyr::arrange(
                          dplyr::select(patStateLevel, SUBJECT_ID, STATE, TIME, DrugLEVEL),
                          SUBJECT_ID
                        ),
                        STATE != "OUT OF COHORT"
                      ),
                      SUBJECT_ID,
                      grp = with(rle(DrugLEVEL), rep(seq_along(lengths), lengths)),
                      COUNT_CONS = 1:dplyr::n()
                    )
                  )
                ), -grp), SUBJECT_ID, DrugLEVEL),
                eraLen = max(COUNT_CONS)
              )
            ), -TIME,-COUNT_CONS)
          ),
          SUBJECT_ID),
          Len = paste0(eraLen, collapse = "-")
        )),
        Len,
        c(
          "Len1",
          "Len2",
          "Len3",
          "Len4",
          "Len5",
          "Len6",
          "Len7",
          "Len8",
          "Len9",
          "Len10",
          "Len11"
        ),
        "-",
        convert = TRUE,
        fill = "right"
      ),
      list(
        Len1 = 0,
        Len2 = 0,
        Len3 = 0,
        Len4 = 0,
        Len5 = 0,
        Len6 = 0,
        Len7 = 0,
        Len8 = 0,
        Len9 = 0,
        Len10 = 0,
        Len11 = 0
      )
    )
  
  
  
  ### Data prep for PATHS ----
  patPaths <-
    tidyr::separate(
      dplyr::mutate(
        dplyr::ungroup(dplyr::mutate(
          dplyr::group_by(dplyr::ungroup(
            dplyr::summarise(
              dplyr::group_by(
                dplyr::select(
                  dplyr::filter(dplyr::ungroup(
                    dplyr::mutate(
                      dplyr::group_by(
                        dplyr::filter(patStateLevel, STATE != "OUT OF COHORT"),
                        SUBJECT_ID,
                        grp = with(rle(STATE), rep(seq_along(lengths), lengths))
                      ),
                      COUNT_CONS = 1:dplyr::n()
                    )
                  ), COUNT_CONS == 1),
                  -grp,
                  -TIME,
                  -OtherLEVEL,
                  -COUNT_CONS
                ),
                SUBJECT_ID
              ),
              path = paste0(STATE, collapse = "-")
            )
          ), SUBJECT_ID),
          path = paste0(path, "-End", collapse = "-")
        )),
        path1 = gsub("-", " ", path),
        pathSep = path
      ),
      pathSep,
      c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10", "L11"),
      "-",
      convert = TRUE,
      fill = "right"
    )
  
  patPaths <- dplyr::left_join(patPaths, patStateLens)
  
  patPaths2 <-
    tidyr::separate(
      dplyr::mutate(
        dplyr::ungroup(dplyr::mutate(
          dplyr::group_by(dplyr::ungroup(
            dplyr::summarise(
              dplyr::group_by(
                dplyr::select(
                  dplyr::filter(dplyr::ungroup(
                    dplyr::mutate(
                      dplyr::group_by(dplyr::filter(patStateLevel, (
                        STATE != "OUT OF COHORT" |
                          (STATE == "OUT OF COHORT" &
                             OtherLEVEL == 1)
                      )), SUBJECT_ID, grp = with(rle(STATE), rep(
                        seq_along(lengths), lengths
                      ))),
                      COUNT_CONS = 1:dplyr::n()
                    )
                  ), COUNT_CONS == 1),-grp,
                  -TIME,
                  -OtherLEVEL,
                  -COUNT_CONS
                ),
                SUBJECT_ID
              ),
              path = paste0(STATE, collapse = "-")
            )
          ), SUBJECT_ID),
          path = paste0(path, "-End", collapse = "-")
        )),
        path1 = gsub("-", " ", path),
        pathSep = path
      ),
      pathSep,
      c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10", "L11"),
      "-",
      convert = TRUE,
      fill = "right"
    )
  
  patPaths2 <- dplyr::filter(dplyr::left_join(patPaths2, patStateLens),path == "OUT OF COHORT-End")
  
  patPaths <- dplyr::arrange(rbind(patPaths, patPaths2), SUBJECT_ID)
    
  ### Data prep for FREQS (sunburst input ) ----
  freqPaths <- dplyr::arrange(dplyr::ungroup(dplyr::summarise(dplyr::group_by(patPaths, path),freq = dplyr::n())),plyr::desc(freq))
  
  return(list("freqPaths" = freqPaths, "labels" = labels, "colors" = colors))
}
