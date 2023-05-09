#' Prepare patientData dataframe for Kaplan-Meier plots
#'
#' This function prepares patientData dataframe for Kaplan-Meier plots
#'
#' @param cohortData A data.frame object which is output of Cohort2Trajectory package
#' @param startCohortId Start cohort Id
#' @param endCohortId End cohort Id
#' @param ageInterval Indicator whether age analysis are prohibited or allowed, if 0 then prohibited
#' @param selectedIntervals Age intervals which were selected
#' @param survivalType If "nearest", then comparisons are made with the startCohortId state present nearest to the endCohortId
#' @keywords internal
#' @return A dataframe ready for using msm package
kmDataPreparation <-
  function(patientData,
           startCohortId,
           endCohortId,
           ageInterval = NULL,
           selectedIntervals = NULL,
           survivalType = "regular") {
    if (is.null(patientData$AGE)) {
      patientData$AGE <- 0
    }
    if (is.null(patientData$GENDER_CONCEPT_ID)) {
      patientData$GENDER_CONCEPT_ID <- 0
    }
    if (is.null(patientData$TIME_IN_COHORT)) {
      patientData$TIME_IN_COHORT <- 0
    }
    patientData$SUBJECT_ID <- as.integer(patientData$SUBJECT_ID)
    if ("COHORT_START_DATE" %in% colnames(patientData)) {
      colnames(patientData) <-
        c(
          "SUBJECT_ID",
          "STATE_LABEL",
          "STATE_START_DATE",
          "STATE_END_DATE",
          "GENDER_CONCEPT_ID",
          "AGE",
          "TIME_IN_COHORT"
        )
    }
    data <-
      dplyr::select(patientData,
                    SUBJECT_ID,
                    STATE_LABEL,
                    STATE_START_DATE,
                    STATE_END_DATE,
                    AGE)
    data <- dplyr::mutate(data, ID = 1:nrow(data))
    
    data1 <- dplyr::filter(data, STATE_LABEL == startCohortId)
    data2 <- dplyr::filter(data, STATE_LABEL == endCohortId)
    if (nrow(data1) == 0 | nrow(data2) == 0) {
      df <- data.frame(matrix(ncol = 9, nrow = 0))
      colnames(df) <-
        c(
          "SUBJECT_ID",
          "START_DATE",
          "AGE",
          "ID_START",
          "END_DATE",
          "ID_END",
          "OUTCOME",
          "GROUP",
          "DATE"
        )
      return(df)
    }
    
    if (survivalType == "nearest") {
      data2 <- dplyr::mutate(dplyr::summarise(
        dplyr::group_by(data2,
                        SUBJECT_ID),
        END_DATE = min(STATE_START_DATE),
        ID = min(ID)
      ),
      STATE_LABEL = endCohortId)
      
      data_surv <-  merge(data1, data2, by = "SUBJECT_ID", all.x = T)
      data_surv <-
        dplyr::select(data_surv,-STATE_END_DATE,-ID.x,-ID.y)
      
      colnames(data_surv) <-
        c("SUBJECT_ID",
          "ID_START",
          "START_DATE",
          "AGE",
          "END_DATE",
          "ID_END")
      data_surv <- dplyr::mutate(
        dplyr::filter(data_surv, START_DATE < END_DATE |
                        is.na(END_DATE)),
        DIFF = difftime(as.Date(START_DATE), as.Date(END_DATE))
      )
      data_surv <- dplyr::filter(dplyr::group_by(data_surv,
                                                 SUBJECT_ID),
                                 DIFF == max(DIFF) | is.na(DIFF))
      data_surv <- dplyr::select(data_surv,-DIFF)
      # keeping earliest events of subjects which do not have end state event occurrence
      
      data_surv <-
        dplyr::slice(dplyr::group_by(data_surv, SUBJECT_ID), 1)
      # ids = c()
      # for (patientID in unique(data2$SUBJECT_ID)) {
      #   endStateDate = dplyr::filter(data2, SUBJECT_ID==patientID)$STATE_START_DATE
      #   startStatesInfo = dplyr::filter(data1, SUBJECT_ID==patientID, STATE_START_DATE <= endStateDate)
      #   ids = c(ids,startStatesInfo$ID[which.min(difftime(as.Date(startStatesInfo$STATE_START_DATE),as.Date(endStateDate)))])
      # }
      
      # data1_plus = dplyr::filter(data1, ID %in% ids)
      #
      # data1_minus = dplyr::filter(
      #   dplyr::mutate(
      #   dplyr::summarise(
      #     dplyr::group_by(
      #       data1,
      #       SUBJECT_ID),
      #     # We are taking min(AGE) so that the inclusion age of the patient could be used in analysis
      #     STATE_START_DATE = min(STATE_START_DATE), AGE = min(AGE), ID = min(ID)), STATE= startCohortId), !(ID %in% ids))
      # data1_minus = dplyr::select(data1_minus, -ID)
      # colnames(data1_minus) = c("SUBJECT_ID", "START_DATE","AGE", "ID_START")
      # data1_plus = dplyr::select(data1_plus, -STATE_END_DATE, -ID)
      # colnames(data1_plus) = c("SUBJECT_ID", "ID_START","START_DATE", "AGE")
      #
      # data1 = rbind(data1_plus, data1_minus)
      # data2 = dplyr::select(data2, -ID)
      # colnames(data2) = c("SUBJECT_ID", "END_DATE", "ID_END")
    }
    else {
      data1 <- dplyr::mutate(
        dplyr::summarise(
          dplyr::group_by(data1,
                          SUBJECT_ID),
          # We are taking min(AGE) so that the inclusion age of the patient could be used in analysis
          STATE_START_DATE = min(STATE_START_DATE),
          AGE = min(AGE),
          ID = min(ID)
        ),
        STATE_LABEL = startCohortId
      )
      # Let's exclude all rows which were used for starting states
      
      data2 <- dplyr::filter(data2,!ID %in% data1$ID)
      
      data2 <- dplyr::mutate(dplyr::summarise(
        dplyr::group_by(data2,
                        SUBJECT_ID),
        STATE_START_DATE = min(STATE_START_DATE)
      ),
      STATE_LABEL = endCohortId)
      data1 <- dplyr::select(data1,!ID)
      colnames(data1) <- c("SUBJECT_ID", "START_DATE", "AGE", "ID_START")
      colnames(data2) <- c("SUBJECT_ID", "END_DATE", "ID_END")
      data_surv <- merge(data1, data2, by = "SUBJECT_ID", all.x = T)
    }
    # max_date = as.numeric(difftime(max(data$STATE_END_DATE), min(data$STATE_START_DATE)))
    # Kui end date on NA is handled aga kui on enne kui start???
    data_surv <-
      dplyr::mutate(data_surv, OUTCOME = ifelse(is.na(END_DATE), 0, 1))
    data_surv$GROUP <- "All"
    ################################################################################
    #
    # We will use the base dataframe data_surv to add other groups which are specified.
    # WITHOUT removing subjects from the target group. If a subject satisfies a group's
    # condition, a new data row is created
    #
    ################################################################################
    
    data_clone <- data.frame(data_surv)
    # Here we should start checking for conditions
    if (!(is.null(ageInterval))) {
      # minAge = min(c(data_surv$AGE,120))
      # maxAge = max(c(data_surv$AGE,0))
      # minAge5 = (ceiling(minAge/5)-1)*5
      # maxAge5 = (ceiling(maxAge/5))*5
      # optionalAges = c(seq(from = minAge5, to = maxAge5,by = as.numeric(ageInterval)),Inf)
      
      for (chosenInterval in selectedIntervals) {
        data_clone_to_add <-
          dplyr::filter(dplyr::mutate(
            data_clone,
            GROUP = dplyr::if_else(
              as.numeric(AGE) >= chosenInterval[1] &
                as.numeric(AGE) <= chosenInterval[2],
              paste(chosenInterval[1], chosenInterval[2], sep = "-") ,
              GROUP
            )
          ),
          GROUP != "All")
        data_surv <- rbind(data_surv, data_clone_to_add)
      }
    }
    data_surv$START_DATE <- as.Date(data_surv$START_DATE)
    data_surv$END_DATE <- as.Date(data_surv$END_DATE)
    data_surv <-
      dplyr::mutate(data_surv, DATE = ifelse(is.na(END_DATE), -1, as.numeric(
        difftime(END_DATE, START_DATE, units = "days")
      )))
    # When the difference of dates is negative, switch outcome to 0?
    data_surv <-
      dplyr::mutate(data_surv, OUTCOME = ifelse(DATE < 0, 0, OUTCOME))
    max_date <- max(data_surv$DATE)
    data_surv <-
      dplyr::mutate(data_surv, DATE = ifelse(DATE == -1, max_date , DATE))
    data_surv <- dplyr::filter(data_surv, DATE >= 0)
    
    data_surv$START_DATE <- as.Date(data_surv$START_DATE)
    data_surv$END_DATE <- as.Date(data_surv$END_DATE)
    
    return(data_surv)
  }


################################################################################
#
#
#
################################################################################
