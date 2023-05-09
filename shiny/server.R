################################################################################
#
# Server content
#
################################################################################
server <- function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)
  ## Retrieve patients from selected cohorts
  v <-
    reactiveValues(
      patientData = NULL,
      states = NULL,
      idStates = NULL,
      profileStochasticPlot = NULL,
      discreteMatrix = NULL,
      qmatrixCMC = NULL,
      modelCMC = NULL,
      dataCMC = NULL,
      patientCostInfo = NULL,
      kmData = NULL,
      kmAgeAnalysis = FALSE,
      kmIntervalIds = NULL,
      dtreeIds = NULL,
      dtreeDataFrame = NULL,
      generatedData = NULL,
      stateStatisticsTable = NULL
    )
  
  ##############################################################################
  #
  # Import data from source
  #
  ##############################################################################
  
  
  output$contents <- renderTable({
    file <- input$pttData
    ext <- tools::file_ext(file$datapath)
    shiny::req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    inputData <-
      as.data.frame(read.csv(file$datapath, header = TRUE))
    # Temporary: show first 5 rows of the file
    rows_to_show <- if (nrow(inputData) < 5) {
      nrow(inputData)
    }
    else {
      5
    }
    v$states <- unique(inputData$STATE_LABEL)
    ids <- unique(inputData$STATE_ID)
    states <- v$states
    idStates <- cbind(ids, states)
    colnames(idStates) <- c("STATE_ID", "STATE_LABEL")
    v$idStates <- as.data.frame(idStates)
    ##############################################################################
    #
    # Adding personal data
    #
    # ##############################################################################
    # Displaying some imported data
    v$patientData <- inputData
    v$patientData[1:rows_to_show, 1:4]
  })
  
  # observeEvent(input$trajectoryType, {
  #   if (input$trajectoryType == 1 & !is.null(v$patientData$STATE_ID)) {
  #     v$states = unique(v$patientData$STATE)
  #     ids = unique(v$patientData$STATE_ID)
  #     states = v$states
  #     idStates = cbind(ids, states)
  #     colnames(idStates) = c("STATE_ID", "STATE")
  #     v$idStates = as.data.frame(idStates)
  #   }
  #   else{
  #     v$idStates = NULL
  #   }
  # })
  
  
  output$sunburst <- sunburstR::renderSunburst({
    if (is.null(v$patientData)) {
      NULL
    }
    else {
      ParallelLogger::logInfo("Creating and saving sunburst plot!")
      sunburstDetails <- drawSunburst(v$patientData)
      plot <- sunburstR::sunburst(
          sunburstDetails$freqPaths,
          count = TRUE,
          colors = list(
            range = c(sunburstDetails$colors, "#cccccc", "#cccccc"),
            domain = c(sunburstDetails$labels, "OUT OF COHORT", "End")
          ),
          legend = list(w = 200, h = 20, s = 5),
          breadcrumb = htmlwidgets::JS(("function(x) {return x;}")),
          height = "800px",
          width = "100%"
        )
      
      save_object(sunburstDetails, path = paste(
        pathToResults,
        paste("/tmp/databases/",
              studyName,
              "/",
              studyName,
              "sunburst.rdata",
              sep = ""),
        sep = ""
      ))
      return(sunburstR::add_shiny(plot))
    }
  })
  
  
  
  ##############################################################################
  #
  # Markov analysis
  #
  ##############################################################################
  
  # observeEvent(input$trajectoryType, {
  #   if(input$trajectoryType == 1) {
  #     shiny::hideTab("shinyBody","markov")
  #   }
  # })
  
  stochasticTransitions <- reactive({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Import' tab!"
    ))
    v$discreteMatrix <- getStohasticMatrix(
      cohortData = v$patientData,
      stateCohorts = v$states,
      pathToResults = pathToResults,
      studyName = studyName,
      excludedStates = input$excludedStatesDisc
    )
    v$discreteMatrix
  })
  
  output$excludeStatesDisc <- renderUI({
    shiny::checkboxGroupInput(
      inputId = "excludedStatesDisc",
      label = h3("Select the states you want to exclude from analysis:"),
      choices = v$states
    )
  })
  output$stochasticTransitions <- DT::renderDT({
    if (input$markovAnalyseButton == 0) {
      return()
    }
    isolate(# Real value
      stochasticTransitions())
  })
  
  trajectoryStartStatistics <- reactive({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Import' tab!!"
    ))
    tmpData <- getFirstState(v$patientData, input$excludedStatesDisc)
    return(
      getFirstStateStatistics(
        connection = conn,
        dbms = dbms,
        cohortData = tmpData,
        cdmTmpSchema = cdmTmpSchema,
        studyName = studyName
      )
    )
  })
  
  output$trajectoryStartStatistics <- DT::renderDT({
    if (input$markovAnalyseButton == 0) {
      return()
    }
    isolate(# Development value
      DT::formatPercentage(
        DT::formatCurrency(
          DT::formatStyle(
            DT::datatable(trajectoryStartStatistics(), options = list(pageLength = 50)),
            colnames(trajectoryStartStatistics())
          ),
          currency = "€",
          columns = c("MEAN CHARGE", "MEAN COST", "MEAN PAID")
        ),
        columns = c("PERCENTAGE"),
        digits = 2
      ))
    
  })
  
  stateStatistics <- reactive({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Import' tab!!"
    ))
    
    v$stateStatisticsTable <- getStateStatistics(
      conn,
      dbms = dbms,
      cohortData = v$patientData,
      cdmTmpSchema = cdmTmpSchema,
      stateStatistics = v$stateStatistics,
      studyName = studyName,
      cost_domains = input$costDomainsDisc,
      excludedStates = input$excludedStatesDisc
    )
    return(v$stateStatisticsTable$table)
  })
  
  output$textMean <-
    shiny::renderText({
      paste("The mean trajectory cost of a patient:",
            round(as.numeric(v$stateStatisticsTable$mean, 2)),
            "€",
            sep = " ")
    })
  output$textMedian <-
    shiny::renderText({
      paste("The median trajectory cost of a patient:",
            round(as.numeric(v$stateStatisticsTable$median, 2)),
            "€",
            sep = " ")
    })
  
  output$stateStatistics <- DT::renderDT({
    if (input$markovAnalyseButton == 0) {
      return()
    }
    isolate(# Development value
      DT::formatPercentage(
        DT::formatCurrency(
          DT::formatStyle(
            DT::datatable(stateStatistics(), options = list(pageLength = 50)),
            colnames(stateStatistics())
          ),
          currency = "€",
          columns = c("MEAN CHARGE", "MEAN COST", "MEAN PAID")
        ),
        columns = c("PERCENTAGE"),
        digits = 2
      ))
    
  })
  
  costDistPlot <- reactive({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Import' tab!!"
    ))
    return(getCostDistPlot(conn,
                           dbms = dbms,
                           cdmTmpSchema = cdmTmpSchema))
  })
  
  
  
  output$costDistPlotDisc <- shiny::renderPlot({
    if (input$markovAnalyseButton == 0) {
      return()
    }
    isolate(costDistPlot())
  })
  
  # Solution for checking probabilities in n cycles
  transferCountP <- reactive({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Import' tab!"
    ))
    table <-
      data.frame(matrixcalc::matrix.power(v$discreteMatrix, round(input$transferCountInput)))
    table
  })
  
  output$transferCountP <- DT::renderDT({
    if (input$transferCountButton == 0) {
      return()
    }
    isolate(transferCountP())
  })
  
  
  ##############################################################################
  #
  # CMC implementation
  #
  ##############################################################################
  
  observeEvent(v$idStates, {
    # Creating a corresponding intensity matrix
    v$qmatrixCMC <-
      matrix(1, nrow = length(v$states), ncol = length(v$states))
    diag(v$qmatrixCMC) <- 0
    v$qmatrixCMC[, 1] <- 0
    v$qmatrixCMC[length(v$states), ] <- 0
    # Add correct labels
    v$idStates <- dplyr::arrange(v$idStates, STATE_ID)
    
    colnames(v$qmatrixCMC) <- v$idStates$STATE_LABEL
    rownames(v$qmatrixCMC) <- v$idStates$STATE_LABEL
  })
  
  
  
  output$cmcInitialQ <- shiny::renderUI({
    shinyMatrix::matrixInput("cmcQMatrix",
                             value =  v$qmatrixCMC ,
                             class = "numeric")
  })
  
  
  
  observeEvent(input$cmcButton, {
    # v$dataCMC = dataPreparation(patientData = v$patientData
    #                             )
    v$dataCMC  <- v$patientData
    v$modelCMC <-  msm::msm(
      STATE_ID ~ TIME_IN_COHORT,
      subject = SUBJECT_ID,
      data = v$dataCMC,
      qmatrix = input$cmcQMatrix,
      # Maybe add a checkbox for generation
      gen.inits = T,
      exacttimes = T,
      control = list(fnscale = 400000, maxit = 400)
    )
    ParallelLogger::logInfo("Continuous time Markov model calculated!")
    save_object(v$modelCMC, path = paste(
      pathToResults,
      paste(
        "/tmp/databases/",
        studyName,
        "/",
        studyName,
        "_continuous_intensity_matrix.rdata",
        sep = ""
      ),
      sep = ""
    ))
    ParallelLogger::logInfo(paste(
      "Saved to: ",
      pathToResults,
      paste(
        "/databases/",
        studyName,
        "/",
        studyName,
        "_continuous_intensity_matrix.rdata",
        sep = ""
      ),
      sep = ""
    ))
  })
  
  cmcCalculatedQ <- reactive({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Import' tab!"
    ))
    table <-
      data.frame(unclass(msm::qmatrix.msm(v$modelCMC)$estimates))
    table
  })
  
  output$cmcCalculatedQ <- DT::renderDT({
    if (input$cmcButton == 0) {
      return()
    }
    isolate(cmcCalculatedQ())
  })
  
  
  cmcCalculatedSojourn <- reactive({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Import' tab!"
    ))
    table1 <- data.frame(unclass(msm::sojourn.msm(v$modelCMC)))
    table2 <-
      data.frame(unclass(msm::totlos.msm(v$modelCMC)))[1:nrow(table1), ]
    table <- cbind(table1, table2)
    rownames(table) <-
      plyr::mapvalues(
        rownames(table),
        from = v$idStates$STATE_ID,
        to = v$idStates$STATE_LABEL,
        warn_missing = FALSE
      )
    colnames(table) <- c(
      "Estimate (days)",
      "Standard error",
      "Lower",
      "Upper",
      "Estimated stay per patient (days)"
    )
    table <- table * 365.25
    table
  })
  
  output$cmcCalculatedSojourn <- DT::renderDT({
    if (input$cmcButton == 0) {
      return()
    }
    isolate(cmcCalculatedSojourn())
  })
  
  cmcCalculatedP <- reactive({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Import' tab!"
    ))
    table <-
      data.frame(unclass(round(
        msm::pmatrix.msm(v$modelCMC, t = input$cmcTimeInput / 365.25),
        4
      )))
    table
  })
  
  output$cmcCalculatedP <- DT::renderDT({
    if (input$cmcTimeButton == 0) {
      return()
    }
    isolate(cmcCalculatedP())
  })
  
  output$excludeStatesCon <- renderUI({
    shiny::checkboxGroupInput(
      inputId = "excludedStatesCon",
      label = h3("Select the states you want to exclude from analysis:"),
      choices = v$states
    )
  })
  
  trajectoryStartStatisticsCon <- reactive({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Import' tab!!"
    ))
    tmpData <- getFirstState(v$patientData, input$excludedStatesCon)
    return(
      getFirstStateStatistics(
        connection = conn,
        dbms = dbms,
        cohortData = tmpData,
        cdmTmpSchema = cdmTmpSchema,
        studyName = studyName
      )
    )
  })
  
  output$trajectoryStartStatisticsCon <- DT::renderDT({
    if (input$cmcButton == 0) {
      return()
    }
    isolate(# Development value
      DT::formatPercentage(
        DT::formatCurrency(
          DT::formatStyle(
            DT::datatable(trajectoryStartStatistics(), options = list(pageLength = 50)),
            colnames(trajectoryStartStatistics())
          ),
          currency = "€",
          columns = c("MEAN CHARGE", "MEAN COST", "MEAN PAID")
        ),
        columns = c("PERCENTAGE"),
        digits = 2
      ))
    
  })
  
  
  stateStatisticsCon <- reactive({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Import' tab!!"
    ))
    v$stateStatisticsTable <-  getStateStatistics(
      conn,
      dbms = dbms,
      cohortData = v$patientData,
      cdmTmpSchema = cdmTmpSchema,
      studyName = studyName,
      cost_domains = input$costDomainsCon,
      excludedStates = input$excludedStatesCon
    )
    return(v$stateStatisticsTable$table)
  })
  
  output$stateStatisticsCon <- DT::renderDT({
    if (input$cmcButton == 0) {
      return()
    }
    isolate(# Development value
      DT::formatPercentage(
        DT::formatCurrency(
          DT::formatStyle(
            DT::datatable(stateStatistics(), options = list(pageLength = 50)),
            colnames(stateStatistics())
          ),
          currency = "€",
          columns = c("MEAN CHARGE", "MEAN COST", "MEAN PAID")
        ),
        columns = c("PERCENTAGE"),
        digits = 2
      ))
    
  } ,)
  
  costDistPlotCon <- reactive({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Import' tab!!"
    ))
    return(getCostDistPlot(conn,
                           dbms = dbms,
                           cdmTmpSchema = cdmTmpSchema))
  })
  
  
  
  output$costDistPlotCon <- shiny::renderPlot({
    if (input$cmcButton == 0) {
      return()
    }
    isolate(costDistPlotCon())
  })
  ##############################################################################
  #
  # Kaplan-Meier analysis content
  #
  ##############################################################################
  
  output$kmStateChoices <- renderUI({
    choices <-  sort(paste(rbind(t(combn(
      rev(v$states), 2
    )), t(combn((v$states), 2
    )))[, 1],
    " → ",
    rbind(t(combn(
      rev(v$states), 2
    )), t(
      combn(v$states, 2)
    ))[, 2]))
    choices <- choices[!startsWith(choices, "EXIT")]
    choices <- choices[!endsWith(choices, "START")]
    shiny::checkboxGroupInput("kmStates", label = NULL,
                              choices = choices)
    
  })
  
  observeEvent(input$kmAgeAnalysisInclusion, {
    if (input$kmAgeAnalysisInclusion == 0) {
      v$kmAgeAnalysis <- FALSE
    }
    else {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      progress$set(message = "Loading personal data...", value = 1 / 4)
      
      v$patientData <- addPersonalData(v$patientData, conn)
      progress$set(message = "Loading  complete!", value = 1)
      v$kmAgeAnalysis <- TRUE
    }
  })
  
  output$kmAgeAnalysisIntervals <- renderUI({
    if (!v$kmAgeAnalysis) {
      return()
    }
    # # minAge5 = (ceiling(minAge/5)-1)*5
    # # maxAge5 = (ceiling(maxAge/5))*5
    
    # optionalAges = seq(from = minAge5, to = maxAge5,by = as.numeric(input$kmAgeAnalysisInclusion))
    # optionalIntervals = paste(optionalAges, c(optionalAges[-1], ""), sep = "-")
    
    #shiny::checkboxGroupInput("kmAgeAnalysisChosen", label = "Age interval", choiceNames = optionalIntervals, choiceValues = 1:length(optionalIntervals))
    #shiny::sliderInput('kmAgeAnalysisChosen', label = "Age interval", min = minAge, max = maxAge, value = c(minAge,maxAge), step = NULL, round = TRUE)
    shiny::actionButton("addIntervalButton", "Add an interval")
  })
  
  output$kmRemoveAgeAnalysisIntervals <- renderUI({
    if (!v$kmAgeAnalysis) {
      return()
    }
    shiny::actionButton("rmIntervalButton", "Remove an interval")
  })
  
  observeEvent(input$addIntervalButton, {
    if (is.null(v$kmIntervalIds)) {
      v$kmIntervalIds <<- c(1)
    } else {
      v$kmIntervalIds <<- c(v$kmIntervalIds, max(v$kmIntervalIds) + 1)
    }
    minAge <- max(round(min(c(
      v$patientData$AGE, 120
    ))) - 1, 0)
    maxAge <- round(max(c(v$patientData$AGE, 0))) + 1
    output$inputsIntervals <- renderUI({
      tagList(lapply(1:length(v$kmIntervalIds), function(i) {
        shiny::sliderInput(
          paste0("intervalInput", v$kmIntervalIds[i]),
          label = sprintf("Interval #%d", v$kmIntervalIds[i]),
          min = minAge,
          max = maxAge,
          value = c(minAge, maxAge),
          step = NULL,
          round = TRUE
        )
      }))
    })
  })
  
  #TODO: Add rm button
  
  observeEvent(input$rmIntervalButton, {
    if (is.null(v$kmIntervalIds) | length(v$kmIntervalIds) == 1) {
      v$kmIntervalIds <<- NULL
      
      output$inputsIntervals <- renderUI({
        
      })
    } else {
      v$kmIntervalIds <<-
        v$kmIntervalIds[v$kmIntervalIds < max(v$kmIntervalIds)]
      minAge <- max(round(min(c(
        v$patientData$AGE, 120
      ))) - 1, 0)
      maxAge <- round(max(c(v$patientData$AGE, 0))) + 1
      output$inputsIntervals <- renderUI({
        tagList(lapply(1:length(v$kmIntervalIds), function(i) {
          shiny::sliderInput(
            paste0("intervalInput", v$kmIntervalIds[i]),
            label = sprintf("Interval #%d", v$kmIntervalIds[i]),
            min = minAge,
            max = maxAge,
            value = c(minAge, maxAge),
            step = NULL,
            round = TRUE
          )
        }))
      })
    }
  })
  
  
  #TODO: NB! For combining ggsurvplots we might use ggsurvplot_combine function
  
  kmPlots <- reactive({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Import' tab!"
    ))
    plots <- list()
    for (i in 1:length(input$kmStates)) {
      states <- stringr::str_split(input$kmStates[i], "  →  ")[[1]]
      
      interval_ids <-
        sapply(1:length(v$kmIntervalIds), function(i) {
          paste("intervalInput", v$kmIntervalIds[i], sep = "")
        })
      inputItervalsDT <- list()
      
      for (j in 1:length(interval_ids)) {
        inputItervalsDT [[j]] <- input[[interval_ids[j]]]
      }
      v$kmData <- kmDataPreparation(
        v$patientData,
        startCohortId = states[1],
        endCohortId <- states[2],
        ageInterval = as.numeric(input$kmAgeAnalysisInclusion),
        selectedIntervals = inputItervalsDT,
        survivalType = "regular"
      )
      
      surv_object <-
        survival::Surv(time = v$kmData$DATE,
                       event =  v$kmData$OUTCOME)
      v$kmData$SURV_OBJECT = surv_object
      fit <- survival::survfit(SURV_OBJECT ~ GROUP, data = v$kmData)
      ########################################################################
      #
      # Has to be checked if working, ATM no p rank test is shown
      #
      ########################################################################
      if (v$kmAgeAnalysis) {
        # p = survminer::ggsurvplot(fit, data = v$kmData)$plot + ggplot2::labs(title = input$kmStates[i], pval = TRUE, conf.int = TRUE)
        p <- survminer::ggsurvplot(
          fit,
          data = v$kmData,
          pval = TRUE,
          conf.int = TRUE,
          surv.median.line = 'hv'
        )$plot  + ggplot2::labs(title = input$kmStates[i])
      }
      else {
        p <-
          survminer::ggsurvplot(fit, data = v$kmData, conf.int = TRUE)$plot + ggplot2::labs(title = input$kmStates[i])#, pval = TRUE)$plot
      }
      plots[[i]] <- p
    }
    
    return(ggpubr::ggarrange(plotlist = plots, ncol = 1))
  })
  
  output$kmPlotOutput <- renderPlot({
    if (input$kmPlotButton == 0)
      return() #renderPlot
    isolate(kmPlots())
  })
  ##############################################################################
  #
  # Profile tab content
  #
  ##############################################################################
  output$patientSex <- shinydashboard::renderInfoBox({
    if (input$profileSearchButton == 0)
      return(shinydashboard::infoBox(
        "Sex",
        "Unknown",
        icon = shiny::icon("venus-mars"),
        color = "red"
      ))
    isolate(
      shinydashboard::infoBox(
        "Sex",
        v$dataPerson$genderString[1],
        icon = shiny::icon("venus-mars"),
        color = "red"
      )
    )
  })
  
  output$patientAge <- shinydashboard::renderInfoBox({
    if (input$profileSearchButton == 0)
      return(
        shinydashboard::infoBox(
          "Birthdate",
          "??-??-????",
          icon = shiny::icon("calendar-alt"),
          color = "yellow"
        )
      )
    isolate(
      shinydashboard::infoBox(
        "Birthdate",
        v$dataPerson$birthtimeString[1],
        icon = shiny::icon("calendar-alt"),
        color = "yellow"
      )
    )
  })
  
  
  output$patientExists <- shinydashboard::renderInfoBox({
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Analyze' tab!!"
    ))
    if (input$profileSearchButton == 0)
      return(shinydashboard::infoBox(
        "In cohort",
        "?",
        icon = shiny::icon("registered"),
        color = "green"
      ))
    isolate(
      shinydashboard::infoBox(
        "In cohort",
        ifelse(idExists(
          v$patientData,  as.numeric(input$profiles_personIdInput)
        ), "Yes", "No"),
        icon = shiny::icon("registered"),
        color = "green"
      )
    )
  })
  
  
  observeEvent(input$profileSearchButton, {
    v$dataPerson <-
      getProfileData(
        conn,
        dbms = connectionDetails$dbms,
        resultsSchema = cdmResultsSchema,
        cdmSchema = cdmSchema,
        personId = input$profiles_personIdInput
      )
  })
  
  observeEvent(input$profileSearchButton, {
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Analyze' tab!!"
    ))
    # Check if selected patient's id exists, if it does generate the plot, if it does not, do not generate the plot
    if (idExists(v$patientData, input$profiles_personIdInput)) {
      v$profileStochasticPlot <-
        visualisePatient(
          v$patientData,
          as.numeric(input$profiles_personIdInput),
          connection = conn,
          dbms = dbms,
          cdmTmpSchema = cdmTmpSchema
          #theme = input$visualisePatient
        )
    }
  })
  
  observeEvent(input$markovAnalysePlotButton, {
    validate(need(
      !is.null(v$patientData),
      "Please import relevant cohorts under 'Analyze' tab!!"
    ))
    # Check if selected patient's id exists, if it does generate the plot, if it does not, do not generate the plot
    if (idExists(v$patientData,  as.numeric(input$profiles_personIdInput))) {
      v$profileStochasticPlot <-
        visualisePatient(
          v$patientData,
          as.numeric(input$profiles_personIdInput),
          trajectoryStopDays = as.numeric(input$markovStateDurationPlot),
          theme = input$markovStatePlotType,
          cdmTmpSchema = cdmTmpSchema,
          dbms = dbms,
          feature = input$markovStatePlotFeatures
        )
    }
  })
  
  output$patientPlot <- shiny::renderPlot({
    # Check if the plot has been generated, if it has been --> show
    if (is.null(v$profileStochasticPlot)) {
      return()
    }
    v$profileStochasticPlot
  })
  
  ################################################################################
  #
  # Decision Trees
  #
  ################################################################################
  
  ##############################################################################
  #
  # Adding layers to the tree
  #
  ##############################################################################
  observeEvent(input$addLayerButton, {
    if (is.null(v$dtreeIds)) {
      v$dtreeIds <<- 1
    } else {
      v$dtreeIds <<- c(v$dtreeIds, max(v$dtreeIds) + 1)
    }
    output$inputs <- renderUI({
      tagList(lapply(1:length(v$dtreeIds), function(i) {
        checkboxGroupInput(
          paste0("layerInput", v$dtreeIds[i]),
          sprintf("Layer #%d", v$dtreeIds[i]),
          choices = v$states
        ) # choices = v$customisedStates
      }))
    })
  })
  
  observeEvent(input$rmLayerButton, {
    if (is.null(v$dtreeIds) | length(v$dtreeIds) == 1) {
      v$dtreeIds <- NULL
      output$inputs <- renderUI({
        NULL
      })
    } else {
      v$dtreeIds <<- v$dtreeIds[v$dtreeIds < max(v$dtreeIds)]
      
      output$inputs <- renderUI({
        ifelse(is.null(v$dtreeIds), NULL,
               tagList(lapply(1:length(v$dtreeIds), function(i) {
                 checkboxGroupInput(
                   paste0("layerInput", v$dtreeIds[i]),
                   sprintf("Layer #%d", v$dtreeIds[i]),
                   choices = v$states
                 ) # choices = v$customisedStates
               })))
      })
    }
  })
  
  output$plotOut <- shiny::renderPlot({
    if (input$getLayersButton == 0) {
      return() #renderPlot
    }
    else {
      # Get ids for layers
      layer_ids <- sapply(1:length(v$dtreeIds), function(i) {
        paste("layerInput", v$dtreeIds[i], sep = "")
      })
      inputLayersDT <- list()
      
      # This is a sanity check whether or not every checkbox has at least one box ticked
      nullCheck <- sum(sapply(sapply(layer_ids, function(i) {
        input[[i]]
      }), is.null)) > 0
      if (nullCheck) {
        return() #renderPlot
      } else {
        for (i in 1:length(layer_ids)) {
          inputLayersDT [[i]] <- sprintf(input[[layer_ids[i]]])
        }
        labels <- c('START', inputLayersDT [[1]])
        # Multiplier for counting branches
        n <- 1
        d <-
          data.frame(from = "0_0", to = paste("0_", seq(1, length(
            inputLayersDT [[1]]
          )), sep = ""))
        edges <- d
        d_last <- d
        if (length(layer_ids) - 1 > 0) {
          for (i in 1:(length(layer_ids) - 1)) {
            d_next <-
              data.frame(
                from = rep(d_last$to, each = length(inputLayersDT [[i + 1]])),
                to = paste(i, seq(
                  1, length(d_last$to) * length(inputLayersDT [[i + 1]])
                ), sep = "_")
              )
            
            n <- n * length(inputLayersDT [[i]])
            labels <-
              c(labels, rep(inputLayersDT [[i + 1]], times = n))
            edges <- rbind(edges, d_next)
            
            d_last <- d_next
          }
        }
        ##########################################################################
        #
        # Let's save the information in dataframe "edges" to a reactive value
        # for using and modifying it later
        #
        ##########################################################################
        v$dtreeDataFrame <- edges
        
        mygraph <- igraph::graph_from_data_frame(edges)
        p <-
          ggraph::ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
          ggraph::geom_edge_diagonal() +
          ggraph::geom_node_text(
            ggplot2::aes(label = as.character(labels), filter = leaf),
            hjust = 1,
            nudge_y = .3
          ) +
          ggraph::geom_node_text(
            ggplot2::aes(label = as.character(labels), filter = !leaf),
            hjust = 1,
            nudge_x = 0.5,
            nudge_y = -.1
          ) +
          ggplot2::theme_void() +
          ggplot2::coord_flip() +
          ggplot2::scale_y_reverse()
        return(p)
      }
    }
  })
  
  
  
  output$plotOut2 <- renderPlot({
    if (input$generateTreeButton == 0) {
      return() #renderPlot
    }
    else {
      # Get ids for layers
      layer_ids <- sapply(1:length(v$dtreeIds), function(i) {
        paste("layerInput", v$dtreeIds[i], sep = "")
      })
      inputLayersDT  <- list()
      
      # This is a sanity check whether or not every checkbox has at least one box ticked
      nullCheck <- sum(sapply(sapply(layer_ids, function(i) {
        input[[i]]
      }), is.null)) > 0
      
      if (nullCheck) {
        return() #renderPlot
      } else {
        for (i in 1:length(layer_ids)) {
          inputLayersDT [[i]] <- sprintf(input[[layer_ids[i]]])
        }
        
        # Multiplier for counting branches
        n <- 1
        d <-
          data.frame(
            from = "0_0" ,
            to = paste("0_", seq(1, length(
              inputLayersDT [[1]]
            )), sep = ""),
            from_label = 'START',
            to_label = inputLayersDT [[1]]
          )
        edges <- d
        d_last <- d
        if (length(layer_ids) - 1 > 0) {
          for (i in 1:(length(layer_ids) - 1)) {
            d_next <-
              data.frame(
                from = rep(d_last$to, each = length(inputLayersDT [[i + 1]])),
                to = paste(i, seq(
                  1, length(d_last$to) * length(inputLayersDT [[i + 1]])
                ), sep = "_"),
                from_label = rep(d_last$to_label, each = length(inputLayersDT [[i + 1]])),
                to_label = rep(inputLayersDT [[i + 1]], times = length(d_last$to_label))
              )
            
            n <- n * length(inputLayersDT [[i]])
            edges <- rbind(edges, d_next)
            
            d_last <- d_next
          }
        }
        ##########################################################################
        #
        # Let's save the information in dataframe "edges" to a reactive value
        # for using and modifying it later
        #
        ##########################################################################
        v$dtreeDataFrame <- edges
        mygraph <- igraph::graph_from_data_frame(edges)
        # vertices <- data.frame(
        #   name=name,
        #   group=c( rep(NA,8) ,  rep( paste("group", seq(1,7), sep=""), each=7)),
        #   cluster=sample(letters[1:4], length(name), replace=T),
        #   value=sample(seq(10,30), length(name), replace=T)
        # )
        dTreeProbabilities <- getDTreeProbabilities(cohortData <-
                                                      v$patientData,
                                                    stateCohorts <- v$states)
        
        edges <- mergeEdgeTransition(
          cohortData = v$patientData,
          dTreeEdges = edges ,
          stateTransitionTable = dTreeProbabilities,
          probabilityType = input$dTreeProbabilityType
        )
        labels <-
          c('START', paste(edges$to_label, paste(edges$PROB, "%", sep =
                                                   ""), sep = " "))
        p <-
          ggraph::ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
          ggraph::geom_edge_diagonal() +
          ggraph::geom_node_text(
            ggplot2::aes(label = as.character(labels), filter = leaf),
            hjust = 1,
            nudge_y = .3
          ) +
          ggraph::geom_node_text(
            ggplot2::aes(label = as.character(labels), filter = !leaf),
            hjust = 1,
            nudge_x = 0.5,
            nudge_y = -.1
          ) +
          ggplot2::theme_void() +
          ggplot2::coord_flip() +
          ggplot2::scale_y_reverse()
        return(p)
      }
    }
  })
  ##############################################################################
  #
  # Data generation
  #
  ##############################################################################
  
  
  observeEvent(input$generationButton, {
    if (input$trajectoryTypeMatrix == 2) {
      v$generatedData <- generateDataContinuous(
        model = v$modelCMC,
        n = input$generationNrPatients,
        minDate = input$generationDateRange[1],
        maxDate = input$generationDateRange[2],
        pathToResults = pathToResults,
        generateCost = input$trajectoryGenerateCost,
        statisticsTable = v$stateStatisticsTable,
        studyName = studyName
      )
    }
    else {
      #      firstStates = getFirstState(v$patientData)$FIRST_STATE
      #      startingProbabilities = as.data.frame(matrix(cumsum(prop.table(
      #        table(firstStates)
      #      )), nrow = 1))
      #      colnames(startingProbabilities) = unique(firstStates)
      v$generatedData <- generateDataDiscrete(
        transitionMatrix = v$discreteMatrix,
        n = input$generationNrPatients,
        minDate = input$generationDateRange[1],
        maxDate = input$generationDateRange[2],
        maxOut = input$generationMaxOtherDuration ,
        stateDuration = input$generationStateDuration,
        pathToResults = pathToResults,
        generateCost = input$trajectoryGenerateCost,
        statisticsTable = v$stateStatisticsTable,
        studyName = studyName
      )
    }
  })
  
  generationCompareTable <- reactive({
    validate(need(!is.null(v$generatedData), "Please generate some data!"))
    compareTrajectoryData(observedData = if (input$trajectoryTypeMatrix == 2) {
      v$dataCMC
    }
    else {
      v$patientData
    },
    generatedData <- v$generatedData)
  })
  
  output$generationCompareTable <-  shiny::renderTable({
    if (input$generationButton == 0) {
      return()
    }
    isolate(generationCompareTable())
  }, digits = 4)
  
  
  generationLRMatrix <- reactive({
    validate(validate(need(
      !is.null(v$generatedData), "Please generate some data!"
    )))
    compareTrajectoryDataLogRank(observedData = if (input$trajectoryTypeMatrix == 2) {
      v$dataCMC
    }
    else {
      v$patientData
    },
    generatedData <- v$generatedData)
  })
  
  output$generationLRMatrix <- DT::renderDT({
    if (input$generationButton == 0) {
      return()
    }
    isolate(generationLRMatrix())
  })
  ################################################################################
  #
  # Publishing
  #
  ################################################################################
  
  # observeEvent(input$publishButton, {
  #   ParallelLogger::logInfo("Saving data")
  #   save_object(paste(pathToResults, "/published3.Rdata", sep = ""),
  #               chronologicalTransitions())
  # })
  
}
