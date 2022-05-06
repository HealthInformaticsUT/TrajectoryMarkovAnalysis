################################################################################
#
# Header content
#
################################################################################
header =  shinydashboard::dashboardHeader(title = "TrajectoryMarkovAnalysis dashboard",
                                          tags$li(
                                            div(
                                              img(
                                                src = 'images/logo.png',
                                                title = "OHDSI PLP",
                                                height = "40px",
                                                width = "40px"
                                              ),
                                              style = "padding-top:0px; padding-bottom:0px;"
                                            ),
                                            class = "dropdown"
                                          ))

################################################################################
#
# Sidebar content
#
################################################################################
sidebar = shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Description", tabName = "description", icon = icon("home")),
    shinydashboard::menuItem("Import", tabName = "import", icon = icon("file-import")),
    shinydashboard::menuItem(
      "Discrete time Markov analysis",
      tabName = "markov",
      icon = icon("magic")
    ),
    shinydashboard::menuItem(
      "Continuous time Markov analysis",
      tabName = "cmarkov",
      icon = icon("hat-wizard")
    ),
    shinydashboard::menuItem(
      "Kaplan-Meier plots",
      tabName = "kmplots",
      icon = icon("chart-line")
    ),
    shinydashboard::menuItem("Decision trees", tabName = "dtrees", icon = icon("tree")),
    shinydashboard::menuItem("Profiles", tabName = "profiles", icon = icon("user")),
    shinydashboard::menuItem("Generation", tabName = "generation", icon = icon("bolt")),
    # shinydashboard::menuItem("Publish", tabName = "publish", icon = icon("upload")),
    shinydashboard::menuItem("Help", tabName = "help", icon = icon("info"))
  )
)

################################################################################
#
# Body content
#
################################################################################

body = shinydashboard::dashboardBody(
  id = "shinyBody",
  shinydashboard::tabItems(
    # First tab content
    shinydashboard::tabItem(
      tabName = "description",
      shiny::includeMarkdown(path = "./www/shinyDescription.md")
      
    ),
    ################################################################################
    #
    # Import content
    #
    ################################################################################
    # Second tab content
    shinydashboard::tabItem(
      tabName = "import",
      # Importing the data
      shinydashboard::box(
        title = "Import the patient treatment trajectory data:",
        status = "primary",
        solidHeader = TRUE,
        shiny::fileInput("pttData", "Choose CSV File (MAX 150MB)", accept = ".csv"),
        shiny::radioButtons(
          inputId = "trajectoryType",
          label = p("Which type of data are you using:"),
          choices = list("Discrete time" = 0, "Continuous time" = 1)
        )
      ),
      shinydashboard::box(
        title = "Preview of the imported dataset:",
        status = "primary",
        solidHeader = TRUE,
        shinycssloaders::withSpinner(tableOutput("contents"))
      ),
      
      
      shinydashboard::box(
        title = "SunburstPlot of the imported data:",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        shinycssloaders::withSpinner(sunburstR::sunburstOutput("sunburst", width = "100%", height = "800px")))
    ),
    ################################################################################
    #
    # Discrete Markov Chain content
    #
    ################################################################################
    # Third tab content
    shinydashboard::tabItem(
      tabName = "markov",
      shinydashboard::box(
        width = 12,
        title = "Initial settings",
        status = "primary",
        solidHeader = T,
      shiny::column(
        width = 4,
        shiny::checkboxGroupInput(
          inputId = "costDomainsDisc",
          label = h3("Select the cost domains included in analysis:"),
          choices = list(
            'Drug' = 'Drug',
            'Visit' = 'Visit',
            'Procedure' = 'Procedure',
            'Device' = 'Device',
            'Measurement' = 'Measurement',
            'Observation' = 'Observation',
            'Specimen' = 'Specimen'
          ),
          selected = c(
            'Drug',
            'Visit',
            'Procedure',
            'Device',
            'Measurement',
            'Observation',
            'Specimen'
          )
        )
      ),
      shiny::column(
        width = 4,
        shiny::uiOutput(outputId = "excludeStatesDisc")
      ),
      actionButton("markovAnalyseButton", "Analyze")
      ),
      shinydashboard::box(
        width = 12,
        title = "Stochastic transitions",
        status = "primary",
        solidHeader = T,
        shinycssloaders::withSpinner(DT::dataTableOutput('stochasticTransitions'))
      ),
      shinydashboard::box(
        width = 12,
        title = "Trajectory start statistics",
        status = "primary",
        solidHeader = T,
        shinycssloaders::withSpinner(DT::dataTableOutput('trajectoryStartStatistics')),
        shinycssloaders::withSpinner(shiny::textOutput("textMean")),
        shinycssloaders::withSpinner(shiny::textOutput("textMedian"))
      ),
      shinydashboard::box(
        width = 12,
        title = "State statistics (cost per 1 day)",
        status = "primary",
        solidHeader = T,
        shinycssloaders::withSpinner(DT::dataTableOutput('stateStatistics'))
      ),
      shinydashboard::box(
        width = 12,
        title = "State cost distributions",
        status = "primary",
        solidHeader = T,
        shinycssloaders::withSpinner(shiny::plotOutput("costDistPlotDisc"))
      ),
      shinydashboard::box(
        width = 12,
        title = "Probability of transitions in x transfers",
        status = "primary",
        solidHeader = T,
        shiny::numericInput(
          inputId = "transferCountInput",
          label = "Number of transfers:",
          value = 1,
          min = 1
        ),
        shiny::actionButton("transferCountButton", "Calculate"),
        shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "transferCountP"))
      )
    ),
    ################################################################################
    #
    # Profile content
    #
    ################################################################################
    # Fourth tab content
    shinydashboard::tabItem(
      tabName = "profiles",
      shiny::fluidRow(shiny::column(
        width = 11,
        shiny::textInput(
          inputId = "profiles_personIdInput",
          label = tags$div(
            HTML(
              '<i class="	fas fa-user-alt" style = "color:#00000;"></i><span style = "color:#00000"> Profiles </span>'
            )
          ),
          placeholder = "Person Id"
        )
      )),
      actionButton("profileSearchButton", "Search"),
      shinydashboard::tabBox(
        title = "Profile's overview",
        id = "tabsetProfile",
        width = "100%",
        shiny::tabPanel(title = "Information",
                        shiny::fluidRow(
                          shiny::column(
                            width = 12,
                            shinydashboard::box(
                              width = 12,
                              title = "Profile",
                              status = "primary",
                              solidHeader = T,
                              shinycssloaders::withSpinner(shinydashboard::infoBoxOutput("patientSex")),
                              shinycssloaders::withSpinner(shinydashboard::infoBoxOutput("patientAge")),
                              shinycssloaders::withSpinner(shinydashboard::infoBoxOutput("patientExists"))
                            ),
                          )
                          
                        )),
        shiny::tabPanel(
          title = "Markov States",
          shiny::fluidRow(shinycssloaders::withSpinner(shiny::plotOutput("patientPlot"))),
          shiny::column(
            width = 4,
            shiny::radioButtons(
              inputId = "markovStatePlotFeatures",
              label = h3("Plot y-axis feature"),
              choices = list(
                "No feature" = 0,
                "Cost analysis" = 1
              )
            )
          ),
          shiny::column(
            width = 8,
            shiny::radioButtons(
              "markovStatePlotType",
              label = h3("Plot type"),
              choices =
                c("Normal" = 1,
                  "Distinguishable" = 2),
              selected = 1
            ),
            shiny::numericInput(
              inputId = "markovStateDurationPlot",
              label = tags$div(
                HTML(
                  '<i class="	fas fa-user-cog" style = "color:#00000;"></i><span style = "color:#00000"> Length of allowed inactivity (days) </span>'
                )
              ),
              value = 30,
              min = 0
            )
          ),
          actionButton("markovAnalysePlotButton", "Refresh")
        )
      )
      
    ),
    ############################################################################
    #
    # Continuous markov chain tab content
    #
    ############################################################################
    shinydashboard::tabItem(
      tabName = "cmarkov",
      shiny::fluidPage(
        shinydashboard::box(
          width = 12,
          title = "Initial intensity matrix Markov model",
          status = "primary",
          solidHeader = T,
          uiOutput(outputId = "cmcInitialQ"),
          shiny::column(
            width = 4,
          shiny::checkboxGroupInput(
            inputId = "costDomainsCon",
            label = h3("Select the cost domains included in analysis:"),
            choices = list(
              'Drug' = 'Drug',
              'Visit' = 'Visit',
              'Procedure' = 'Procedure',
              'Device' = 'Device',
              'Measurement' = 'Measurement',
              'Observation' = 'Observation',
              'Specimen' = 'Specimen'
            ),
            selected = c(
              'Drug',
              'Visit',
              'Procedure',
              'Device',
              'Measurement',
              'Observation',
              'Specimen'
            )
          ))
          ,
          shiny::column(
            width = 4,
            shiny::uiOutput(outputId = "excludeStatesCon")
          ),
          shiny::actionButton("cmcButton", "Create CMC model")
        )
      ),
      shiny::fluidPage(
        shinydashboard::box(
          width = 12,
          title = "Calculated CMC intensity matrix",
          status = "primary",
          solidHeader = T,
          shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "cmcCalculatedQ"))
        )
      ),
      shiny::fluidPage(
        shinydashboard::box(
          width = 12,
          title = "Calculated CMC sojourn time",
          status = "primary",
          solidHeader = T,
          shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "cmcCalculatedSojourn"))
        )
      ),
      shiny::fluidPage(
        shinydashboard::box(
          width = 12,
          title = "Calculated CMC transition matrix",
          status = "primary",
          solidHeader = T,
          shiny::numericInput(
            inputId = "cmcTimeInput",
            label = "Transistion time in days:",
            value = 7,
            min = 0,
            step = 1
          ),
          shiny::actionButton("cmcTimeButton", "Calculate"),
          shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "cmcCalculatedP"))
        ),
        
        ### Here cost analysis
        shinydashboard::box(
          width = 12,
          title = "Trajectory start statistics",
          status = "primary",
          solidHeader = T,
          shinycssloaders::withSpinner(DT::dataTableOutput('trajectoryStartStatisticsCon'))
        ),
        shinydashboard::box(
          width = 12,
          title = "State statistics (cost per 1 day)",
          status = "primary",
          solidHeader = T,
          shinycssloaders::withSpinner(DT::dataTableOutput('stateStatisticsCon'))
        ),
        shinydashboard::box(
          width = 12,
          title = "State cost distributions",
          status = "primary",
          solidHeader = T,
          shinycssloaders::withSpinner(shiny::plotOutput("costDistPlotCon"))
        ),
        
        
      )
    ),
    ############################################################################
    #
    # Kaplan-Meyer analysis tab content
    #
    ############################################################################
    
    shinydashboard::tabItem(
      tabName = "kmplots",
      shiny::fluidPage(
        shinydashboard::box(
          width = 12,
          title = "Kaplan-Meier analysis",
          status = "primary",
          solidHeader = T,
          shiny::uiOutput(outputId = "kmStateChoices"),
          shiny::radioButtons(
            inputId = "kmAgeAnalysisInclusion",
            label = h3("Age analysis options"),
            choices = list("Do not include" = 0, "Include" = 1)
          ),
          shiny::uiOutput("kmAgeAnalysisIntervals"),
          shiny::uiOutput("kmRemoveAgeAnalysisIntervals"),
          shiny::uiOutput("inputsIntervals"),
          # shiny::radioButtons(
          #   inputId = "kmOtherAnalysisInclusion",
          #   label = h3("Other options"),
          #   choices = list("None" = 1, "Generated curve" = 2)
          # ),
          shiny::actionButton("kmPlotButton", "Plot"),
        ),
        shiny::fluidPage(
          shinydashboard::box(
            width = 12,
            title = "Kaplan-Meier plots",
            status = "primary",
            solidHeader = T,
            shinycssloaders::withSpinner(shiny::plotOutput("kmPlotOutput"))
          )
        )
      )
    ),
    
    ############################################################################
    #
    # Decision tree chain tab content
    #
    ############################################################################
    shinydashboard::tabItem(
      tabName = "dtrees",
      shiny::fluidPage(
        shinydashboard::box(
          width = 12,
          title = "DT architecture",
          status = "primary",
          solidHeader = T,
          shiny::actionButton("addLayerButton", "Add layer"),
          shiny::actionButton("rmLayerButton", "Remove layer"),
          shiny::actionButton("getLayersButton", "Show tree structure"),
          shiny::uiOutput("inputs")
        ),
        
        
        # Show a plot of the built tree
        shiny::fluidPage(
          shinydashboard::box(
            width = 12,
            title = "DT architecture",
            status = "primary",
            solidHeader = T,
            shinycssloaders::withSpinner(shiny::plotOutput("plotOut"))
          )
        ),
        # Import tab
        shiny::fluidPage(
          shinydashboard::box(
            width = 12,
            title = "Import",
            status = "primary",
            solidHeader = T,
            shiny::actionButton("generateTreeButton", "Generate on Data"),
            shiny::radioButtons(
              inputId = "dTreeProbabilityType",
              label = "Select probability type",
              choices = c(
                "Probability in target cohort" = 1,
                "Probability on layer level" = 2
  # TODO: Some bugs have emerged  
  #            "Exact trajectory probability in data" = 3
              )
            )
          ),
          shiny::fluidPage(
            shinydashboard::box(
              width = 12,
              title = "Probability tree",
              status = "primary",
              solidHeader = T,
              shinycssloaders::withSpinner(shiny::plotOutput("plotOut2"))
            )
          )
        )
      )
      
    ),
    ################################################################################
    #
    # Generation tab content
    #
    ################################################################################
    shinydashboard::tabItem(
      tabName = "generation",
      shiny::fluidPage(
        shinydashboard::box(
          width = 12,
          title = "Generation settings",
          status = "primary",
          solidHeader = T,
          shiny::numericInput(
            inputId = "generationNrPatients",
            label = "Number of patients",
            value = 100,
            min = 0
          ),
          dateRangeInput(
            inputId = "generationDateRange",
            label = "Date range",
            start = "2001-01-01",
            end   = "2010-12-31",
            format = "yyyy-mm-dd"
          ),
          shiny::numericInput(
            inputId = "generationStateDuration",
            label = "Duration patient spends in a state",
            value = 30,
            min = 0
          ),
          shiny::numericInput(
            inputId = "generationMaxOtherDuration",
            label = "Maximum days out of cohort",
            value = 183,
            min = 0
          ),
          shiny::radioButtons(
            inputId = "trajectoryTypeMatrix",
            label = "Matrix to use: state selection type",
            choices = c("Discrete time trajectory" = 1,
                        "Continuous time trajectory" = 2)
          ),
          shiny::radioButtons(
            inputId = "trajectoryGenerateCost",
            label = "Generate cost per state from observed data?",
            choices = c("No" = 0,
                        "Yes" = 1)
          ),
          
          shiny::actionButton("generationButton", "Generate")
        ),
        shinydashboard::box(
          width = 12,
          title = "Observed vs generated: (%) of occurrence",
          status = "primary",
          solidHeader = T,
          shinycssloaders::withSpinner(shiny::tableOutput(outputId = "generationCompareTable"))
          
        ),
        shinydashboard::box(
          width = 12,
          title = "Observed vs generated: LogRank tests for transfering between states",
          status = "primary",
          solidHeader = T,
          shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "generationLRMatrix"))
          
        )
      )
    ),
    # Seventh tab content
    # shinydashboard::tabItem(tabName = "publish",
    #                         shiny::fluidPage(
    #                           shinydashboard::box(
    #                             width = 12,
    #                             title = "Publishing settings",
    #                             status = "primary",
    #                             solidHeader = T,
    #                             shiny::actionButton("publishButton", "Publish")
    #                           )
    #                         )),
    # Eighth tab content
    shinydashboard::tabItem(
      tabName = "help",
      shinydashboard::tabBox(
        title = "Help",
        id = "helpTabset",
        width = "100%",
        shiny::tabPanel(
          title = "Description",
          shiny::includeMarkdown(path = "./www/shinyHelpDescription.md")
        ),
        shiny::tabPanel(
          title = "Import",
          shiny::includeMarkdown(path = "./www/shinyHelpImport.md")
        ),
        shiny::tabPanel(
          title = "Discrete time Markov model",
          shiny::includeMarkdown(path = "./www/shinyHelpDiscreteMarkovAnalysis.md")
        ),
        shiny::tabPanel(
          title = "Continuous time Markov model",
          shiny::includeMarkdown(path = "./www/shinyHelpContinuousMarkovAnalysis.md")
        ),
        shiny::tabPanel(
          title = "Profiles",
          shiny::includeMarkdown(path = "./www/shinyHelpProfiles.md")
        ),
        shiny::tabPanel(
          title = "Decision trees",
          shiny::includeMarkdown(path = "./www/shinyHelpDecisionTrees.md")
        ),
        shiny::tabPanel(
          title = "Generation",
          shiny::includeMarkdown(path = "./www/shinyHelpGeneration.md")
        ),
        # shiny::tabPanel(
        #   title = "Publish",
        #   shiny::includeMarkdown(path = "./www/shinyHelpPublish.md")
        # )
      )
    )
  )
)

################################################################################
#
# Shiny content
#
################################################################################

shinydashboard::dashboardPage(skin = "black", header, sidebar, body)
