################################################################################
#
# Header content
#
################################################################################
header <-
  shinydashboard::dashboardHeader(title = "TMA results dashboard",
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
sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Description", tabName = "description", icon = icon("home")),
    shinydashboard::menuItem("Matrixes", tabName = "matrixes", icon = icon("square-o")),
    shinydashboard::menuItem("Sunburst plots", tabName = "sunbursts", icon = icon("sun")),
    shinydashboard::menuItem("State costs", tabName = "statecosts", icon = icon("bar-chart"))
  ),
  shiny::uiOutput("activeDatabases")
)

################################################################################
#
# Body content
#
################################################################################

body <- shinydashboard::dashboardBody(
  id = "shinyBody",
  shinydashboard::tabItems(
    shinydashboard::tabItem(
      tabName = "description",
      shinycssloaders::withSpinner(shiny::htmlOutput("databaseList"))
    ),
    shinydashboard::tabItem(
      tabName = "matrixes",
      shinycssloaders::withSpinner(shiny::plotOutput("matrixHeatmaps"))
    ),
    shinydashboard::tabItem(
      tabName = "sunbursts",
      shinycssloaders::withSpinner(shiny::uiOutput("sunburstPlots")),
      # shinycssloaders::withSpinner(sunburstR::sunburstOutput("sunburstPlots", width = "100%", height = "800px"))
    ),
    shinydashboard::tabItem(
      tabName = "statecosts",
      shiny::tabsetPanel(
      # tabName = "statecosts",
      # shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "State cost statistics",
          shinycssloaders::withSpinner(
            shiny::plotOutput("costBarPlots", width = "100%", height = "1000px")
          )
        ),
        shiny::tabPanel(
          "Trajectory head statistics",
          shinycssloaders::withSpinner(
            shiny::plotOutput("trajectoryStartPlots", width = "100%", height = "1000px")
          ),
          shinycssloaders::withSpinner(
            shiny::plotOutput("costDistPlot", width = "100%", height = "1000px")
          )
        )
      #   # shinycssloaders::withSpinner(sunburstR::sunburstOutput("sunburstPlots", width = "100%", height = "800px"))
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
