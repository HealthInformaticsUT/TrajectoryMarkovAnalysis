################################################################################
#
# Server content
#
################################################################################
server <- function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)
  ## Retrieve patients from selected cohorts
  v <-
    reactiveValues(#  studyDatabases = NULL,
      databaseDescriptions = NULL,
      databaseDiscreteMarkovChains = NULL)
  # Reading all the different instances of result databases
  studyDatabases <-
    list.dirs(
      path = paste(pathToResults, "/tmp/databases/", sep = ""),
      full.names = FALSE,
      recursive = FALSE
    )
  # Selecting active databases
  output$activeDatabases <- renderUI({
    shiny::selectInput(
      inputId = "activeDatabases",
      label = "Selected databases:",
      choices =  studyDatabases,
      multiple = TRUE,
      selected = studyDatabases[1]
    )
  })
  
  # Reading each of the databases' descriptions
  v$databaseDescriptions <-
    lapply(studyDatabases, function(db) {
      stringr::str_replace_all(readr::read_file(
        paste(
          pathToResults,
          "/tmp/databases/",
          db,
          "/description.md",
          sep = ""
        )
      ), pattern = "\n", "")
    })
  # Combining content for description tab
  output$databaseList <- renderUI({
    string = paste('<b>',
                   studyDatabases,
                   ': </b>',
                   v$databaseDescriptions,
                   sep = "")
    HTML(paste(string, collapse = "<br/><br/>"))
  })
  
  # Observing changes in selected databases' vector
  observeEvent(input$activeDatabases, {
    if (length(input$activeDatabases) == 0) {
      return()
    }
    else {
      # Reading each of the databases' Markov chains
      v$databaseDiscreteMarkovChains = lapply(input$activeDatabases, function(db) {
        get(load(
          paste(
            pathToResults,
            "/tmp/databases/",
            db,
            "/",
            db,
            "_discrete_transition_matrix.rdata",
            sep = ""
          )
        ))
      })
      
      # Creating plots of the matrices
      output$matrixHeatmaps <- renderPlot({
        plotsOut <- mapply(function(db, M) {
          if ("START" %in% colnames(M) & "EXIT" %in% colnames(M)) {
            col.order <-
              c("START", setdiff(sort(colnames(M)), c("START", "EXIT")), "EXIT")
          }
          else {
            col.order <- sort(colnames(M))
          }
          M <- M[col.order , col.order]
          ggplotify::as.grob(
            pheatmap::pheatmap(
              # # Matrix
              #  get(load(
              #  paste(pathToResults, "/databases/", db, "/Temp_discrete_transition_matrix.rdata", sep = "")
              # )),
              M,
              cluster_rows = F,
              cluster_cols = F,
              display_numbers = TRUE,
              fontsize_number = 15,
              number_format = '%.4f',
              number_color = 'black',
              color = grDevices::colorRampPalette(c('#FFFFFF', '#39ff14'))(100),
              main = db,
              legend = FALSE
            )
          )
        },
        input$activeDatabases,
        v$databaseDiscreteMarkovChains)
        return(gridExtra::grid.arrange(grobs = plotsOut,
                                       ncol = 2))
      })
      
      
      # Reading in sunburst plots' HTML
      
      getPlot <- function(db) {
        sunburstDetails  = get(load(paste(
          pathToResults,
          paste("/tmp/databases/",
                db,
                "/",
                db,
                "sunburst.rdata",
                sep = ""),
          sep = ""
        )))
        
        plot <- sunburstR::sunburst(
          sunburstDetails$freqPaths,
          count = TRUE,
          colors = list(
            range = c(sunburstDetails$colors, "#cccccc", "#cccccc"),
            domain = c(sunburstDetails$labels, "OUT OF COHORT", "End")
          ),
          legend = list(w = 200, h = 20, s = 5),
          breadcrumb = htmlwidgets::JS(("function(x) {return x;}")),
          height = "400px",
          width = "100%"
        )
        return(sunburstR::add_shiny(plot))
      }
      
      
      output$sunburstPlots <- renderUI({
        # plot_output_list <- lapply(input$activeDatabases, getPlot)
        #
        # tagList(plot_output_list)
        
        
        tagList(lapply(1:length(input$activeDatabases), function(i) {
          shinydashboard::box(
            width = 6,
            title = input$activeDatabases[i],
            status = "primary",
            solidHeader = TRUE,
            getPlot(input$activeDatabases[i])
          )
        }))
      })
      
      output$costBarPlots <- renderPlot({
        costData <- data.frame()
        for (db in input$activeDatabases) {
          tmpData = read.delim(paste(
            pathToResults,
            paste(
              "/tmp/databases/",
              db,
              "/",
              db,
              "_state_statistics.txt",
              sep = ""
            ),
            sep = ""
          ),
          sep = ",")
          tmpData$dbs = db
          costData = rbind(costData, tmpData)
        }
        colnames(costData) <-
          c(
            "state",
            "perc",
            "mean_charge",
            "ci_charge",
            "mean_cost",
            "ci_cost",
            "mean_paid",
            "ci_paid",
            "dbs"
          )
        costData$perc = round(costData$perc, 3)
        costData$mean_charge = round(costData$mean_charge, 2)
        costData$mean_cost = round(costData$mean_cost, 2)
        costData$mean_paid = round(costData$mean_paid, 2)
        
        p_charge <-
          ggplot2::ggplot(costData,
                          ggplot2::aes(
                            fill = dbs,
                            y = mean_charge,
                            x = dbs
                          )) +
          ggplot2::geom_bar(position = "dodge",
                            stat = "identity",
                            width = 1) +
          ggplot2::ylim(0, 1.3 * max(costData$mean_charge)) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(mean_charge, '€')),
            position = ggplot2::position_dodge(width = 0.5),
            vjust = -1.25,
            size = 5,
            na.rm = TRUE
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(perc * 100, '%')),
            position = ggplot2::position_dodge(width = 1),
            vjust = -.25,
            size = 5,
            na.rm = TRUE
          ) +
          ggplot2::facet_wrap( ~ state) +
          ggplot2::theme_minimal() +
          ggplot2::ylab("Avg charge") +
          ggplot2::xlab("") +
          # ggplot2::geom_line(ggplot2::aes(
          #   x = dbs,
          #   y = mean_charge,
          #   group = 1
          # ), na.rm = TRUE) +
          ggplot2::guides(fill = ggplot2::guide_legend(title = "Database"),
                          size = NULL) + ggplot2::theme(
                            text = ggplot2::element_text(size = 20),
                            axis.text.x = ggplot2::element_blank(),
                            axis.ticks.x = ggplot2::element_blank()
                          )
        p_cost <-
          ggplot2::ggplot(costData, ggplot2::aes(
            fill = dbs,
            y = mean_cost,
            x = dbs
          )) +
          ggplot2::geom_bar(position = "dodge",
                            stat = "identity",
                            width = 1) +
          ggplot2::ylim(0, 1.3 * max(costData$mean_cost)) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(mean_cost, '€')),
            
            position = ggplot2::position_dodge(width = 0.5),
            vjust = -1.25,
            size = 5,
            na.rm = TRUE
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(perc * 100, '%')),
            position = ggplot2::position_dodge(width = 1),
            vjust = -.25,
            size = 5,
            na.rm = TRUE
          ) +
          ggplot2::facet_wrap( ~ state) +
          ggplot2::theme_minimal() +
          ggplot2::ylab("Avg cost") +
          ggplot2::xlab("") +
          # ggplot2::geom_line(ggplot2::aes(
          #   x = dbs,
          #   y = mean_cost,
          #   group = 1
          # ), na.rm = TRUE) +
          ggplot2::guides(fill = ggplot2::guide_legend(title = "Database"),
                          size = NULL) + ggplot2::theme(text = ggplot2::element_text(size = 20),
                                                        axis.text.x = ggplot2::element_blank(),
                                                        axis.ticks.x = ggplot2::element_blank())
        p_paid <-
          ggplot2::ggplot(costData,
                          ggplot2::aes(
                            fill = dbs,
                            y = mean_paid,
                            x = as.factor(dbs)
                          )) +
          ggplot2::geom_bar(position = "dodge",
                            stat = "identity",
                            width = 1) +
          ggplot2::ylim(0, 1.3 * max(costData$mean_paid)) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(mean_paid, '€')),
            position = ggplot2::position_dodge(width = 0.5),
            vjust = -1.25,
            size = 5
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(perc * 100, '%')),
            position = ggplot2::position_dodge(width = 1),
            vjust = -.25,
            size = 5
          ) +
          ggplot2::facet_wrap( ~ state) +
          ggplot2::theme_minimal() +
          ggplot2::ylab("Avg paid") +
          ggplot2::xlab("") +
          # ggplot2::geom_line(ggplot2::aes(
          #   x = as.factor(dbs),
          #   y = mean_paid,
          #   group = 1
          # ), na.rm = TRUE) +
          ggplot2::guides(fill = ggplot2::guide_legend(title = "Database"),
                          size = NULL) + ggplot2::theme(text = ggplot2::element_text(size = 20),
                                                        axis.text.x = ggplot2::element_blank(),
                                                        axis.ticks.x = ggplot2::element_blank())
        p <- ggpubr::ggarrange(
          p_cost,
          p_charge,
          p_paid,
          labels = c("Cost", "Charge", "Paid"),
          ncol = 1
        )
        return(p)
        
      })
      
      
      output$trajectoryStartPlots <- renderPlot({
        startData <- data.frame()
        for (db in input$activeDatabases) {
          tmpData = read.delim(paste(
            pathToResults,
            paste(
              "/tmp/databases/",
              db,
              "/",
              db,
              "_first_state_statistics.txt",
              sep = ""
            ),
            sep = ""
          ),
          sep = ",")
          tmpData$dbs = db
          startData = rbind(startData, tmpData)
        }
        colnames(startData) <-
          c("state", "perc", "charge", "cost", "paid", "dbs")
        startData$perc = round(startData$perc, 3)
        startData$charge = round(startData$charge, 2)
        startData$cost = round(startData$cost, 2)
        startData$paid = round(startData$paid, 2)
        
        p_cost <-
          ggplot2::ggplot(startData, ggplot2::aes(
            fill = state,
            y = cost,
            x = dbs
          )) +
          ggplot2::geom_bar(position = "dodge",
                            stat = "identity",
                            width = 0.5) +
          ggplot2::ylim(0, 1.3 * max(startData$cost)) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(cost, '€')),
            position = ggplot2::position_dodge(width = 0.5),
            vjust = -1.25,
            size = 5
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(perc * 100, '%')),
            position = ggplot2::position_dodge(width = 0.5),
            vjust = -0.25,
            size = 5
          ) +
          ggplot2::theme_minimal() +
          ggplot2::ylab("Avg trajectory cost") +
          ggplot2::xlab("Database") +
          ggplot2::guides(fill = ggplot2::guide_legend(title = "First state")) + ggplot2::theme(text = ggplot2::element_text(size = 20))
        p_charge <-
          ggplot2::ggplot(startData, ggplot2::aes(
            fill = state,
            y = charge,
            x = dbs
          )) +
          ggplot2::geom_bar(position = "dodge",
                            stat = "identity",
                            width = 0.5) +
          ggplot2::ylim(0, 1.3 * max(startData$charge)) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(charge, '€')),
            position = ggplot2::position_dodge(width = 0.5),
            vjust = -1.25,
            size = 5
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(perc * 100, '%')),
            position = ggplot2::position_dodge(width = 0.5),
            vjust = -0.25,
            size = 5
          ) +
          ggplot2::theme_minimal() +
          ggplot2::ylab("Avg trajectory charge") +
          ggplot2::xlab("Database") +
          ggplot2::guides(fill = ggplot2::guide_legend(title = "First state")) + ggplot2::theme(text = ggplot2::element_text(size = 20))
        p_paid <-
          ggplot2::ggplot(startData, ggplot2::aes(
            fill = state,
            y = paid,
            x = dbs
          )) +
          ggplot2::geom_bar(position = "dodge",
                            stat = "identity",
                            width = 0.5) +
          ggplot2::ylim(0, 1.3 * max(startData$paid)) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(paid, '€')),
            position = ggplot2::position_dodge(width = 0.5),
            vjust = -1.25,
            size = 5
          )+
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(perc * 100, '%')),
            position = ggplot2::position_dodge(width = 0.5),
            vjust = -0.25,
            size = 5
          ) +
          ggplot2::theme_minimal() +
          ggplot2::ylab("Avg trajectory paid") +
          ggplot2::xlab("Database") +
          ggplot2::guides(fill = ggplot2::guide_legend(title = "First state")) + ggplot2::theme(text = ggplot2::element_text(size = 20))
        
        p <- ggpubr::ggarrange(
          p_cost,
          p_charge,
          p_paid,
          ncol = 1
        )
        return(p)
      })
      
      
      output$costDistPlot <- renderPlot({
        startData <- data.frame()
        for (db in input$activeDatabases) {
          tmpData = read.csv(paste(
            pathToResults,
            paste(
              "/tmp/databases/",
              db,
              "/",
              db,
              "_state_cost.csv",
              sep = ""
            ),
            sep = ""
          ),
          sep = ",")
          tmpData$dbs = db
          startData = rbind(startData, tmpData)
        }
        colnames(startData) <-
          c(
            "state",
            "charge1day",
            "cost1day",
            "paid1day",
            "person_id",
            "total_charge",
            "dbs"
          )
        
        startData = dplyr::summarise(dplyr::group_by(startData, person_id, dbs),
                                     total_charge = sum(total_charge))
        
        p = ggplot2::ggplot(startData, ggplot2::aes(x = total_charge, fill = dbs)) + ggplot2::geom_density(alpha = 0.4) + ggplot2::xlim(0,
                                                                                                                                        mean(startData$total_charge) + 1.96 * 2 * sd(startData$total_charge)) + ggplot2::labs(fill = 'Database') + ggplot2::xlab('Total charge') + ggplot2::ylab('Density') + ggplot2::theme_bw() + ggplot2::theme(text = ggplot2::element_text(size = 20))
        return(p)
      })
    }
  })
}
