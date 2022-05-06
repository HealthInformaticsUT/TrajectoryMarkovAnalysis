################################################################################
#
# Study settings
#
################################################################################
# devtools::install_github("HealthInformaticsUT/TrajectoryMarkovAnalysis")
library(TrajectoryMarkovAnalysis)
studyName = "todo" 
pathToResults = getwd()

################################################################################
#
# Database credentials
#
################################################################################

pathToDriver = './Drivers'
dbms <- "todo"
user <- 'todo'
pw <-"todo"
server <- "todo/todo" #'ip/database'
port <-  'todo' #'5432'

cdmSchema = "todo" # Schema which contains the OHDSI Common Data Model
cdmTmpSchema = "todo" # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema = "todo" # Schema which will contain the final results

################################################################################
#
# Initiate the database connection
#
################################################################################

connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    server = server,
    user = user,
    password = pw,
    port = port,
    pathToDriver = pathToDriver
  )

conn <- DatabaseConnector::connect(connectionDetails)

################################################################################
#
# Start the GUI application
#
################################################################################
runGUI(
  connection = conn,
  connectionDetails = connectionDetails,
  pathToDriver = pathToDriver,
  dbms = dbms,
  cdmSchema = cdmSchema,
  cdmTmpSchema = cdmTmpSchema,
  cdmResultsSchema = cdmResultsSchema,
  studyName = studyName,
  pathToResults = pathToResults
)




