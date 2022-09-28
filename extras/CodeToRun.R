################################################################################
#
# Study settings
#
################################################################################
# devtools::install_github("HealthInformaticsUT/TrajectoryMarkovAnalysis@v1.0.0") # Run for installing release v1.0.0
# devtools::install_github("HealthInformaticsUT/TrajectoryMarkovAnalysis") # Run for installing the HEAD
library(TrajectoryMarkovAnalysis)
studyName = "Temp" # TODO
pathToResults <- getwd()   # TODO

################################################################################
#
# Database credentials
#
################################################################################
pathToDriver = './Drivers'
dbms <- 'postgresql' #TODO
user <- '' #TODO
pw <- "" #TODO
server <- 'ip/database' #TODO
port <- '5432' #TODO

cdmSchema <- "ohdsi_cdm" #TODO # Schema which contains the OHDSI Common Data Model
cdmTmpSchema <- "ohdsi_temp" #TODO # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema <- "ohdsi_results" #TODO # Schema which will contain the final results

databaseDescription <- "This is a very cool database." #TODO


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
  pathToResults = pathToResults,
  databaseDescription = databaseDescription
)
################################################################################
#
# Run without GUI
#
################################################################################


# inputData <- readr::read_csv("/home/...") # trajectory data
# modelType <- "discrete" # "discrete" or "continuous"
# excludedStates <- c("OUT OF COHORT")
# 
# costDomains <- c(
#                'Drug',
#                'Visit',
#                'Procedure',
#                'Device',
#                'Measurement',
#                'Observation',
#                'Specimen'
#                )
# 
# TrajectoryMarkovAnalysis(
#  conn,
#  dbms,
#  cdmSchema,
#  cdmTmpSchema,
#  inputData,
#  modelType,
#  studyName,
#  pathToResults,
#  excludedStates,
#  costDomains,
#  databaseDescription
# )



################################################################################
#
# Data generation
#
################################################################################

# Discrete trajectories
# transistionMatrix = get(load(paste(pathToResults,"/tmp/models/todo" ,sep = "")))

# generateDataDiscrete(transitionMatrix = transistionMatrix,
#                                n = 100, # TODO : Number of patients
#                                minDate = "1900-01-01",
#                                maxDate = "2021-12-31",
#                                maxOut = 365, # TODO : Maximum days out of cohort
#                                stateDuration = 30, # TODO : state duration (time in days)
#                                pathToResults = getwd(),
#                                studyName = studyName)



# Continuous trajectories
# intensityMatrix = get(load(paste(pathToResults,"/tmp/models/todo" ,sep = "")))

# generateDataContinuous(model = intensityMatrix,
#                      n = 100,  # TODO : Number of patients
#                      minDate = "1900-01-01",
#                      maxDate = "2021-12-31",
#                      pathToResults = pathToResults,
#                      studyName = studyName)


################################################################################
#
# Run dashboard
#
################################################################################

# The pathToResults variable should point to the directory with subfolders tmp/databases/... which will be created as the result of running TrajectoryMarkovanalysis package. 

runDashboard(pathToResults = pathToResults)
