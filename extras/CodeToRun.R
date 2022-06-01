################################################################################
#
# Study settings
#
################################################################################
# devtools::install_github("HealthInformaticsUT/TrajectoryMarkovAnalysis")
library(TrajectoryMarkovAnalysis)
studyName = "Temp" # TODO
pathToResults <- getwd()   # TODO

################################################################################
#
# Database credentials
#
################################################################################
pathToDriver = './Drivers'
dbms <- "" #TODO
user <- '' #TODO
pw <- "" #TODO
server <- 'ip/database' #TODO
port <- '5432' #TODO

cdmSchema = "" #TODO # Schema which contains the OHDSI Common Data Model
cdmTmpSchema = "" #TODO # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema = "" #TODO # Schema which will contain the final results


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
################################################################################
#
# Run without GUI
#
################################################################################


# inputData = readr::read_csv("todo") # trajectory data
# modelType = "discrete" # "discrete" or "continuous"
# excludedStates = c()

# costDomains = c('Drug',
#                'Visit',
#                'Procedure',
#                'Device',
#                'Measurement',
#                'Observation',
#                'Specimen')

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
#  costDomains
#)


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
