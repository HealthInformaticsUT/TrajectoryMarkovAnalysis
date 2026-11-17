# Run the supported, synthetic example after installing this checkout.
# Installation in R, with the repository as the current directory:
# install.packages(".", repos = NULL, type = "source")
# Restart R after installing over a loaded package.

example_file <- system.file(
  "examples", "quickstart.R", package = "TrajectoryMarkovAnalysis"
)
if (!nzchar(example_file)) {
  stop("Install the current TrajectoryMarkovAnalysis checkout first.")
}
source(example_file)
