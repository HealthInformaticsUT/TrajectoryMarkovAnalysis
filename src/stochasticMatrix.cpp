#include <Rcpp.h>
#include <limits>
#include <string>
#include <cstring>
#include <algorithm>
#include <cmath>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix stochasticMatrix(int matrixSize,
                               CharacterVector states,
                               DataFrame discreteTrajectories){
  
  // Initiate the transition matrix with zeros
  NumericMatrix M(matrixSize, matrixSize);
  // We need values of subject id and state
  
  NumericVector patientIDs = discreteTrajectories["SUBJECT_ID"];
  NumericVector stateClasses = discreteTrajectories["CLASS"];
  
  for (int i=1; i < patientIDs.size(); i++) {
    // # If we have the same subject we continue the movement between states
    if (patientIDs[i] == patientIDs[i - 1]) {
      // # We add +1 to the adjacency matrix on the appropriate coordinates
      M(stateClasses[i-1], stateClasses[i]) += 1;
    }
    // # If the subject changes we do not make any changes
  }
  
  // Adding row-and colnames
  rownames(M) = states;
  colnames(M) = states;
  return M;
}
