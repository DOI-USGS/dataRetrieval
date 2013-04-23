#' USGS Mulitple Parameter List
#'
#' Imports a table of information on a set of parameters such as parameter name, units, group, and srs name.
#' Warning! This function can be very slow because an individual web service call has to be made for each parameter.
#' There is currently no way to request multiple parameters from the web service and get the extended information.
#' 
#' @param pCodes vector set of 5-digit parameter codes to gather information on
#' @param interactive logical Option for interactive mode.  If true, a progress indicator is printed to the console.
#' @keywords data import USGS web service
#' @return retval dataframe with all information found in the expanded site file
#' @export
#' @examples
#' # These examples require an internet connection to run
#' availableData <- getMultipleParameterNames(c("00060", "00065", "00010"),interactive=FALSE)
getMultipleParameterNames <- function(pCodes, interactive=TRUE){
  
  numObs <- length(pCodes)
  printUpdate <- floor(seq(1,numObs,(numObs-1)/100))
  if(interactive) cat("Percent complete: \n")
  for (i in 1:numObs){
    if (1 == i) {
      pcodeINFO <- getParameterInfo(pCodes[i])
    } else {
      pcodeINFO <- rbind(pcodeINFO, getParameterInfo(pCodes[i]))
    }
    if(interactive) {
      
      if(i %in% printUpdate) cat(floor(i*100/numObs),"\t")
    }
  }
  return(pcodeINFO)
}
