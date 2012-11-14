#' Convert Preloaded QW Data
#'
#' Imports data from data already pre-loaded into the R workspace. Specifically used to import water flow data for use in the WRTDS package.
#' For WRTDS usage, the first column is expected to be dates, the second column measured values.
#' The third column is optional, it contains any remark codes.
#'
#' @param rawData dataframe data already loaded into R workspace
#' @keywords data import
#' @return retval dataframe with dateTime, value, and code columns
getPreLoadedQWData <- function (rawData){  
  retval <- as.data.frame(rawData, stringsAsFactors=FALSE)
  numColumns <- ncol(retval)
  i <- 1
  while (i <= numColumns) {
    retval[[i]] <- as.character(retval[[i]])
    i <- i + 1
  }
  retval[[1]] <- as.character(retval[[1]])
  return (retval)
}