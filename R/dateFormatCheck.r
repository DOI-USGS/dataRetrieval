#' Check date format
#'
#' Checks to see if format is YYYY-MM-DD. Also performs a few other date checks.
#'
#' @param date string
#' @keywords WRTDS flow
#' @return condition logical if TRUE, 
#' @export
#' @examples
#' date <- '1985-01-01'
#' dateFormatCheck(date)
#' dateWrong <- '1999/1/7'
#' dateFormatCheck(dateWrong)
dateFormatCheck <- function(date){  # checks for the format YYYY-MM-DD
  parts <- strsplit(date,"-",fixed=TRUE)
  condition <- FALSE
  if (length(parts[[1]])>1) {
    if (nchar(parts[[1]][1]) == 4 && nchar(parts[[1]][2]) == 2 && nchar(parts[[1]][3]) == 2){
      testYear <- as.numeric(parts[[1]][1])
      testMonth <- as.numeric(parts[[1]][2])
      testDay <- as.numeric(parts[[1]][3])
      if (!is.na(testYear) && !is.na(testMonth) && !is.na(testDay)){
        if (testMonth <= 12 && testDay <= 31){
          condition <- TRUE
        }        
      }      
    }
  }
  return(condition)
}
