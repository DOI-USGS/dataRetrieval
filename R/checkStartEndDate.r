#' checkStartEndDate
#'
#' Checks that the start date is before the end date.  If not, it will give the user the opportunity to correct, otherwise will create a warning.
#'
#' @param startDate string
#' @param endDate string
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords WRTDS flow
#' @return vector where first value is startDate, second is endDate
#' @export
#' @examples
#' startDate <- '1985-01-01'
#' endDate <- '1990-01-01'
#' checkStartEndDate(startDate, endDate)
checkStartEndDate <- function(startDate, endDate,interactive=TRUE){
  start <- as.Date("1850-01-01")
  end <- as.Date(Sys.Date())
  
  if (nzchar(startDate)) start <- as.Date(startDate)
  if (nzchar(endDate)) end <- as.Date(endDate)
  if (start > end) {
    if (interactive){
      cat ("Start date must be before end date, you entered Start = ", startDate, " End = ", endDate, "\n")
      cat ("please re-enter startDate (YYYY-MM-DD) - hit Enter for earliest date as startDate: \n")
      startDate <- readline()
      cat("Please re-enter endDate (YYYY-MM-DD) - hit Enter for latest date as endDate: \n")
      endDate <- readline()
    } else {
      warningMessage <- "Starting date was not before ending date, dates will be ignored"
      warning(warningMessage)
      startDate <- as.Date("1851-01-01")
      endDate <- as.Date(Sys.Date())
    }
    
  }  
  return(c(startDate,endDate))
}
