#' checkStartEndDate
#'
#' Checks that the start date is before the end date.  If not, it will give the user the opportunity to correct, otherwise will create a warning.
#'
#' @param StartDate string
#' @param EndDate string
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords WRTDS flow
#' @return vector where first value is StartDate, second is EndDate
#' @export
#' @examples
#' startDate <- '1985-01-01'
#' endDate <- '1990-01-01'
#' checkStartEndDate(startDate, endDate)
checkStartEndDate <- function(StartDate, EndDate,interactive=TRUE){
  start <- as.Date("1850-01-01")
  end <- as.Date(Sys.Date())
  
  if (nzchar(StartDate)) start <- as.Date(StartDate)
  if (nzchar(EndDate)) end <- as.Date(EndDate)
  if (start > end) {
    if (interactive){
      cat ("Start date must be before end date, you entered Start = ", StartDate, " End = ", EndDate, "\n")
      cat ("please re-enter StartDate (YYYY-MM-DD) - hit Enter for earliest date as StartDate: \n")
      StartDate <- readline()
      cat("Please re-enter EndDate (YYYY-MM-DD) - hit Enter for latest date as EndDate: \n")
      EndDate <- readline()
    } else {
      warningMessage <- "Starting date was not before ending date, dates will be ignored"
      warning(warningMessage)
      StartDate <- as.Date("1851-01-01")
      EndDate <- as.Date(Sys.Date())
    }
    
  }  
  return(c(StartDate,EndDate))
}
