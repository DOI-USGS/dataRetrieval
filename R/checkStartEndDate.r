#' Check Start End Dates
#'
#' Checks that the user-supplied starting date is before the ending date.  If not, either the user can re-enter, or the dates will be set to maximum.
#'
#' @param StartDate string date
#' @param EndDate string date
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import from web service
#' @return list StartDate,EndDate
#' @export
#' @examples
#' checkStartEndDate('1985-01-01', '1985-12-31')
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