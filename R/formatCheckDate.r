#' Date Formatting Correction
#'
#' Checks that the user-supplied date is in the format YYYY-MM-DD. If not, asks the user to re-enter.
#'
#' @param Date string date to check
#' @param dateString string should be either 'StartDate' or 'EndDate'
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import from web service
#' @return Date string
#' @export
#' @examples
#' formatCheckDate('1985-01-01', 'StartDate')
formatCheckDate <- function(Date, dateString,interactive=TRUE){
  if(nzchar(Date)){
    if (!dateFormatCheck(Date)){
      if (interactive){
        cat("Date must be entered in the form YYYY-MM-DD (no quotes), you entered: ", Date, "as the StartDate.\n")
        cat("Please re-enter ", dateString, ":\n")
        Date <- readline()
      } else {
        warningMessage <- paste(dateString, " must be entered in the form YYYY-MM-DD, you entered: ", Date, ". ", dateString, " will be ignored",sep="")
        warning(warningMessage)
        Date <- ""
      }
    }
  }
  return(Date)
}