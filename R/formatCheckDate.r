#' formatCheckDate 
#'
#' Response to the date format checker.  If the date is not formated correctly, it will give the user the opportunity to correct, otherwise will create a warning.
#'
#' @param Date string
#' @param dateString string used in either error message or interactive message. An example would be "startDate"
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords WRTDS flow
#' @return condition logical if TRUE, 
#' @export
#' @examples
#' Date <- '1985-01-01'
#' dateString <- 'startDate'
#' formatCheckDate(Date, dateString, interactive = FALSE)
formatCheckDate <- function(Date, dateString,interactive=TRUE){
  if(nzchar(Date)){
    if (!dateFormatCheck(Date)){
      if (interactive){
        cat("Date must be entered in the form YYYY-MM-DD (no quotes), you entered: ", Date, "as the startDate.\n")
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
