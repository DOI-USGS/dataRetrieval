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