#' Date Check for Water Quality Portal
#'
#' Checks date format for inputs to the Water Quality Portal. Used in \code{readWQPqw} 
#' and \code{readWQPdata}.
#'
#' @param values named list with arguments to send to the Water Quality Portal
#' @return values named list with corrected arguments to send to the Water Quality Portal
#' @export
#' @examples
#' values <- list(startDateLo = "01-01-2002",
#'                characteristicName = "Phosphorous", 
#'                endDate = as.Date("2014-01-01"))
#' values <- checkWQPdates(values)
checkWQPdates <- function(values) {
  
  dateNames <- c("startDateLo", "startDateHi",
                 "startDate", "endDate")
  
  if(any(names(values) %in% dateNames)) {
    index <- which(names(values) %in% dateNames)
    
    if("" %in% values[index]) {
      values <- values[-index[values[index] == ""]]
      index <- which(names(values) %in% dateNames)
    }
    
    if(length(index) > 0) {
      # If a valid R date was put in, the format needs to be changed to mm-dd-yyyy for the WQP:
      for(i in index) {
        dateInput <- as.character(values[[i]])
        splitDates <- unlist(strsplit(dateInput, "-"))
        if(length(splitDates) == 3) {
          if(nchar(splitDates[1]) == 4) { #R object
            dates <- as.Date(lubridate::parse_date_time(dateInput, "%Y-%m-%d"))
            dates <- format(dates, format = "%m-%d-%Y")
            values[i] <- dates
          } else if (nchar(splitDates[3]) != 4) { 
            #The way WQP wants it == 4, so this is probably a 2 digit year or something
            warning("Please check the date format for the arguments: ", 
                    paste(names(values)[i], values[i], collapse = ", "))
          }          
        } else { # Probably something wrong
          warning("Please check the date format for the arguments: ", 
                  paste(names(values)[i], values[i], collapse = ", "))
        }        
      }
      
      names(values)[names(values) == "startDate"] <- "startDateLo"
      names(values)[names(values) == "endDate"] <- "startDateHi"
    }
    
  }
  
  return(values)
}
