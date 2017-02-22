#' 
#' Format and organize WQP arguments that are passed in as \code{...}.
#' 
#' @keywords internal
readWQPdots <- function(...){
  
  matchReturn <- c(do.call("c",list(...)[sapply(list(...), class) == "list"]), #get the list parts
                   list(...)[sapply(list(...), class) != "list"]) # get the non-list parts
  
  values <- sapply(matchReturn, function(x) as.character(paste(eval(x),collapse=";",sep="")))
  
  if("bBox" %in% names(values)){
    values['bBox'] <- gsub(pattern = ";", replacement = ",", x = values['bBox'])
  }
  
  values <- checkWQPdates(values)
  
  names(values)[names(values) == "siteNumber"] <- "siteid"
  names(values)[names(values) == "siteNumbers"] <- "siteid"
  
  names(values)[names(values) == "stateCd"] <- "statecode"
  if("statecode" %in% names(values)){
    stCd <- values["statecode"]
    stCdPrefix <- "US:"
    if(!grepl(stCdPrefix, stCd)){
      values["statecode"] <- paste0(stCdPrefix, stateCdLookup(stCd, "id"))
    }
  }
  
  names(values)[names(values) == "countyCd"] <- "countycode"
  if("countycode" %in% names(values)){
    if(!grepl(values["statecode"], values["countycode"])){
      stCd <- gsub("US:", "", values["statecode"])
      values["countycode"] <- paste0(values["statecode"], 
                                     countyCdLookup(stCd, values["countycode"], "id"))
    }
  }
  
  if("zip" %in% names(values)){
    if(class(values["zip"]) == "logical"){
      values["zip"] <- ifelse(values["zip"], "yes","no")
    }
  } else {
    values["zip"] <- "no"
  }
  
  return(values)
}
