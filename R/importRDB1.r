
#' Function to return data from the NWIS RDB 1.0 format
#'
#' This function accepts a url parameter that already contains the desired
#' NWIS site, parameter code, statistic, startdate and enddate. It is not
#' recommended to use the RDB format for importing multi-site data. 
#'
#' @param obs_url character containing the url for the retrieval or a file path to the data file.
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, Date
#' @param tz character to set timezone attribute of datetime. Default converts the datetimes to UTC 
#' (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Recommended US values include "UTC","America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla".
#' For a complete list, see \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones}
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' datetime \tab POSIXct \tab The date and time of the value converted to UTC (if asDateTime = \code{TRUE}), \cr 
#' \tab character \tab or raw character string (if asDateTime = FALSE) \cr
#' tz_cd \tab character \tab The time zone code for datetime \cr
#' code \tab character \tab Any codes that qualify the corresponding value\cr
#' value \tab numeric \tab The numeric value for the parameter \cr
#' tz_cd_reported \tab The originally reported time zone \cr
#' }
#' Note that code and value are repeated for the parameters requested. The names are of the form 
#' XD_P_S, where X is literal, 
#' D is an option description of the parameter, 
#' P is the parameter code, 
#' and S is the statistic code (if applicable).
#' If a date/time (dt) column contained incomplete date and times, a new column of dates and time was inserted. This could happen
#' when older data was reported as dates, and newer data was reported as a date/time.
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' comment \tab character \tab Header comments from the RDB file \cr
#' }
#' @export
#' @import utils
#' @import stats
#' @import lubridate
#' @importFrom dplyr left_join
#' @importFrom readr read_lines
#' @importFrom readr read_delim
#' @importFrom readr problems
#' @examples
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- "00003"
#' property <- "00060"
#' 
#' obs_url <- constructNWISURL(siteNumber,property,
#'          startDate,endDate,"dv",format="tsv")
#' \dontrun{
#' data <- importRDB1(obs_url)
#' 
#' urlMultiPcodes <- constructNWISURL("04085427",c("00060","00010"),
#'          startDate,endDate,"dv",statCd=c("00003","00001"),"tsv")
#' multiData <- importRDB1(urlMultiPcodes)
#' unitDataURL <- constructNWISURL(siteNumber,property,
#'          "2013-11-03","2013-11-03","uv",format="tsv") #includes timezone switch
#' unitData <- importRDB1(unitDataURL, asDateTime=TRUE)
#' qwURL <- constructNWISURL(c('04024430','04024000'),
#'           c('34247','30234','32104','34220'),
#'          "2010-11-03","","qw",format="rdb") 
#' qwData <- importRDB1(qwURL, asDateTime=TRUE, tz="America/Chicago")
#' iceSite <- '04024000'
#' start <- "2015-11-09"
#' end <- "2015-11-24"
#' urlIce <- constructNWISURL(iceSite,"00060",start, end,"uv",format="tsv")
#' ice <- importRDB1(urlIce, asDateTime=TRUE)
#' iceNoConvert <- importRDB1(urlIce, convertType=FALSE)
#' }
#' # User file:
#' filePath <- system.file("extdata", package="dataRetrieval")
#' fileName <- "RDB1Example.txt"
#' fullPath <- file.path(filePath, fileName)
#' importUserRDB <- importRDB1(fullPath)
#' 
importRDB1 <- function(obs_url, asDateTime=TRUE, convertType = TRUE, tz="UTC"){
  
  if(tz == ""){
    tz <- "UTC" 
  }
  
  tz <- match.arg(tz, OlsonNames())

  if(file.exists(obs_url)){
    doc <- obs_url
  } else {
    doc <- getWebServiceData(obs_url, encoding='gzip')
    if("warn" %in% names(attr(doc,"header"))){
      data <- data.frame()
      attr(data, "header") <- attr(doc,"header")
      attr(data, "url") <- obs_url
      attr(data, "queryTime") <- Sys.time()
      
      return(data)
    }
  }
  
  readr.total <- read_lines(doc)
  total.rows <- length(readr.total)
  readr.meta <- readr.total[grep("^#", readr.total)]
  meta.rows <- length(readr.meta)
  header.names <- strsplit(readr.total[meta.rows+1],"\t")[[1]]
  types.names <- strsplit(readr.total[meta.rows+2],"\t")[[1]]
  
  if(convertType){
    readr.data <- suppressWarnings(read_delim(doc, skip = (meta.rows+2),delim="\t",col_names = FALSE))
  #defaults to time in seconds in readr 0.2.2.9??  
    if(length(grep("hms",lapply(readr.data, class))) > 0){
      colHMS <- grep("hms",lapply(readr.data, class))
      
      colList <- as.list(rep("c",length(colHMS)))
      names(colList) <- paste0("X",colHMS)

      readr.data <- suppressWarnings(read_delim(doc, skip = (meta.rows+2),delim="\t",
                                                col_names = FALSE, 
                                                col_types = colList))
    }

  } else {
    readr.data <- read_delim(doc, skip = (meta.rows+2),delim="\t",col_names = FALSE, col_types = cols(.default = "c"))
  }
  
  if(nrow(readr.data) > 0){
    names(readr.data) <- header.names
    
    char.names <- c(header.names[grep("_cd",header.names)],
                    header.names[grep("_id",header.names)],
                    header.names[header.names == "site_no"])
    
    if(length(char.names) > 0){
      char.names.true <- char.names[sapply(readr.data[,char.names], is.character)]
      char.names <- char.names[!(char.names %in% char.names.true)]
    } else {
      char.names <- NULL
    } 
    
    if(nrow(problems(readr.data)) > 0 | length(char.names) > 0){
      readr.data.char <- read_delim(doc, skip = (meta.rows+2),delim="\t",col_names = FALSE, 
                                    col_types = cols(.default = "c"))
      names(readr.data.char) <- header.names    
    }
    
    for(j in char.names){
      readr.data[,j] <- readr.data.char[[j]]
      attr(readr.data, "problems")  <- attr(readr.data, "problems")[attr(readr.data, "problems")[["col"]] != paste0("X",j),]
    }
  
    badCols <- attr(readr.data, "problems")[["col"]]  
    readr.data <- as.data.frame(readr.data)
    
    if(length(badCols) > 0){
      readr.data <- fixErrors(readr.data, readr.data.char, "no trailing characters", as.numeric)
      readr.data <- fixErrors(readr.data, readr.data.char, "date like", parse_date_time, c("%Y-%m-%d %H:%M:%S","%Y-%m-%d","%Y"))
    }
    
    if(length(grep("_va", names(readr.data))) > 0  && 
       any(lapply(readr.data[,grep("_va", names(readr.data))], class) %in% "integer")){ 
      #note... if we simply convert any _va to numeric...we lose some QW censoring information from some formats
      vaCols <- grep("_va", names(readr.data))
      
      if(length(vaCols) > 1){
        vaCols <- vaCols[lapply(readr.data[,vaCols], class) %in% "integer"]
      } 
      
      readr.data[,vaCols] <- sapply(readr.data[,vaCols], as.numeric)
    }
  
    columnsThatMayBeWrong <- grep("n",types.names)[which(!(sapply(readr.data[,grep("n",types.names)], typeof) %in% c("double","integer")))]

    for(i in columnsThatMayBeWrong){
      readr.data[[i]] <- tryCatch({
          test_column <- as.numeric(readr.data[[i]])
        },
        warning=function(cond) {
          test_column <- readr.data[[i]]
          message(paste("Column",i,"contains characters that cannot be automatically converted to numeric."))
        },
        finally={
          test_column
        })
    }
    
    comment(readr.data) <- readr.meta
    problems.orig <- problems(readr.data)
    
    
    if (asDateTime & convertType){
  
      header.suffix <- sapply(strsplit(header.names,"_"), function(x)x[length(x)])
      header.base <- substr(header.names,1,nchar(header.names)-3)
      
      for(i in unique(header.base[header.suffix %in% c("dt","tm")])){
        
        if(all(c(paste0(i,"_dt"),paste0(i,"_tm")) %in% header.names)){
          varname <- paste0(i,"_dateTime")
          varval <- suppressWarnings(parse_date_time(paste(readr.data[,paste0(i,"_dt")],readr.data[,paste0(i,"_tm")]), c("%Y-%m-%d %H:%M:%S","%Y-%m-%d %H:%M"), tz = "UTC"))
        
          if(!all(is.na(varval))){
            readr.data[,varname] <- varval
            tz.name <- paste0(i,"_time_datum_cd")
            
            if(tz.name %in% header.names){
              readr.data <- convertTZ(readr.data,tz.name,varname,tz)
            }
            
            tz.name <- paste0(i,"_tz_cd")
            
            if(tz.name %in% header.names){
              readr.data <- convertTZ(readr.data,tz.name,varname,tz)
            }
          }
        }
      }
      
      if("tz_cd" %in% header.names){
        date.time.cols <- which(sapply(readr.data, function(x) inherits(x, "POSIXct")))
        if(length(date.time.cols) > 0){
          readr.data <- convertTZ(readr.data,"tz_cd",date.time.cols,tz, flip.cols=FALSE)
        }
      }
      
      if("DATE" %in% header.names){
        readr.data[,"DATE"] <- parse_date_time(readr.data[,"DATE"], "Ymd")
      }
      
      if(all(c("DATE","TIME","TZCD") %in% header.names)){
        varname <- "DATETIME"
        varval <- as.POSIXct(fast_strptime(paste(readr.data[,"DATE"],readr.data[,"TIME"]), "%Y-%m-%d %H%M%S", tz = "UTC"))
        readr.data[,varname] <- varval
        readr.data <- convertTZ(readr.data,"TZCD",varname,tz, flip.cols=TRUE)
      }
      
      if("sample_start_time_datum_cd" %in% header.names){
        readr.data <- convertTZ(readr.data,"sample_start_time_datum_cd","sample_dateTime",tz)
        
        if(!("sample_end_time_datum_cd" %in% header.names) & "sample_end_dateTime" %in% names(readr.data)){
          readr.data <- convertTZ(readr.data,"sample_start_time_datum_cd_reported","sample_end_dateTime",tz)
          readr.data$sample_start_time_datum_cd_reported<- readr.data$sample_start_time_datum_cd_reported_reported 
          readr.data$sample_start_time_datum_cd_reported_reported <- NULL 
        }
      }
      names(readr.data)[names(readr.data) == "sample_dateTime"] <- "startDateTime"
      names(readr.data)[names(readr.data) == "sample_end_dateTime"] <- "endDateTime"
    }
    row.names(readr.data) <- NULL
    
    if(nrow(problems.orig) > 0){
      attr(readr.data, "problems") <- problems.orig
    }
  } else {
    readr.data <- data.frame(matrix(vector(), 0, length(header.names),
                                    dimnames=list(c(), header.names)),
                             stringsAsFactors=FALSE)
  }

  names(readr.data) <- make.names(names(readr.data))
  
  attr(readr.data, "queryTime") <- Sys.time()
  if(!file.exists(obs_url)){
    attr(readr.data, "url") <- obs_url
    attr(readr.data, "header") <- attr(doc, "header")
  }

  if("spec" %in% names(attributes(readr.data))){
    attr(readr.data, "spec") <- NULL
  }
  
  return(readr.data)
  
}

convertTZ <- function(df, tz.name, date.time.cols, tz, flip.cols=TRUE){
  
  offsetLibrary <- data.frame(offset=c(5, 4, 6, 5, 7, 6, 8, 7, 9, 8, 10, 10, 0, 0),
                              code=c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST","", NA),
                              stringsAsFactors = FALSE)
  
  offset <- left_join(df[,tz.name,drop=FALSE],offsetLibrary, by=setNames("code",tz.name))
  offset <- offset$offset
  df[,paste0(tz.name,"_reported")] <- df[,tz.name,drop=FALSE]
  
  df[,date.time.cols] <- df[,date.time.cols] + offset*60*60
  df[,date.time.cols] <- as.POSIXct(df[,date.time.cols])
  
  if(tz != ""){
    attr(df[,date.time.cols], "tzone") <- tz
    df[,tz.name] <- tz
  } else {
    attr(df[,date.time.cols], "tzone") <- "UTC"
    df[!is.na(df[,date.time.cols]),tz.name] <- "UTC"
  }
  
  if(flip.cols){
    reported.col <- which(names(df) %in% paste0(tz.name,"_reported"))
    orig.col <- which(names(df) %in% tz.name)
    
    new.order <- 1:ncol(df)
    new.order[orig.col] <- reported.col
    new.order[reported.col] <- orig.col
    
    df <- df[,new.order]
  }
  
  if(all(is.na(df[,date.time.cols]))){
    df[,date.time.cols] <- NULL
  }
  
  return(df)
}

fixErrors <- function(readr.data, readr.data.char, message.text, FUN, ...){
  FUN <- match.fun(FUN)
  badCols <- attr(readr.data, "problems")[["col"]] 
  int.message <- grep(message.text, attr(readr.data, "problems")[["expected"]])
  if(length(int.message) > 0){
    unique.bad.cols <- unique(badCols[int.message])
    index.col <- as.integer(gsub("X","",unique.bad.cols))
    
    for(i in index.col){
      readr.data[,i] <- tryCatch({
        FUN(readr.data.char[[i]], ...)
      }, warning=function(cond){
        readr.data.char[[i]]
      })
      attr(readr.data, "problems") <- attr(readr.data, "problems")[attr(readr.data, "problems")[["col"]] != paste0("X",i),]
    }
  }
  return(readr.data)
}

