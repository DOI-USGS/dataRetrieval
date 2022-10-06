#' Function to return data from the NWISWeb WaterML1.1 service
#'
#' This function accepts a url parameter that already contains the desired
#' NWIS site, parameter code, statistic, startdate and enddate. 
#'
#' @param obs_url character or raw, containing the url for the retrieval or a file path to the data file, or raw XML.
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, Date
#' @param tz character to set timezone attribute of datetime. Default converts the datetimes to UTC 
#' (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Recommended US values include "UTC","America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla".
#' For a complete list, see \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones}
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#'  \tab POSIXct \tab The date and time of the value converted to UTC (if asDateTime = TRUE), \cr 
#' \tab character \tab or raw character string (if asDateTime = FALSE) \cr
#' tz_cd \tab character \tab The time zone code for  \cr
#' code \tab character \tab Any codes that qualify the corresponding value\cr
#' value \tab numeric \tab The numeric value for the parameter \cr
#' }
#' Note that code and value are repeated for the parameters requested. The names are of the form 
#' X_D_P_S, where X is literal, 
#' D is an option description of the parameter, 
#' P is the parameter code, 
#' and S is the statistic code (if applicable).
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' variableInfo \tab data.frame \tab A data frame containing information on the requested parameters \cr
#' statisticInfo \tab data.frame \tab A data frame containing information on the requested statistics on the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' }
#' 
#' @seealso \code{\link{renameNWISColumns}}
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_name
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_attrs
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_root
#' @examplesIf is_dataRetrieval_user()
#' site_id <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- '00003'
#' property <- '00060'
#' obs_url <- constructNWISURL(site_id,property,startDate,endDate,'dv')
#' \donttest{
#' data <- importWaterML1(obs_url, asDateTime=TRUE)
#' 
#' groundWaterSite <- "431049071324301"
#' startGW <- "2013-10-01"
#' endGW <- "2014-06-30"
#' groundwaterExampleURL <- constructNWISURL(groundWaterSite, NA,
#'           startGW,endGW, service="gwlevels")
#' groundWater <- importWaterML1(groundwaterExampleURL)
#' groundWater2 <- importWaterML1(groundwaterExampleURL, asDateTime=TRUE)
#' 
#' unitDataURL <- constructNWISURL(site_id,property,
#'          "2013-11-03","2013-11-03",'uv')
#' unitData <- importWaterML1(unitDataURL,TRUE)
#' 
#' # Two sites, two pcodes, one site has two data descriptors:
#' siteNumber <- c('01480015',"04085427")
#' obs_url <- constructNWISURL(siteNumber,c("00060","00010"),startDate,endDate,'dv')
#' data <- importWaterML1(obs_url)
#' data$dateTime <- as.Date(data$dateTime)
#' data <- renameNWISColumns(data)
#' names(attributes(data))
#' attr(data, "url")
#' attr(data, "disclaimer")
#' 
#' inactiveSite <- "05212700"
#' inactiveSite <- constructNWISURL(inactiveSite, "00060", "2014-01-01", "2014-01-10",'dv')
#' inactiveSite <- importWaterML1(inactiveSite)
#' 
#' inactiveAndAcitive <- c("07334200","05212700")
#' inactiveAndAcitive <- constructNWISURL(inactiveAndAcitive, "00060", "2014-01-01", "2014-01-10",'dv')
#' inactiveAndAcitive <- importWaterML1(inactiveAndAcitive)
#' 
#' # Timezone change with specified local timezone:
#' tzURL <- constructNWISURL("04027000", c("00300","63680"), "2011-11-05", "2011-11-07","uv")
#' tzIssue <- importWaterML1(tzURL, 
#'                           asDateTime = TRUE, tz = "America/Chicago")
#'
#' # raw XML
#' url <- constructNWISURL(service = 'dv', siteNumber = '02319300', parameterCd = "00060", 
#'                          startDate = "2014-01-01", endDate = "2014-01-01")
#' raw <- httr::content(httr::GET(url), as = 'raw')
#' rawParsed <- importWaterML1(raw)
#' }
#' filePath <- system.file("extdata", package="dataRetrieval")
#' fileName <- "WaterML1Example.xml"
#' fullPath <- file.path(filePath, fileName)
#' importFile <- importWaterML1(fullPath,TRUE)
#'
importWaterML1 <- function(obs_url,asDateTime=FALSE, tz="UTC"){
  #note: obs_url is a dated name, does not have to be a url/path
  
  returnedDoc <- check_if_xml(obs_url)
  raw <- !is.character(obs_url)
  
  if(tz == ""){  #check tz is valid if supplied
    tz <- "UTC"
  }
  tz <- match.arg(tz, OlsonNames())
  
  timeSeries <- xml_find_all(returnedDoc, ".//ns1:timeSeries") #each parameter/site combo
  
  #some initial attributes
  queryNodes <- xml_children(xml_find_all(returnedDoc,".//ns1:queryInfo"))
  notes <- queryNodes[xml_name(queryNodes)=="note"]
  noteTitles <- xml_attrs(notes)
  noteText <- xml_text(notes)
  noteList <- as.list(noteText)
  names(noteList) <- noteTitles
  
  if(0 == length(timeSeries)){
    df <- data.frame(agency_cd = character(),
                     site_no = character(),
                     dateTime = as.POSIXct(character()),
                     tz_cd = character())
    attr(df, "queryInfo") <- noteList
    if(!raw){
      attr(df, "url") <- obs_url
    }
    return(df)
  }
  
  mergedDF <- NULL
  
  for(t in timeSeries){
    #check if there are multiple time series (ie with different descriptors)
    #descriptor will be appended to col name if so
    valParents <- xml_find_all(t,".//ns1:values")
    obsDF <- NULL
    useMethodDesc <- FALSE
    if(length(valParents) > 1){ useMethodDesc <- TRUE} #append the method description to colnames later
    
    sourceInfo <- xml_children(xml_find_all(t, ".//ns1:sourceInfo"))
    variable <- xml_children(xml_find_all(t, ".//ns1:variable"))
    agency_cd <- xml_attr(sourceInfo[xml_name(sourceInfo)=="siteCode"],"agencyCode")
    pCode <- xml_text(variable[xml_name(variable)=="variableCode"])
    statCode <- xml_attr(xml_find_all(variable,".//ns1:option"),"optionCode")
    
    #site info
    srsNode <- xml_find_all(sourceInfo,".//ns1:geogLocation")
    srs <- xml_attr(srsNode, 'srs')
    locNodes <- xml_children(srsNode)
    locNames <- xml_name(locNodes)
    locText <- as.numeric(xml_text(locNodes))  
    names(locText) <- sub("longitude","dec_lon_va",sub("latitude","dec_lat_va",locNames))
    sitePropNodes <- sourceInfo[xml_name(sourceInfo)=="siteProperty"]
    siteProp <- xml_text(sitePropNodes)
    names(siteProp) <- xml_attr(sitePropNodes, "name")
    tzInfo <- unlist(xml_attrs(xml_find_all(sourceInfo,"ns1:defaultTimeZone")))
    siteName <- xml_text(sourceInfo[xml_name(sourceInfo)=="siteName"])
    siteCodeNode <- sourceInfo[xml_name(sourceInfo)=="siteCode"]
    site_no <- xml_text(siteCodeNode)
    siteCodeAtt <- unlist(xml_attrs(siteCodeNode))
    siteDF <- cbind.data.frame(t(locText),t(tzInfo),station_nm=siteName,t(siteCodeAtt),srs,t(siteProp),
                               site_no,stringsAsFactors = FALSE)
    defaultTZ <- xml_attr(xml_find_all(sourceInfo,".//ns1:defaultTimeZone"),"zoneAbbreviation")
    
    for(v in valParents){
      obsColName <- paste(pCode,statCode,sep = "_")
      obs <- xml_find_all(v, ".//ns1:value")
      values <- as.numeric(xml_text(obs))  #actual observations

      nObs <- length(values)
      qual <- xml_attr(obs,"qualifiers")
      
      noQual <- all(is.na(qual))
      
      dateTime <- xml_attr(obs,"dateTime")
      if(asDateTime){
        numChar <- nchar(dateTime)
        dateTime <- lubridate::parse_date_time(dateTime, c("%Y","%Y-%m-%d","%Y-%m-%dT%H:%M",
                                                "%Y-%m-%dT%H:%M:%S","%Y-%m-%dT%H:%M:%OS",
                                                "%Y-%m-%dT%H:%M:%OS%z"), exact = TRUE)
        if(any(numChar < 20) & any(numChar > 16)){
          offsetLibrary <- data.frame(offset=c(5, 4, 6, 5, 7, 6, 8, 7, 9, 8, 10, 10, 0),
                                      code=c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST",""),
                                      stringsAsFactors = FALSE)
          
          #not sure there is still a case for this (no offset on times)?
          dateTime[numChar < 20 & numChar > 16] <- dateTime[numChar < 20 & numChar > 16] + offsetLibrary[offsetLibrary$code == defaultTZ,"offset"]*60*60
          warning(paste("site",site_no[1], "had data without time zone offsets, so DST could not be accounted for"))
        }                                        
        
        #^^setting tz in as.POSIXct just sets the attribute, does not convert the time!
        # Set to UTC now, convert the whole thing to tz later.
        attr(dateTime, 'tzone') <- "UTC"
        tzCol <- rep(tz,nObs)
      } else {
        tzCol <- rep(defaultTZ, nObs)
      }
      #create column names, addressing if methodDesc is needed
      if(useMethodDesc){
        methodDesc <- xml_text(xml_find_all(v, ".//ns1:methodDescription"))
        #this keeps column names consistent with old version
        methodDesc <- gsub("\\[|\\]| |\\(|\\)",".",methodDesc)
        
        #sometimes methodDesc is empty
        if(nchar(methodDesc) > 0){
          obsColName <- paste("X",methodDesc,obsColName, sep = "_")
        }else{
          obsColName <- paste("X",obsColName, sep = "_")
        }
      } else{
        obsColName <- paste("X",obsColName, sep = "_")
      }
      qualColName <- paste(obsColName,"cd",sep = "_")
      
      valParentDF <- cbind.data.frame(dateTime, values, qual, tzCol, stringsAsFactors = FALSE)
      names(valParentDF) <- c("dateTime",obsColName, qualColName, "tz_cd")
      #delete qual column if all NA
      if(all(is.na(valParentDF[,eval(qualColName)]))){
        valParentDF <- subset(valParentDF, select = c("dateTime", eval(obsColName), "tz_cd"))
      }
      if(nrow(valParentDF) > 0){
        if(is.null(obsDF)){
          obsDF <- valParentDF
        } else {
          obsDF <- merge(x = obsDF,
                         y = valParentDF,
                         by = c("dateTime","tz_cd"), 
                         all = TRUE)
        }
      } else {
        #need column names for joining later
        # but don't overwrite:
        if(is.null(obsDF)){
          obsDF <- data.frame(dateTime=character(0), tz_cd=character(0), stringsAsFactors = FALSE)
          if(asDateTime){
            obsDF$dateTime <- as.POSIXct(obsDF$dateTime)
            attr(obsDF$dateTime, "tzone") <- "UTC"
          }
        }
      }
    }
    
    if(is.null(obsDF)){
      mergedSite <- data.frame()
      next
    }
    nObs <- nrow(obsDF)
    
    #statistic info
    options <- xml_find_all(variable,"ns1:option")
    stat <- options[xml_attr(options,"name")=="Statistic"]
    stat_nm <- xml_text(options[xml_attr(stat,"name")=="Statistic"])
    statCd <- xml_attr(stat, "optionCode")
    statDF <- cbind.data.frame(statisticCd=statCd,statisticName=stat_nm, stringsAsFactors = FALSE)
    
    #variable info
    varText <- as.data.frame(t(xml_text(variable)),stringsAsFactors = FALSE)
    varNames <- xml_name(variable) 
    varName <- sub("unit", "param_unit",varNames) #rename to stay consistent with orig importWaterMl1
    names(varText) <- varNames
    
    #replace no data vals with NA, change attribute df
    noDataVal <- as.numeric(varText$noDataValue)
    if(nObs > 0 & obsColName %in% names(obsDF)){
      obsDF[[obsColName]][obsDF[[obsColName]] == noDataVal] <- NA
    }
    varText$noDataValue <- NA
    
    #rep site no & agency, combine into DF
    obsDFrows <- nrow(obsDF)
    df <- cbind.data.frame(agency_cd = rep(agency_cd,obsDFrows), 
                           site_no = rep(site_no,obsDFrows), 
                           obsDF, stringsAsFactors = FALSE)
    
    #join by site no 
    #append siteInfo, stat, and variable if they don't match a previous one
    if (is.null(mergedDF)){
      mergedDF <- df
      mergedSite <- siteDF
      mergedVar <- varText
      mergedStat <- statDF
    } else {
      if(nrow(df) > 0){
        #merge separately with any same site nos, then recombine
        sameSite <- mergedDF[mergedDF$site_no == site_no,]
        if(nrow(sameSite) > 0){
          diffSite <- mergedDF[mergedDF$site_no != site_no,]
          #first need to delete the obs and qual columns if they have already been filled with NA
          deleteCols <- grepl(obsColName,colnames(sameSite))
          sameSite <- sameSite[,!deleteCols]
          sameSite_simNames <- intersect(colnames(sameSite), colnames(df))
          sameSite <- merge(x = sameSite, 
                            y = df, 
                            by = sameSite_simNames, 
                            all=TRUE)
          na.omit.unique <- function(x){
            if(all(is.na(x))) NA else stats::na.omit(x)
          }
          
          if(any(duplicated(sameSite[,c("agency_cd", "site_no", 
                                        "dateTime", "tz_cd")]))){

            types <- lapply(sameSite, class)
            sameSite <- stats::aggregate(.~ agency_cd + site_no + dateTime + tz_cd,
                        data = sameSite,
                        FUN = na.omit.unique, na.action=NULL )
            sameSite[,which(types == "numeric")] <- sapply(sameSite[,which(types == "numeric")], as.numeric)
            sameSite[,which(types == "character")] <- sapply(sameSite[,which(types == "character")], as.character)
            
          }
          
          sameSite <- sameSite[order(as.Date(sameSite$dateTime)),]

          mergedDF <- r_bind_dr(sameSite, diffSite)
        } else {
          similarNames <- intersect(colnames(mergedDF), colnames(df))
          mergedDF <- merge(x = mergedDF, 
                            y = df, 
                            by=similarNames, 
                            all = TRUE)
        }
      }
      mergedSite <- merge(x = mergedSite, 
                          y = siteDF, 
                          by = colnames(mergedSite), 
                          all = TRUE)
      mergedVar <- merge(x = mergedVar, 
                         y = varText, 
                         by = colnames(mergedVar), 
                         all = TRUE)
      mergedStat <- merge(x = mergedStat, 
                          y = statDF, 
                          by = colnames(mergedStat),
                          all = TRUE)
    }
  }
  
  if(!is.null(mergedSite)){
    #keep attribute df names the same as old version
    names(mergedSite) <- c("dec_lat_va", "dec_lon_va", "timeZoneOffset", "timeZoneAbbreviation",
                           "station_nm","network","agency_cd","srs","siteTypeCd",
                           "hucCd", "stateCd", "countyCd", "site_no")
    mergedSite <- mergedSite[c("station_nm", "site_no", "agency_cd", "timeZoneOffset", 
                               "timeZoneAbbreviation", "dec_lat_va","dec_lon_va","srs","siteTypeCd",
                               "hucCd","stateCd","countyCd","network")]
  }
  
  #move tz column to far right and sort by increasing site number to be consistent with old version
  mergedNames <- names(mergedDF)
  tzLoc <- grep("tz_cd", names(mergedDF))
  attr(mergedDF$dateTime, 'tzone') <- tz
  mergedDF <- mergedDF[c(mergedNames[-tzLoc],mergedNames[tzLoc])]
  
  mergedDF <- mergedDF[order(mergedDF$site_no, mergedDF$dateTime),]
###############################################################  
  names(mergedDF) <- make.names(names(mergedDF))
  
  #attach other site info etc as attributes of mergedDF
  if(!raw){
    attr(mergedDF, "url") <- obs_url
  }
  attr(mergedDF, "siteInfo") <- mergedSite
  attr(mergedDF, "variableInfo") <- mergedVar
  attr(mergedDF, "disclaimer") <- noteText[noteTitles=="disclaimer"]
  attr(mergedDF, "statisticInfo") <- mergedStat
  attr(mergedDF, "queryTime") <- Sys.time()
  
  return (mergedDF)
}

r_bind_dr <- function(df1, df2){
  
  # Note...this function doesn't retain factors/levels
  # That is not a problem with any dataRetrieval function
  # but, if this function gets used else-where, 
  # that should be addressed.
  df1 <- add_empty_col(df1, df2, setdiff(names(df2), names(df1)))
  df2 <- add_empty_col(df2, df1, setdiff(names(df1), names(df2)))
  
  df3 <- rbind(df1, df2)
  
  return(df3)
}

add_empty_col <- function(df, df_ref, col_names){
  
  if(length(col_names) > 0){
    if(nrow(df) > 0){
      df[,col_names] <- NA
    } else {
      for(i in col_names){
        column_type <- class(df_ref[[i]])
        df[i] <- empty_col(column_type)
      }
    }
  }
  return(df)
}

empty_col <- function(column_type){
  
  if(all(column_type %in% c("POSIXct","POSIXt" ))){
    column_type <- "POSIXct"
  }
  
  col_return <- switch(column_type,
         "numeric" = as.numeric(),
         "factor" = as.factor(),
         "list" = list(),
         "integer" = as.integer(),
         "Date" = as.Date(numeric(), origin = "1970-01-01"),
         "POSIXct" = as.POSIXct(numeric(), origin = "1970-01-01"),
         "POSIXlt" = as.POSIXlt(numeric(), origin = "1970-01-01"),
         "character" = as.character())
  
  return(col_return)
}

check_if_xml <- function(obs_url){

  if(class(obs_url) == "character" && file.exists(obs_url)){
    returnedDoc <- read_xml(obs_url)
  } else if(class(obs_url) == 'raw'){
    returnedDoc <- read_xml(obs_url)
  } else if(inherits(obs_url, c("xml_node", "xml_nodeset"))) {
    returnedDoc <- obs_url
  } else {
    
    doc <- getWebServiceData(obs_url, encoding='gzip')
    if(is.null(doc)){
      return(invisible(NULL))
    }
    returnedDoc <- xml_root(doc)
  }
  return(returnedDoc)
}