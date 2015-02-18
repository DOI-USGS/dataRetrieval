#' Raw Data Import for USGS NWIS QW Data
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/qwdata}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @details Valid parameter code groups are "All," or group codes:
#'\tabular{ll}{
#'Code \tab Description\cr
#'INF \tab Information \cr
#'PHY \tab Physical \cr
#'INM \tab Inorganics, Major, Metals (major cations) \cr
#'INN \tab Inorganics, Major, Non-metals (major anions) \cr
#'NUT \tab Nutrient \cr
#'MBI \tab Microbiological \cr
#'BIO \tab Biological \cr
#'IMN \tab Inorganics, Minor, Non-metals \cr
#'IMM \tab Inorganics, Minor, Metals \cr
#'TOX \tab Toxicity \cr
#'OPE \tab Organics, pesticide \cr
#'OPC \tab Organics, PCBs \cr
#'OOT \tab Organics, other \cr
#'RAD \tab Radiochemical \cr
#'SED \tab Sediment \cr
#'POP \tab Population/community \cr
#'}
#'
#' @param siteNumbers character of USGS site numbers.  This is usually an 8 digit number
#' @param parameterCd character that contains the code for a parameter
#' group, or a character vector of 5-digit parameter codes. See \bold{Details}.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @param expanded logical defaults to \code{TRUE}. If \code{TRUE}, retrieves additional information. Expanded data includes
#' remark_cd (remark code), result_va (result value), val_qual_tx (result value qualifier code), meth_cd (method code),
#' dqi_cd (data-quality indicator code), rpt_lev_va (reporting level), and rpt_lev_cd (reporting level type). If \code{FALSE},
#' only returns remark_cd (remark code) and result_va (result value). Expanded = \code{FALSE} will not give
#' sufficient information for unbiased statistical analysis.
#' @param reshape logical, reshape the data. If \code{TRUE}, then return a wide data frame with all water-quality in a single row for each sample. 
#' If \code{FALSE} (default), then return a long data frame with each water-quality result in a single row.
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @keywords data import USGS web service
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' datetime \tab POSIXct \tab The date and time of the value converted to UTC (if asDateTime = TRUE), \cr 
#' \tab character \tab or raw character string (if asDateTime = FALSE) \cr
#' tz_cd \tab character \tab The time zone code for datetime \cr
#' code \tab character \tab Any codes that qualify the corresponding value\cr
#' value \tab numeric \tab The numeric value for the parameter \cr
#' }
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' comment \tab character \tab Header comments from the RDB file \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' variableInfo \tab data.frame \tab A data frame containing information on the requested parameters \cr
#' }
#' @export
#' @import reshape2
#' @seealso \code{\link{readWQPdata}}, \code{\link{whatWQPsites}}, 
#' \code{\link{readWQPqw}}, \code{\link{constructNWISURL}}
#' @examples
#' siteNumbers <- c('04024430','04024000')
#' startDate <- '2010-01-01'
#' endDate <- ''
#' parameterCd <- c('34247','30234','32104','34220')
#' \dontrun{
#' rawNWISqwData <- readNWISqw(siteNumbers,parameterCd,startDate,endDate)
#' rawNWISqwDataReshaped <- readNWISqw(siteNumbers,parameterCd,
#'           startDate,endDate,reshape=TRUE)
#' parameterCd <- "all"
#' rawNWISall <- readNWISqw(siteNumbers,parameterCd,
#'           startDate,endDate)
#' pgroup <- c("NUT")
#' rawNWISNutrients <- readNWISqw(siteNumbers,pgroup,
#'           startDate,endDate)
#' groups <- c("NUT","OPE")
#' rawNWISNutOpe <- readNWISqw(siteNumbers,groups,
#'           startDate,endDate) 
#' rawNWISOpe <- readNWISqw(siteNumbers,"OPE",
#'           startDate,endDate) 
#'          } 
readNWISqw <- function (siteNumbers,parameterCd,startDate="",endDate="",
                        expanded=TRUE,reshape=FALSE,tz=""){  
  
  pgrp <- c("INF", "PHY", "INM", "INN", "NUT", "MBI", "BIO", "IMM", "IMN", "TOX",
                           "OPE", "OPC", "OOT", "RAD", "XXX", "SED", "POP")
  
  
  

  if(any(parameterCd == "all") | any(parameterCd == "All") ){
    
    siteNumbers <- paste(siteNumbers, collapse=",")
    url <- paste0("http://nwis.waterdata.usgs.gov/nwis/qwdata?multiple_site_no=", siteNumbers,
           "&sort_key=site_no&group_key=NONE&inventory_output=0",
           "&begin_date=", startDate, "&end_date=", endDate,
           "&TZoutput=0",
           "&radio_parm_cds=all_parm_cds&qw_attributes=0&format=rdb",
           "&qw_sample_wide=0&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD",
           "&rdb_compression=value&list_of_search_criteria=multiple_site_no")
  } else if (all(parameterCd %in% pgrp)){
    siteNumbers <- paste(siteNumbers, collapse=",")
    groups <- paste(parameterCd, collapse=",")
    url <- paste0("http://nwis.waterdata.usgs.gov/nwis/qwdata?multiple_site_no=", siteNumbers,
                  "&sort_key=site_no&group_key=NONE&inventory_output=0",
                  "&begin_date=", startDate, "&end_date=", endDate,
                  "&TZoutput=0&param_group=", groups,
                  "&qw_attributes=0&format=rdb",
                  "&qw_sample_wide=0&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD",
                  "&rdb_compression=value&list_of_search_criteria=multiple_site_no")

  } else {
    url <- constructNWISURL(siteNumbers,
                            parameterCd,
                            startDate,
                            endDate,"qw",expanded=expanded)    
  }


  
  data <- importRDB1(url,asDateTime=TRUE, qw=TRUE, tz = tz)
  originalHeader <- comment(data)
  parameterCd <- unique(data$parm_cd)
  
  if(reshape){
    if(expanded){
      columnsToMelt <- c("agency_cd","site_no","sample_dt","sample_tm",
                         "sample_end_dt","sample_end_tm","sample_start_time_datum_cd","tm_datum_rlbty_cd",
                         "parm_cd","startDateTime","endDateTime","coll_ent_cd", "medium_cd","project_cd",
                         "aqfr_cd","tu_id","body_part_id", "hyd_cond_cd", "samp_type_cd",
                         "hyd_event_cd","sample_lab_cm_tx")
      columnsToMelt <- columnsToMelt[columnsToMelt %in% names(data)]
      dataWithPcodes <- data[data$parm_cd != "",]
      if(sum(data$parm_cd != "") > 0){
        warning("Some or all data returned without pCodes, those data will not be included in reshape")
      }
      longDF <- melt(dataWithPcodes, columnsToMelt)
      wideDF <- dcast(longDF, ... ~ variable + parm_cd )
      wideDF[,grep("_va_",names(wideDF))] <- sapply(wideDF[,grep("_va_",names(wideDF))], function(x) as.numeric(x))
      pCodesReturned <- unique(dataWithPcodes$parm_cd)
      groupByPCode <- as.vector(sapply(pCodesReturned, function(x) grep(x, names(wideDF)) ))
      data <- wideDF[,c(1:length(columnsToMelt)-1,groupByPCode)]
      comment(data) <- originalHeader
    } else {
      warning("Reshape can only be used with expanded data. Reshape request will be ignored.")
    }
  }
  parameterCd <- parameterCd[parameterCd != ""]
  
  siteInfo <- readNWISsite(siteNumbers)
  
  varInfo <- readNWISpCode(parameterCd)
  
  attr(data, "siteInfo") <- siteInfo
  attr(data, "variableInfo") <- varInfo
  attr(data, "statisticInfo") <- NULL
  
  return (data)

}
