#' Get USGS Water Use Data
#' 
#' @description USGS's National Water Availability Assessment Data Companion
#' (NWDC) offers web services that provide access to national-scale USGS
#' modeled water availability data underlying the National Water Availability
#' Assessment. More information at \url{https://water.usgs.gov/nwaa-data}.
#' 
#' @export
#' @param model See model IDs on the NWDC data catalog page. 
#' Options are : "wu-public-supply-wd", "wu-thermoelectric", "wu-irrigation-cu",
#' "wu-irrigation-wd", and "wu-public-supply-cu"
#' \url{https://water.usgs.gov/nwaa-data/data-catalog}
#' @param variable See variable IDs on the NWDC data catalog page.
#' @param location Options : huc12, huc10, huc8, huc6, huc4, huc2, countyCd, stateCd
#' Locations can be visualized with the Spatial Extent Viewer. Only one location
#' can be queried at a time. All data are returned on a HUC12 spatial resolution.
#' When a queried location contains more than the maximum number of HUC12s
#' allowed per page or file (defined under limit below), multiple pages or files
#' will be returned. All huc12, huc10, and countyCd location queries return a
#' single page or file. All huc2 location queries return multiple pages or files.
#' All other (huc8, huc6, huc4, stateCd) location queries may or may not return
#' multiple pages or files, depending on their size.
#' @param timeres Options : monthly, annualcy, annualwy. 
#' @param startdate Format : YYYY (for annual data) or YYYY-MM (for monthly data)
#' @param enddate Format : YYYY (for annual data) or YYYY-MM (for monthly data)
#' @param intersection Options : overlap, envelop
#' @param limit Defines the number of HUC12s returned per page or file.
#' Queries of locations containing more than the below maximum value of HUC12s
#' will return multiple pages or files (see location above for details).
#' @param attach_request logical, defaults to `r getOption("dataRetrieval.attach_request")`.
#' If set to `TRUE`, the full request sent to the Water Data API is attached
#' as an attribute to the data set.
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' wu1 <- read_wateruse(model = "wu-public-supply-wd",
#'                      variable = c("pswdtot", "pswdgw", "pswdsw"),
#'                      location = "stateCd:RI",
#'                      startdate = "2020-01",
#'                      timeres = "monthly")
#'                                
#' wu2 <- read_wateruse(model = "wu-thermoelectric",
#'                      variable = c('tecufgw', 'tecufsw', 'tecuftot',
#'                                   'tewdfgw', 'tewdfsw', 'tewdftot',
#'                                   'tewdssw'),
#'                      location = "stateCd:RI",
#'                      startdate = "2020-01",
#'                      timeres = "monthly")
#'
#' wu3 <- read_wateruse(model = "wu-irrigation-cu",
#'                      variable = "irrcutot",
#'                      location = "stateCd:WI",
#'                      startdate = "2020-01",
#'                      timeres = "monthly")
#'
#' wu4 <- read_wateruse(model = "wu-irrigation-wd",
#'                      variable = c("irrwdtot", "irrwdgw", "irrwdsw"),
#'                      location = "huc2:04",
#'                      startdate = "2015",
#'                      timeres = "annualcy")
#'          
#' wu5 <- read_wateruse(model = "wu-public-supply-cu",
#'                      variable = "pscutot",
#'                      location = "stateCd:WI",
#'                      startdate = "2020",
#'                      timeres = "annualwy")
#' }
read_wateruse <- function(model = NA_character_,
                          variable = NA_character_,
                          location = NA_character_,
                          timeres = NA_character_,
                          startdate = NA_character_,
                          enddate = NA_character_,
                          intersection = "overlap",
                          limit = 500,
                          attach_request = getOption("dataRetrieval.attach_request")){
  
  match.arg(intersection, choices = c("overlap", "envelop"), several.ok = FALSE)
  match.arg(model, choices = c("wu-public-supply-wd",
                               "wu-thermoelectric",
                               "wu-irrigation-cu",
                               "wu-irrigation-wd",
                               "wu-public-supply-cu"), several.ok = FALSE)
  
  match.arg(timeres, choices = c("monthly", "annualcy", "annualwy"))
  
  location_prefix <- sapply(strsplit(location, ":"), function(x) x[1])
  choices = c("huc12", "huc10", "huc8", "huc6", "huc4", "huc2", "countyCd", "stateCd")
  

  if(!location_prefix %in% choices){
    stop("Invalid location argument.")
  }

  if(limit > 500){
    limit <- 500
    message("limit was reset to 500 which is the maximum value for this service.")
  }
    
  if(!is.character(variable)){
    stop("Invalid variable argument.")
  }
  
  # Monthly needs XXXX-XX
  if(timeres == "monthly"){
    if(!is.na(startdate) & !grepl("^[0-9]{4}-[0-9]{2}$", startdate, perl = TRUE)){
      stop("Invalid startdate argument.")
    }
    
    if(!is.na(enddate) & !grepl("^[0-9]{4}-[0-9]{2}$", enddate, perl = TRUE)){
      stop("Invalid enddate argument.")
    }    
  } else { # Annual needs XXXX
    if(!is.na(startdate) & !grepl("^[0-9]{4}$", startdate, perl = TRUE)){
      stop("Invalid startdate argument.")
    }
    
    if(!is.na(enddate) & !grepl("^[0-9]{4}$", enddate, perl = TRUE)){
      stop("Invalid enddate argument.")
    }
  }

  args <- mget(names(formals()))

  args[["attach_request"]] <- NULL
  
  args <- Filter(Negate(anyNA), args)
  
  format <- "csv"
  
  baseURL <-   httr2::request("https://api.water.usgs.gov/") |> 
    httr2::req_url_path_append("nwaa-data") |> 
    httr2::req_url_path_append("data") |> 
    httr2::req_user_agent(default_ua()) |> 
    httr2::req_headers(`Accept` = "application/json") |> 
    httr2::req_url_query(format = format ) |> 
    httr2::req_error(body = error_body) |> 
    httr2::req_timeout(seconds = 180)

  baseURL <- explode_query(baseURL, POST = FALSE, args, multi = "comma")
  
  message("Requesting:\n", baseURL$url)

  resps <- httr2::req_perform_iterative(baseURL, 
                                        next_req = next_req_url_wu_csv, 
                                        max_reqs = Inf, on_error = "stop")

  df <- resps |>
    httr2::resps_successes() |>
    httr2::resps_data(\(resp) data.table::fread(text = httr2::resp_body_string(resp),
                                                data.table = FALSE,
                                                colClasses = list(character = "huc12_id")))

  if(attach_request){
    attr(df, "request") <- baseURL
  }
  attr(df, "queryTime") <- Sys.time()
  return(df)
  
}

next_req_url_wu_csv <- function(resp, req){

  headers <- httr2::resp_headers(resp)
  if("link" %in% names(headers)){
    if(grepl(pattern = '\"next\"', headers$link)){
      next_url <- gsub(pattern = ".*<(.*)>.*", 
                       replacement =  "\\1",
                       x =  headers$link)
      next_url <- gsub(pattern = "https://water.usgs.gov", 
                       replacement = "https://api.water.usgs.gov", 
                       x = next_url)
      return(httr2::req_url(req = req, url = next_url))
    }
  }
  return(NULL)
}




