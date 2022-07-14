#' @title Trim and Cull NULLs
#' @description Remove NULL arguments from a named list
#' @param x a list
#' @keywords nldi internal
#' @return a list
#' @noRd

tc <- function(x) {
  Filter(Negate(is.null), x)
}

#' @title Find Good Names
#' @description minimize names of returned features
#' @param input data returned from NLDI
#' @param type type of return
#' @keywords nldi internal
#' @return a list
#' @noRd

find_good_names = function(input, type) {
  # The features names are different across features, navigation, and basin returns
  # This sets the environment for what to expect
  if (type == "nav") {
    grep("comid", names(input), value = TRUE)
  } else if (type == "feature") {
    c("sourceName", "identifier", "comid",
      names(input)[names(input) %in% c("name", "reachcode", "measure")])
  } else {
    NULL
  }
}

#' @title Get current NLDI offerings
#' @description Used to query the current resources available through the NLDI
#' @return data.frame
#' @export
#' @keywords nldi
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @examples
#' \donttest{
#' get_nldi_sources()
#' }
get_nldi_sources <- function() {
  res <-
    httr::RETRY("GET",
                pkg.env$nldi_base,
                times = 3,
                pause_cap = 60)
  
  if (res$status_code == 200) {
    jsonlite::fromJSON(httr::content(res, "text",
                                     encoding = "UTF8"),
                       simplifyDataFrame = TRUE)
    
  } else {
    message("Error in: ",  url)
  }
}

#' @title Query NLDI
#' @description Queries the NLDI for a given URL. If local sf install is
#' available the function returns a sf data.frame.
#' If the object requested is a POINT object, the XY coordinates are added
#' as columns. Otherwise the columns returned are "sourceName" and
#' "identifier" for features, and "nhdplus_comid".
#' @param url the URL to retrieve
#' @param type the type of data being returned (nav or feature)
#' @param use_sf should a local sf install be usedto parse data
#' @keywords nldi internal
#' @noRd
#' @return a data.frame
#' @importFrom httr content RETRY
#' @importFrom jsonlite fromJSON
#' @examples
#' \donttest{
#'  base = "https://labs.waterdata.usgs.gov/api/nldi/linked-data/"
#'  get_nldi(paste0(base, "comid/101"), type = "feature", use_sf = FALSE)
#'  get_nldi(paste0(base, "comid/101"), type = "feature", use_sf = TRUE)
#'  get_nldi(url = paste0(base, "nwissite/USGS-11120000"), type = "feature", use_sf = TRUE)
#'  get_nldi(paste0(base, "nwissite/USGS-11120000"), type = "feature", use_sf = TRUE)
#'  }

get_nldi = function(url, type = "", use_sf = FALSE) {
  # Query
  res <- httr::RETRY("GET", url, times = 3, pause_cap = 60)
  
  # If successful ...
  if (res$status_code == 200) {
    # Interpret as text
    d <- httr::content(res, "text", encoding = "UTF8")
    
    if(grepl('An HTML representation is not available for this resource', d)){
      warning("No data returned for: ", url, call. = FALSE)
      return(NULL)
    }
    
    if (d == "") {
      warning("No data returned for: ", url, call. = FALSE)
      return(NULL)
    }
    
    if (use_sf) {
      #Parse with sf
      
      tmp <- tryCatch({
        sf::read_sf(d) }, 
      error   = function(e){ 
        message("No data found for: ", basename(url))
        return(NULL)},
      warning = function(w){  
        message("No data found for: ", basename(url))
        return(NULL) }
      )
        
      good_name = find_good_names(tmp, type)
      
      if(!is.null(tmp)){
        # if of type POINT at the X,Y coordinates as columns
        if (sf::st_geometry_type(tmp)[1] == "POINT") {
          tmp$X <- sf::st_coordinates(tmp)[, 1]
          tmp$Y <- sf::st_coordinates(tmp)[, 2]
          
          tmp <- tmp[, c(good_name, "X", "Y")]
        } else {
          # If line/polygon then keep geometry but don't expand
          tmp <- tmp[, c(good_name, attr(tmp, "sf_column"))]
        }
      }
     
      
      # Returning as data.frame drops the geometry column ...
      return(tmp)
      
    } else {
      # Parse as simplified JSON
      d <- jsonlite::fromJSON(d, simplifyDataFrame = TRUE)
      
      input <-  d$features$properties
     
      good_name = find_good_names(input, type)
      
      if(is.null(input) & type != "basin"){
        tmp = NULL
      } else {
        # if of type POINT at the X,Y coordinates as columns
        if (d$features$geometry$type[1] == "Point") {
          geom = d$features$geometry$coordinates
          
          tmp = cbind(input[, good_name], do.call(rbind, geom))
          
          names(tmp) <- c(good_name, "X", "Y")
          
          return(tmp)
          
        } else {
          # If line/polygon then keep geometry but don't expand
          return(input[, good_name])
        }
      }
    }
    
  } else {
    message("Error in: ",  url)
  }
}

#' Clean NWIS NLDI ids
#' @description The NWIS ids come as "USGS-XXXXXXXX". This is not suitable for 
#' passing to other package functions like readNWISdv.
#' This function strips the "USGS-" from these ids.
#' @param tmp a data.frame retrieved from get_nldi()
#' @return the input object with potentially modified identifiers
#' @keywords nldi internal
#' @noRd
clean_nwis_ids = function(tmp) {
  # If data.frame, and of type NWIS, then strip "USGS-" from identifiers
  if (is.data.frame(tmp)) {
    if ("sourceName" %in% names(tmp) &&
        tmp$sourceName[1] == "NWIS Sites") {
      tmp$identifier = gsub("USGS-", "", tmp$identifier)
      
    }
  }
  tmp
}

#' @title NLDI Validity Check
#' @description tests if NLDI feature is available. Is vectorized and works with partial string matching.
#' @param all a data.frame of available features (see get_nldi_sources)
#' @param type type(s) to check (character)
#' @return a list with good and bad entries
#' @keywords nldi internal
#' @noRd
#' @examples
#' \donttest{
#' valid_ask(all = get_nldi_sources(), "nwis")
#' }

valid_ask = function(all, type) {
  # those where the requested pattern is included in a nldi_source ...
  # means we will catch nwis - not just nwissite ...
  # means we will catch both wqp and WQP ...
  
  ### WOW! This is hacky and will hopefully be unneeded latter on....
  type = ifelse(type == "nwis", "nwissite", type)
  all = rbind(all, c("flowlines", "NHDPlus comid", NA))
  
  good  <-
    grepl(paste0(tolower(type), collapse = "|"), tolower(c(all$source)))
  
  bad <-
    grepl(paste0(tolower(all$source), collapse = "|"), tolower(c(all$source)))
  
  list(good = all[good, ], bad = type[!bad])
  
}

#' @title R Client for the Network Linked Data Index
#' @description Provides a formal client to the USGS
#' \href{https://labs.waterdata.usgs.gov/about-nldi/index.html}{Network Linked Data Index}.
#' @details The function is useful for topology and location based
#' feature discovery. A user must specify an origin feature, optional navigation
#' direction(s) along the network, as well as features to identify along the
#' navigated paths. Valid starting options can be given by one of the following
#' arguments: comid, nwis, huc12, wqp, location, and start.
#' @param comid numeric or character. An NHDPlusV2 COMID
#' @param nwis  numeric or character. A USGS NWIS surface water siteID
#' @param wqp   numeric or character. A water quality point ID
#' @param huc12 numeric or character. A WBD HUC12 unit ID
#' @param location numeric vector. Coordinate pair in WGS84
#' SRS ordered lng/lat (X,Y)
#' @param origin named list. Specifying a feature type and ID
#' (e.g. list("comid" = 101))
#' @param nav character vector. where to navigate from the starting point.
#' Options include along the upper mainsteam (UM), upstream tributary (UT),
#' downstream mainstem (DM) and downstream divergences (DD). You may select
#' one or more of the abbreviations ("UM", "UT", DM", "DD").
#' @param find character vector. Define what resources to find along the
#' navigation path(s) (see get_nldi_sources()$source). Can also include 'basin'
#' or 'flowline', which will return the upstream basin of the starting feature
#' or flowlines along the navigation respectively. The default is "flowlines". 
#' If you provide any other resource, AND want flowlines, then flowlines must 
#' be explicitly requested.
#' @param distance_km numeric. Define how far to look along the navigation path in
#' kilometers (default = 100)
#' @param no_sf if available, should `sf` be used for parsing,
#' defaults to `TRUE` if `sf` is locally installed
#' @return a list of data.frames if sf is not installed, a list of sf objects if it is
#' @export
#' @keywords nldi
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' # Find Features / Define origin features
#'
#' ## Find feature by COMID
#'  findNLDI(comid = 101)
#'  
#' ## Find feature by NWIS ID
#'   findNLDI(nwis = '11120000')
#' 
#' ## Find feature by WQP ID 
#'  findNLDI(wqp = 'USGS-04024315')
#'
#' ## Find feature by LOCATION
#'  findNLDI(location = c(-115,40))
#'
#' ## GENERAL ORIGIN: COMID
#'  findNLDI(origin = list("comid" = 101))
#'
#' ## GENERAL ORIGIN: WaDE
#'  findNLDI(origin = list("wade" = 'CA_45206'))
#'
#' # Navigation (flowlines will be returned if find is unspecified)
#' # UPPER MAINSTEM of USGS-11120000
#'  findNLDI(nwis = '11120000', nav = "UM")
#'
#' # MULTI-REQUEST
#' # UPPER MAINSTEM and TRIBUTARY of USGS-11120000
#'  findNLDI(nwis = '11120000', nav = c("UT", "UM"))
#'
#' # Discover Features(flowlines will not be returned unless included in find)
#'
#' ## Find feature(s) on the upper tributary of USGS-11120000
#'  findNLDI(nwis = '11120000', nav = "UT", find = c("nwis", "wqp"))
#'
#' ## Find upstream basin boundary and  of USGS-11120000
#'  findNLDI(nwis = '11120000',  find = "basin")
#'
#' # Control Distance
#' ## Limit search to 50 km
#'  findNLDI(comid = 101, nav = "DM", find = c("nwis", "wqp", "flowlines"), distance_km = 50)
#'}

findNLDI <- function(comid = NULL,
                     nwis = NULL,
                     wqp = NULL,
                     huc12 = NULL,
                     location = NULL,
                     origin = NULL,
                     nav = NULL,
                     find = c("flowlines"),
                     distance_km = 100,
                     no_sf = FALSE) {
  
  # Should sf be used? Both no_sf and pkg.env must agree
  use_sf = all(pkg.env$local_sf, !no_sf)
  
  # Should the basin be identified?
  getBasin = ("basin" %in% find)
  
  # From the collection of possible origins, pick 1 and remove NULLS
  starter <- tc(c(
    list(
      comid = comid,
      nwis = nwis,
      wqp = wqp,
      huc12 = huc12,
      location = location
    ),
    origin
  ))
  
  # a single starting location must be given ...
  if (is.null(starter) | length(starter) > 1) {
    stop("Define a single starting point. Use `find` to identify other resources.")
  }
  
  # Ensure nav types are valid
  bad_nav <- !nav %in% c("UM", "UT", "DD", "DM")
  
  if (any(bad_nav)) {
    stop(nav[bad_nav], " not a valid navigation. Chose from: UM, UT, DD, DM")
  }
  
  # name of starter
  start_type = names(starter)
  
  # If location, ensure lng is first argument (hack for USA features)
  if (start_type == 'location') {
    
    if (any(grepl("sfc$|sf$", class(location))) & use_sf) {
      if (sf::st_geometry_type(location) != "POINT") {
        stop("Only POINT objects can be passed to location")
      }
      
      location  = sf::st_coordinates(location)
    } else {
      if (location[1] > 0) {
        stop("Provide location in the form c(lng,lat)")
      }
    }
    
    # Must convert location to COMID for tracing and discovery ...
    tmp_url <- paste0(
      pkg.env$nldi_base,
      "comid/position?f=json&coords=POINT%28",
      location[1] ,
      "%20",
      location[2] ,
      "%29", "&f=json"
    )
    
    tmp_return <- get_nldi(tmp_url, type = "feature", use_sf = use_sf)
    
    # Override starter with location based COMID
    starter <- list("comid" = tmp_return$identifier)
  }
  
  # Reset (if needed)
  start_type <- names(starter)
  
  if (is.null(pkg.env$current_nldi)) {
    pkg.env$current_nldi <- get_nldi_sources()
  }
  
  # Defining the origin URL.
  #  Align request with formal name from sources
  #  If NWIS, add "USGS-" prefix
  start_url = paste0(
    valid_ask(all = pkg.env$current_nldi, type = start_type)$good$features,
    "/",
    ifelse(start_type == "nwis", paste0("USGS-", starter), starter),
    "/"
  )
  
  # Makes sure that all requested features to `find` are valid and name-aligned
  if (!is.null(find)) {
    find = valid_ask(pkg.env$current_nldi, type = find)$good$source
  }
  
  # Set empty lists to store origin, navigation, features, and basin requests ...
  navigate <- features  <- list()
  
  # Build navigation URLs
  for (i in seq_along(nav)) {
    navigate[[nav[i]]] = paste0(start_url, "navigation/", nav[i])
  }
  
  # Build find URLs
  if (length(find) > 0) {
    features = lapply(navigate,
                      paste0,
                      paste0("/", find),
                      paste0("?f=json&distance=", distance_km))
  }
  
  names  <- unlist(lapply(nav, paste0, paste0("_", find)))
  
  search <- data.frame(
    url = unlist(c(start_url,
                   if (getBasin) {
                     paste0(start_url, "basin?f=json")
                   },
                   features)),
    
    type = c("feature",
             if (getBasin) {
               'basin'
             },
             ifelse(
               rep(find, length(nav)) == "flowlines", "nav", "feature"
             )),
    
    name = c("origin",
             if (getBasin) {
               'basin'
             },
             names[!names %in%  c("UM_", "UT_", "DD_", "DM_")])
  )
  
  # Send NLDI queries ...
  shp <- lapply(1:nrow(search), function(x) {
    get_nldi(url = search$url[x],
             type = search$type[x],
             use_sf = use_sf)
  })
  
  # Set the names of the parsed URL list
  names(shp) <- search$name
  
  # Clean up NWIS ids, trim NULLs, and return ...
  shp = tc(lapply(shp, clean_nwis_ids))
  
  # dont return list for one length elements
  if (length(shp) == 1) {
    shp = shp[[1]]
  }
  
  return(shp)
}
