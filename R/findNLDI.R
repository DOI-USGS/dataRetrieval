#' @title Trim and Cull NULLs
#' @description Remove NULL arguments from a named list
#' @param x a list
#' @keywords nldi internal
#' @return a list
#' @noRd

tc <- function(x) {
  Filter(Negate(is.null), x)
}

#' @title Get current NLDI offerings
#' @description Used to query the current resources available through the NLDI
#' @return data.frame
#' @export
#' @keywords nldi
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @examples
#' get_nldi_sources()

get_nldi_sources <- function() {
  res <-
    httr::RETRY("GET",
                pkg.env$nldi_base,
                times = 3,
                pause_cap = 60)

  if (res$status_code == 200) {

    d <- httr::content(res, "text", encoding = "UTF8")

    d <- jsonlite::fromJSON(d, simplifyDataFrame = TRUE)

    return(d)
  } else {
    message("Error in: ",  url)
  }
}

#' @title Query NLDI
#' @description Queries the NLDI for a given URL. If local sf install is available the function returns a data.frame with the sfc geometry column listed. Such an object can be converted to sf with `sf::st_as_sf()`. If the object requested is a POINT object, the XY coordinates are added as columns. Otherwise the columns returned are "sourceName" and "identifier" for features, and "nhdplus_comid" for navigated paths.
#' @param url the URL to retrieve
#' @param type the type of data being returned (nav or feature)
#' @param use_sf should a local sf install be usedto parse data
#' @keywords nldi internal
#' @noRd
#' @return a data.frame
#' @importFrom httr content RETRY
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#'  base = "https://labs.waterdata.usgs.gov/api/nldi/linked-data/"
#'  get_nldi(paste0(base, "comid/101"), type = "feature", use_sf = FALSE)
#'  get_nldi(paste0(base, "comid/101"), type = "feature", use_sf = TRUE)
#'  get_nldi(url = paste0(base, "nwissite/USGS-11120000"), type = "feature", use_sf = TRUE)
#'  get_nldi(paste0(base, "nwissite/USGS-11120000"), type = "feature", use_sf = TRUE)
#'  }

get_nldi = function(url, type = "", use_sf = FALSE){

  # The features names are different across features, navigation, and basin returns
  # This sets the environment for what to expect

  if(type == "nav"){
    good_name = c("nhdplus_comid")
  } else if(type == "feature") {
    good_name = c("sourceName", "identifier")
  } else {
    good_name = NULL
  }

  # Query
  res <- httr::RETRY("GET", url, times = 3, pause_cap = 60)

  # If successful ...
  if(res$status_code == 200){

    # Interpret as text
    d <- httr::content(res, "text", encoding = "UTF8")

    if(d == ""){
      message("No data returned for: ", url, call. = FALSE)
      return(NULL)
    }

    if(use_sf){

      #Parse with sf
       tmp <- sf::read_sf(d)

      # if of type POINT at the X,Y coordinates as columns
      if(sf::st_geometry_type(tmp)[1] =="POINT"){

        tmp$X = sf::st_coordinates(tmp)[,1]
        tmp$Y = sf::st_coordinates(tmp)[,2]

        tmp = tmp[,c(good_name, "X", "Y")]

      } else {
        # If line/polygon then keep geometry but don't expand
        tmp = tmp[,c(good_name, "geometry")]

      }
       # Returning as data.frame drops the geometry column ...
        return(data.frame(tmp))

    } else {

      # Parse as simplified JSON
      d <- jsonlite::fromJSON(d, simplifyDataFrame = TRUE)

      # if of type POINT at the X,Y coordinates as columns
      if(d$features$geometry$type[1] == "Point"){

        geom = d$features$geometry$coordinates

        tmp = cbind(d$features$properties[,good_name], do.call(rbind, geom))

        names(tmp) <- c(good_name, "X", "Y")

        return(tmp)

      } else {
        # If line/polygon then keep geometry but don't expand
        return(d$features$properties[,c(good_name)])
      }
    }

    } else {
      message("Error in: ",  url)
    }

}

#' Clean NWIS NLDI ids
#' @description The NWIS ids come as "USGS-XXXXXXXX". This is not suitable for passing to other package functions like readNWISdv.
#' This function strips the "USGS-" from these ids.
#' @param tmp a data.frame retrieved from get_nldi()
#' @return the input object with potentially modified identifiers
#' @keywords nldi internal
#' @noRd

clean_nwis_ids = function(tmp) {
  # If data.frame, and of type NWIS, then strip "USGS-" from identifiers
  if (class(tmp) == 'data.frame') {

    if (sum(tmp$sourceName[1] == "NWIS Sites") == 1) {

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
#' \dontrun{
#' valid_ask(get_nldi_sources(), "nwis")
#' }

valid_ask = function(all, type){
  # those where the requested pattern is included in a nldi_source ...
    # means we will catch nwis - not just nwissite ...
    # means we will catch both wqp and WQP ...
  cond  <- grepl(paste0(tolower(type), collapse = "|"), tolower(all$source))

  cond2 <- grepl(paste0(tolower(all$source), collapse = "|"), tolower(type))

  list(good = all[cond,], bad = type[!cond2])

}

#' @title 	Retrieve features from the \href{https://labs.waterdata.usgs.gov/api/nldi/swagger-ui/index.html?configUrl=/api/nldi/v3/api-docs/swagger-config}{NLDI}
#' @description Provides a formal query to the
#' \href{https://labs.waterdata.usgs.gov/about-nldi/index.html}{Network Linked Data Index}.
#' The function is useful for topology and location based feature discovery.
#' A user must supply a starting feature, and can add optional navigation direction(s),
#' and features to identify on the navigated network.
#' Valid starting options can be given by one of the following arguments: comid, nwis, huc12,
#'  wqp, location, and start.
#' @param comid an NHDPlusV2 COMID
#' @param nwis  a USGS NWIS siteID
#' @param wqp a water quality point ID
#' @param huc12 a HUC12 ID
#' @param location Coordinate pair in WGS84 GCS provided as a numeric vector ordered lng/lat
#' @param origin a named list specifying a feature type and ID (e.g. list("comid" = 101))
#' @param nav where to navigate from the starting point ("UM", "UT", DM", "DD")
#' @param find what resources to find along the navigation path(s) (see get_nldi_sources()$source). Can also include 'basin', which will return the upstream basin of the starting feature
#' @param distance_km how far to look along the navigation path in kilometers (default = 100)
#' @param no_sf if available, should `sf` be used for parsing, defaults to `TRUE` if `sf` is locally installed
#' @return a list of data.frames
#' @export
#' @keywords nldi
#' @examples
#'
#' # Find Features / Define origin features
#'
#' ## Find feature by COMID
#'  findNLDI(comid = 101)
#'
#' ## Find feature by NWIS ID
#'  findNLDI(nwis = '11120000')
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
#' # Navigation
#' # UPPER MAINSTEM of USGS-11120000
#'  str(findNLDI(nwis = '11120000', nav = "UM"), max.level = 1)
#'
#' # MULTI-REQUEST
#' # UPPER MAINSTEM and TRIBUTARY of USGS-11120000
#'  str(findNLDI(nwis = '11120000', nav = c("UT", "UM")), max.level = 1)
#'
#' # Discover Features
#'
#' ## Find feature(s) on the upper tributary of USGS-11120000
#'  str(findNLDI(nwis = '11120000', nav = "UT", find = c("nwis", "wqp")), max.level = 1)
#'
#' ## Find upstream basin boundary of USGS-11120000
#'  str(findNLDI(nwis = '11120000',  find = "basin"), max.level = 1)
#'
#' # Control Distance
#'
#' ## Limit search to 100 km
#'  str(findNLDI(comid = 101, nav = "DM", find = c("nwis", "wqp"), distance_km = 100), max.level = 1)
#'
#' ## Convert returned list of data.frames to list of spatial features
#' \donttest{
#'  nldi = findNLDI(nwis = '11120000', nav = "UT", find = c("nwis", "wqp"))
#'  str(lapply(nldi, sf::st_as_sf), max.level = 1)
#'  }


findNLDI <- function(comid = NULL,
                    nwis = NULL,
                    wqp = NULL,
                    huc12 = NULL,
                    location = NULL,
                    origin = NULL,
                    nav = NULL,
                    find = NULL,
                    distance_km = 100,
                    no_sf = FALSE) {

  # Should sf be used? Both no_sf and pkg.env must agree
  use_sf = all(pkg.env$local_sf, !no_sf)

  # Should the basin be identified?
  getBasin = ("basin" %in% find)

  # From the collection of all possible origins, pick 1 and remove NULLS
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
  bad_nav = !nav %in% c("UM", "UT", "DD", "DM")

  if (any(bad_nav)) {
    stop(nav[bad_nav], " not a valid navigation. Use one of: UM, UT, DD, DM")
  }

  # name of starter
  start_type = names(starter)

  # If location, ensure lng is first argument (hack for USA features)
  if (start_type == 'location') {
    if (location[1] > 0) {
      stop("Please provide location in the form c(lng,lat)")
    }

  # Must convert location to COMID for tracing and discovery ...
    tmp_url = paste0(
      pkg.env$nldi_base,
      "comid/position?coords=POINT%28",
      location[1] ,
      "%20",
      location[2] ,
      "%29"
    )

    tmp_return = get_nldi(tmp_url, "feature", use_sf = use_sf)

    # Override starter with location based COMID
    starter = list("comid" = tmp_return$identifier)

  }

  # Reset (if needed)
  start_type = names(starter)

  # Defining the origin URL.
    #  Align request with formal name from offerings
    # If NWIS, add "USGS-" prefix
  start_url = paste0(
    valid_ask(pkg.env$current_nldi, type = start_type)$good$feature,
    "/",
    ifelse(start_type == "nwis", paste0("USGS-", starter), starter),
    "/"
  )

  # Makes sure that all requested features to `find` are valid and name-aligned
  if (!is.null(find)) {
    find = valid_ask(pkg.env$current_nldi, type = find)$good$source
  }

  # Set empty lists to store origin, navigation, features, and basin requests ...
  start <- navigate <- features <- basin <- list()

  # Set origin url
  start[["start"]] <- start_url

  # Build navigation URLs
  for (i in seq_along(nav)) {
      navigate[[nav[i]]] = paste0(start_url, "navigation/", nav[i])
  }

  # Build basin URL
  if (getBasin) {
    basin[["basin"]] = paste0(start_url, "basin")
  }

  # Build find URLs
  if (length(find) > 0) {
    features = lapply(navigate, paste0, paste0("/", find))
  }

  # Add distance constraints to features
  features = lapply(features, paste0, paste0("?distance=", distance_km))

  # Add distance constraints to navigations flowpaths
  navigate = lapply(navigate, paste0, paste0("/flowlines?distance=", distance_km))

  # combine, unlist, relist
  ll = as.list(unlist(c(start, basin, navigate, features)))

  # define the type of each URL (feature, basin, or nav)
  # This is needed as the attribute names for each varies
  types = c("feature",
            if (getBasin) { 'basin' },
            rep("nav", length(nav)),
            rep("feature", length(find) * length(nav)))

  # Send NLDI queries ...
  shp = lapply(seq_along(ll), function(x) {
    get_nldi(ll[[x]], type = types[x], use_sf = use_sf)
  })

  # Remove basin from find list ...
  find = find[find != 'basin']

  # if no features (aside from basin) were requested then names are NULL
  # else, the names are the combination of the navigation direction
  # and the feature discovered ...

  feats = if (is.null(find) | length(find) == 0) {
    NULL
  } else {
    unlist(lapply(nav, paste0, paste0("_", find)))
  }

  # Set the names of the parsed URL list
  names(shp) = c("origin",
                 if (getBasin){'basin'},
                 nav,
                 feats)

  # Clean up NWIS ids, trim NULLs, and return ...
  tc(lapply(shp, clean_nwis_ids))
}

