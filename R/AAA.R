pkg.env <- new.env()

.onLoad <- function(libname, pkgname) {
  suppressMessages(setAccess("public"))
  pkg.env$nldi_base <- "https://labs.waterdata.usgs.gov/api/nldi/linked-data/"
  pkg.env$local_sf <- requireNamespace("sf", quietly = TRUE)
}


#' Is this a dataRetrieval user
#'
#' Reveals if this is a user or not
#' @export
#' @examples
#' is_dataRetrieval_user()
is_dataRetrieval_user <- function() {
  interactive() ||
    !identical(Sys.getenv("CI"), "") ||
    identical(Sys.getenv("NOT_CRAN"), "true")
}


wqp_message <- function(){
  message("NEWS: USGS data availability and format are changing. 
Beginning in February 2024 the data obtained from legacy profiles
will not include new USGS data or recent updates to 
existing data. When new WQX (3.0) profiles are available, the default 
profile will change. Users can continue to access data using the 
legacy profiles for some time, however they will not serve recent 
USGS data. Banners posted on the WQP site will have the most 
updated information (https://www.waterqualitydata.us/).
For additional details, see vignettes:
https://doi-usgs.github.io/dataRetrieval/articles/Status.html
https://doi-usgs.github.io/dataRetrieval/articles/qwdata_changes.html
If you have additional questions about these changes, 
email CompTools@usgs.gov.")
}

