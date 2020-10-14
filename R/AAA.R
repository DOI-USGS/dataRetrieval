pkg.env <- new.env()

.onLoad = function(libname, pkgname){
    suppressMessages(setAccess('public'))
    pkg.env$nldi_base <- "https://labs.waterdata.usgs.gov/api/nldi/linked-data/"
    pkg.env$local_sf <- requireNamespace("sf", quietly = TRUE)
}
