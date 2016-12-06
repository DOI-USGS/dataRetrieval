pkg.env <- new.env()

.onLoad = function(libname, pkgname){
  suppressMessages(setAccess('public'))
  options(Access.dataRetrieval = NULL)
}

.onAttach = function(libname, pkgname){
  packageStartupMessage("USGS is switching from http to https: 
Please see https://help.waterdata.usgs.gov/news/December%205%2C%202016
for more information.")  
}
