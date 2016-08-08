pkg.env <- new.env()

.onLoad = function(libname, pkgname){
  suppressMessages(setAccess('public'))
  options(Access.dataRetrieval = NULL)
}

.onAttach = function(libname, pkgname){
  packageStartupMessage("Recent changes to NWIS data may affect dataRetrieval output. 
Please see http://help.waterdata.usgs.gov/news/061016 
for more information.")  
}
