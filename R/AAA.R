pkg.env <- new.env()
.onLoad = function(libname, pkgname){
  suppressMessages(setAccess('public'))
  options(Access.dataRetrieval = NULL)
  packageStartupMessage("Recent changes to NWIS data may affect dataRetrieval output. \n
                        Please see http://help.waterdata.usgs.gov/news/061016 \n
                        for more information.")
}