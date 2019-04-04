pkg.env <- new.env()

.onLoad = function(libname, pkgname){
  if("Access.dataRetrieval" %in% names(options())){
    suppressMessages(setAccess(options()[["Access.dataRetrieval"]]))
  } else {
    suppressMessages(setAccess('public'))
    options(Access.dataRetrieval = NULL)    
  }
}


