pkg.env <- new.env()
.onLoad = function(libname, pkgname){
  suppressMessages(setAccess('public'))
  options(Access.dataRetrieval = NULL)
}