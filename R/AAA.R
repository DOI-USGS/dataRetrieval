pkg.env <- new.env()
.onLoad = function(libname, pkgname){
  setAccess('public')
}