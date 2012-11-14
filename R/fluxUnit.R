#' fluxUnit class
#'
#' Some details about the fluxUnit class
#'
#' \describe{
#'    \item{shortName}{A character specifying the short name.}
#'
#'    \item{unitFactor}{A numeric representing the conversion factor}
#' 
#'    \item{unitName}{A character specifying the full name.}
#'  }
#' @name fluxUnit-class
#' @rdname fluxUnit-class
#' @exportClass fluxUnit
setClass("fluxUnit",
	representation(
		shortName  = "character",
		unitFactor = "numeric",
		unitName   = "character"
	)	
)

testFunc <- function (fluxUnitVar=FLUX_UNIT$POUNDS_DAY){
  unitFactorReturn <- fluxUnitVar@unitFactor
  return(unitFactorReturn)
}

