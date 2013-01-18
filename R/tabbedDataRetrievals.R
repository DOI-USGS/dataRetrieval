#' Retrieval functions for USGS data
#'
#' \tabular{ll}{
#' Package: \tab dataRetrieval\cr
#' Type: \tab Package\cr
#' Version: \tab 1.2.1\cr
#' Date: \tab 2012-12-31\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' http://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Collection of functions to help retrieve USGS data from either web services or user provided data files.
#'
#' @name dataRetrieval-package
#' @docType package
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}, Laura De Cicco \email{ldecicco@@usgs.gov}
#' @references Hirsch, R. M., Moyer, D. L. and Archfield, S. A. (2010), Weighted Regressions on Time, Discharge, and Season (WRTDS), with an Application to Chesapeake Bay River Inputs. JAWRA Journal of the American Water Resources Association, 46: 857-880. doi: 10.1111/j.1752-1688.2010.00482.x
#' @keywords data, retrieval
NULL

#' Example Water Quality Data included in dataRetrieval
#'
#' Example data representing Nitrate from the Choptank River at Greensboro, MD, USGS data
#' \if{html}{\figure{Chop1.png}{Example Data}}
#' \if{latex}{\figure{Chop1.png}}
#'
#' @name ChoptankRiverNitrate
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Example Streamflow Data included in dataRetrieval
#'
#' Example data representing Streamflow and Nitrate from the Choptank River at Greensboro, MD, USGS data
#'
#' @name ChoptankRiverFlow
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water flow data
NULL

#' Flux units included in dataRetrieval
#'
#' Flux units included:
#' \tabular{lllll}{
#' Number \tab ObjectName \tab shortName \tab unitFactor \tab unitName  \cr
#' 1      \tab POUNDS_DAY \tab lbs/day   \tab 2.204623   \tab pounds/day\cr
#' 2      \tab TONS_DAY   \tab tons/day  \tab 0.001102  \tab tons/day   \cr
#' 3      \tab KG_DAY     \tab kg/day    \tab 1          \tab kg/day    \cr
#' 4      \tab THOUSAND_KG_DAY\tab 10^3 kg/day \tab 0.001 \tab "thousands of kg/day\cr
#' 5      \tab TONS_YEAR\tab tons/yr \tab 0.402619 \tab tons/year\cr
#' 6      \tab THOUSAND_TONS_YEAR\tab 10^3 tons/yr \tab 0.000402619 \tab thousands of tons/year \cr
#' 7      \tab MILLION_TONS_YEAR\tab 10^6 tons/yr \tab 4.02619e-07 \tab millions of tons/year\cr
#' 8      \tab THOUSAND_KG_YEAR\tab 10^3 kg/yr \tab 0.36525 \tab thousands of kg/year\cr
#' 9      \tab MILLION_KG_YEAR\tab 10^6 kg/yr \tab 0.00036525 \tab millions of kg/year\cr
#' 10     \tab BILLION_KG_YEAR\tab 10^9 kg/yr \tab 3.6525e-07 \tab billions of kg/year \cr
#' }
#' 
#'
#' @name FLUX_UNIT
#' @docType data
NULL

#' Example Sample Dataframe included in dataRetrieval
#'
#' Initial Sample data frame from the Choptank River
#'
#' @name exSample
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water flow data
NULL

#' Example Daily Dataframe included in dataRetrieval
#'
#' Initial Daily data frame from the Choptank River
#'
#' @name exDaily
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water flow data
NULL





