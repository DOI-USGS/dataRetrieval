.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()
  dataRetrieval_version = utils::packageVersion("dataRetrieval")
  packageStartupMessage("dataRetrieval ", dataRetrieval_version,"
Extended Documentation: https://rconnect.usgs.gov/dataRetrieval")
}

#' Retrieval functions for USGS and EPA data
#'
#' \tabular{ll}{
#' Package: \tab dataRetrieval\cr
#' Type: \tab Package\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' \url{https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits}\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Retrieval functions for USGS and EPA hydrologic and water quality data.
#'
#' Please see \url{https://pubs.er.usgs.gov/publication/tm4A10} for more information.
#'
#' @name dataRetrieval
#' @docType package
#' @author Laura De Cicco \email{ldecicco@@usgs.gov}
NULL

#' List of USGS parameter codes
#'
#' Complete list of USGS parameter codes as of Dec. 20, 2021.
#'
#' @name parameterCdFile
#' @return parameterData data frame with information about USGS parameters.
#'
#' \tabular{lll}{
#' Name \tab Type \tab Description\cr
#' parameter_cd \tab character \tab 5-digit USGS parameter code \cr
#' parameter_group_nm \tab character \tab USGS parameter group name\cr
#' parameter_nm \tab character \tab USGS parameter name\cr
#' casrn \tab character \tab Chemical Abstracts Service (CAS) Registry Number\cr
#' srsname \tab character \tab Substance Registry Services Name\cr
#' parameter_units \tab character \tab Parameter units\cr
#' }
#'
#'
#' @docType data
#' @export parameterCdFile
#' @examples
#' head(parameterCdFile[, 1:2])
NULL




#' Data to convert USGS parameter code to characteristic names
#'
#' Data pulled from Water Quality Portal on December 20, 2021. The data was pulled from
#' \url{https://www.waterqualitydata.us/Codes/public_srsnames/?mimeType=csv}.
#'
#' @name pCodeToName
#' @return pCodeToName data frame with information about USGS parameters and how they
#' relate to characteristic names (useful for WQP requests).
#'
#' \tabular{lll}{
#' Name \tab Type \tab Description\cr
#' parm_cd \tab character \tab 5-digit USGS parameter code \cr
#' description \tab character \tab Parameter description\cr
#' characteristicname \tab character \tab Characteristic Name \cr
#' measureunitcode \tab character \tab Parameter units\cr
#' resultsamplefraction \tab character \tab Result sample fraction text\cr
#' resulttemperaturebasis \tab character \tab Temperature basis information\cr
#' resultstatisticalbasis \tab character \tab Statistical basis\cr
#' resulttimebasis \tab character \tab Time basis\cr
#' resultweightbasis \tab character \tab Weight basis\cr
#' resultparticlesizebasis \tab character \tab Particle size basis\cr
#' last_rev_dt \tab character \tab Latest revision of information\cr
#' }
#' @docType data
#' @export pCodeToName
#' @keywords internal
#' @examples
#' head(pCodeToName[, 1:2])
NULL

#' US State Code Lookup Table
#'
#' Data originally pulled from \url{https://www2.census.gov/geo/docs/reference/state.txt}
#' on April 1, 2015. On Feb. 11, 2022, the fields were updated with the
#' file found in inst/extdata, which is used internally with NWIS retrievals.
#'
#' @name stateCd
#' @return stateCd data frame.
#'
#' \tabular{lll}{
#' Name \tab Type \tab Description\cr
#' STATE \tab character \tab FIPS State Code  \cr
#' STUSAB \tab character \tab Official United States Postal Service (USPS) Code \cr
#' STATE_NAME \tab character \tab State Name \cr
#' STATENS \tab character \tab  Geographic Names Information System Identifier (GNISID)\cr
#' }
#' @docType data
#' @export stateCd
#' @keywords USGS stateCd
#' @examples
#' head(stateCd)
NULL

#' US County Code Lookup Table
#'
#' Data originally pulled from \url{https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt}
#' on April 1, 2015. On Feb. 11, 2022, the fields were updated with the
#' file found in inst/extdata, which is used internally with NWIS retrievals.
#'
#' @name countyCd
#' @return countyCd data frame.
#'
#' \tabular{lll}{
#' Name \tab Type \tab Description\cr
#' STUSAB \tab character \tab State abbreviation \cr
#' STATE \tab character \tab two-digit ANSI code  \cr
#' COUNTY \tab character \tab three-digit county code \cr
#' COUNTY_NAME \tab character \tab County full name \cr
#' COUNTY_ID \tab character \tab County id \cr
#' }
#' @docType data
#' @export countyCd
#' @keywords USGS countyCd
#' @examples
#' head(countyCd)
NULL

# nolint start: commented_code_linter
# Here's how to incorporate the state_county.json into the historic
# sysdata.rda. The original data included some IDs that aren't in the json
# We're leaving those in for now because maybe it's helpful,
# even though to my knowledge it's not used in any dataRetrieval query.

# #Step 1: open the json file, parse out what's needed:
# x <- jsonlite::read_json("inst/extdata/state_county.json")
# states <- x[["US"]]
# state_df <- data.frame("STATE" = names(sapply(states$state_cd, function(x) x[[1]])),
#                        "STATE_NAME" = as.character(sapply(states$state_cd, function(x) x[[1]])))
# #Step 2: join it into the original stateCd data frame:
# # This preserves STATE_NAME, which is nice
#
# library(dplyr)
# state_df <- state_df %>%
#   left_join(stateCd, by = c("STATE", "STATE_NAME"))
#
# state_df$STUSAB[state_df$STATE_NAME == "Virgin Islands"] <- "VI"
#
# #Step 3: now get the county names/ids:
# y <- sapply(states$state_cd, function(x) x[["county_cd"]])
#
# county_df <- data.frame()
#
# for(st in state_df$STATE) {
#   county_nums <- names(y[[st]])
#   county_names <- as.character(unlist(y[[st]]))
#   county_df_st <- data.frame(STATE = st,
#                              COUNTY = county_nums,
#                              COUNTY_NAME = county_names)
#   county_df <- dplyr::bind_rows(county_df, county_df_st)
# }
#
# county_df_full <- county_df %>%
#   left_join(select(state_df, STUSAB, STATE), by = "STATE") %>%
#   left_join(select(countyCd, STATE, STUSAB, COUNTY, COUNTY_ID),
#             by = c("STATE", "STUSAB", "COUNTY"))
#
# countyCd <- county_df_full
# stateCd <- state_df
# save(countyCd, stateCd, parameterCdFile, pCodeToName,
#      file = "R/sysdata.rda", compress = "xz")
# nolint end
