#' Parameter code to characteristic name
#'
#' This function no longer works because the underlying service
#' has been removed. Similar functionality can be found with
#' `check_waterdata_sample_params`.
#'
#' @export
#' @param parameterCd character that contains the code for a character vector
#' of 5-digit parameter codes.
#' @examples
#' pcodes <- c("00070", "00075", "00430", "52642")
#' \donttest{
#' all_new <- read_waterdata_parameter_codes(parameter_code = pcodes)
#' ref_list <- check_waterdata_sample_params("reference-list")
#' ref_list_sm <- ref_list[ref_list$parm_cd %in% pcodes, ]
#'
#' }
pcode_to_name <- function(parameterCd = "all") {
  .Deprecated(
    new = "check_waterdata_sample_params",
    package = "dataRetrieval",
    msg = "Convert to check_waterdata_sample_params('reference-list')."
  )
}
