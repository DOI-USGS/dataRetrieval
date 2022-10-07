#' Pad string with leading zeros
#'
#' Function to pad a string with leading zeros. Useful for parameter codes and USGS site IDs.
#'
#' @param x character
#' @param padTo number Final desired length of the character
#' @keywords data import USGS web service
#' @return x character returned with leading zeros
#' @export
#' @examples
#' pCode <- "10"
#' correctPCode <- zeroPad(pCode, 5)
#' pCodes <- c("100", "1000", "0", "12345", "1565465465465465")
#' correctPCodes <- zeroPad(pCodes, 5)
#' pCodeNA <- c(1, 2, NA)
#' padPCodeNA <- zeroPad(pCodeNA, 4)
zeroPad <- function(x, padTo) {
  if (padTo <= 1) {
    return(x)
  }

  numDigits <- nchar(x, keepNA = TRUE)
  padding <- padTo - numDigits

  if (anyNA(padding)) {
    padding[is.na(padding)] <- 0
  }

  paddingZeros <- vapply(
    X = padding[padding > 0],
    FUN = function(y) paste0(rep("0", y), collapse = ""),
    FUN.VALUE = ""
  )

  x[padding > 0] <- paste0(paddingZeros, x[padding > 0])
  return(x)
}
