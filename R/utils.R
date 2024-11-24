#' Set User Agent
#'
#' \code{setUserAgent} Set user agent header.
#'
#' @usage setUserAgent(ua)
#'
#' @param ua user agent string.
#'
#' @return NONE
#'
#' @examples
#' \dontrun{
#'
#' setUserAgent('jbennett@jbennettconsulting.com')
#'}
setUserAgent <- function(ua) {
  options(HTTPUserAgent = ua)
}

#' CIK to 10 digit
#'
#' \code{cik.to.10} Convert cik to 10 digit.
#'
#' \@usage cik.to.10(cik)
#'
#' @param cik CIK number
#'
#' @return The 10-digit cik version.
#'
#' @examples
#' \dontrun{
#'
#' cik.to.10(1321655)
#'}
cik.to.10 <- function(cik) {
  if(nchar(cik) < 10){
    nzeros <- 10 - nchar(cik)
    cik <- paste(c(rep('0', nzeros), cik), collapse='')
  }
  return(cik)
}
