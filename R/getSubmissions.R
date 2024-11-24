#' Using the EDGAR API, download a list of submissions by this company.
#'
#' \code{getSubmissions} retrieve the list of reports submitted by this company.
#'
#' getSubmissions takes the cik of the firm as an input parameter and provides
#' information about the reports submitted to EDGAR by the firm.
#'
#' @usage getSubmissions(cik)
#'
#' @param cik the cik number of the firm.
#'
#' @return dataframe list of all submitted reports by the firm.
#'
#' @examples
#' \dontrun{
#'   pltr.df <- getSubmissions(1321655)
#'}

getSubmissions <- function(cik) {
  cik <- cik.to.10(cik)
  edgar.url <- url(paste0('https://data.sec.gov/submissions/CIK', cik, '.json'))
  edgar.json <- readLines(edgar.url, warn=F)
  parser <- newJSONParser()
  parser$addData(edgar.json)
  edgar.data <- parser$getObject()
  fields <- names(edgar.data$filings$recent)
  as.data.frame(do.call(cbind, lapply(edgar.data$filings$recent, function(x) x)))
}
