#' Using the EDGAR API, download a list of submissions by this company.
#'
#' \code{getCompanyConcept} retrieve the list of reports submitted by this company.
#'
#' getCompanyConcept takes the cik of the firm as an input parameter and provides
#' information about the reports submitted to EDGAR by the firm.
#'
#' @usage getCompanyConcept(cik, concept)
#'
#' @param cik the cik number of the firm.
#'
#' @param concept a taxonomy and tag in the form of 'taxonomy/tag'
#'
#' @return dataframe list of all submitted reports by the firm.
#'
#' @examples
#' \dontrun{
#'   pltr.df <- getCompanyConcept(1321655, 'us-gaap/AssetsCurrent')
#'}

getCompanyConcept <- function(cik, concept) {
  cik <- cik.to.10(cik)
  edgar.url <- paste0('https://data.sec.gov/api/xbrl/companyconcept/CIK',
                      cik, '/', concept, '.json')
  edgar.json <- readLines(edgar.url, warn=F)
  parser <- newJSONParser()
  parser$addData(edgar.json)
  edgar.data <- parser$getObject()
  fields <- c("end", "val", "accn", "fy", "fp", "form", "filed", "frame")
  do.call(rbind, lapply(edgar.data$units$USD, function(x) {
    x[setdiff(fields, names(x))] <- NA

    as.data.frame(x[fields], stringAsFactors = F)
  }))
}
