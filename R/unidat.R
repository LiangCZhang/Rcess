#' Data of 37 Australian public universities
#'
#' A panel data containing the cost, outputs, and prices of 37 Australian public universities
#' over the period from 2003 to 2012
#'
#' @details
#'  This data is formatted based on Zhang et al. (in press) and collected from multiple sources. For outputs and control variables (y, q, z, and g),
#'  they are from Australian Government Department of Education and Training (n.d.-b)  For Total operating expenditure and price variables (c and w), they are
#'  from Australian Government Department of Education and Training (n.d.-a). Please see the data copyright
#'  here \href{https://www.education.gov.au/copyright} https://www.education.gov.au/copyright
#'
#' @format A data frame with 370 rows and 24 variables:
#' \describe{
#'   \item{year}{Data collection time point }
#'   \item{unicode}{Index of Australian universities}
#'   \item{c}{Total operating expenditure, in thousands of AUD (2003 = 100)}
#'   \item{w1}{Price of academic labour, in thousands of AUD (2003 = 100)}
#'   \item{w2}{Price of non-academic labour, in thousands of AUD (2003 = 100)}
#'   \item{w3}{Price of non-labour, in thousands of AUD (2003 = 100)}
#'   \item{y1}{Domestic science completions}
#'   \item{y2}{Domestic non-science completions}
#'   \item{y3}{Overseas science completions}
#'   \item{y4}{Overseas non-science completions}
#'   \item{y5}{Number of publications}
#'   \item{q}{Attrition rate, in percentages}
#'   \item{z1}{One for ATN institutions, otherwise zero}
#'   \item{z2}{One for Go8 institutions, otherwise zero}
#'   \item{z3}{One for IRU institutions, otherwise zero}
#'   \item{z4}{One for RUN institutions, otherwise zero}
#'   \item{g1}{One for Institutions located in NSW, otherwise zero}
#'   \item{g2}{One for Institutions located in VIC, otherwise zero}
#'   \item{g3}{One for Institutions located in QLD, otherwise zero}
#'   \item{g4}{One for Institutions located in WA, otherwise zero}
#'   \item{g5}{One for Institutions located in SA, otherwise zero}
#'   \item{g6}{One for Institutions located in ACT, otherwise zero}
#'   \item{g7}{One for Institutions located in TAS, otherwise zero}
#'   \item{g8}{One for Institutions located in NT, otherwise zero}
#' }
#' @source
#' Australian Government Department of Education and Training. (n.d.-a). Higher education publications: Finance
#' publication. Retrieved from \href{https://www.education.gov.au/finance-publication}https://www.education.gov.au/finance-publication
#'
#' Australian Government Department of Education and Training. (n.d.-b). uCube - Higher education statistics.
#' Retrieved from \href{http://highereducationstatistics.education.gov.au/Default.aspx}http://highereducationstatistics.education.gov.au/Default.aspx
#'
#' Zhang, L.-C., Worthington, A. C., & Hu, M. (in press). Cost economies in the provision
#' of higher education for international students: Australian evidence. Higher Education. doi: \href{http://dx.doi.org/10.1007/s10734-016-0078-9}{10.1007/s10734-016-0078-9}
"unidat"
