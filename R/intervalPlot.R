#' Generate interval plot for inferring the existence of economies of scale or scope
#'
#' @param intervalData The data used for plotting intervals.
#' @param estimates A vector of estimates which you want to plot with.
#' @param meanLevels A vector of percentage of output mean.
#' @param lowerLevel A vector of lower confidence intervals.
#' @param upperLevel A vector of upper confidence intervals.
#' @param h A horizontal red line for inferring the existence of economies of scale (=1) or scope (=0).
#' @param ylab Y axis label.
#' @param ylim Y axis limit.
#' @return A interval plot
#' @author
#' Liang-Cheng Zhang
#' @references
#' Zhang, L.-C., Worthington, A. C., & Hu, M. (in press). Cost economies in the
#' provision of higher education for international students: Australian evidence. Higher Education. doi: \href{http://dx.doi.org/10.1007/s10734-016-0078-9}{10.1007/s10734-016-0078-9}
#'
#' Zhang, L.-C. (2015). Inferring (dis)economies of scope with a
#' proper procedure: using an interval plot as an alternative solution. Paper presented
#' at the 2015 AFE PhD Research Symposium in Economics, Economics and Business Statistics
#' Discipline, Griffith Business School, Griffith University. \href{https://www.researchgate.net/publication/279849568_Inferring_diseconomies_of_scope_with_a_proper_procedure_Using_an_interval_plot_as_an_alternative_solution}{PDF Link}
#' @examples
#' #interval plot for GSE
#' data(unidat)
#' data = unidat
#' library(minpack.lm)
#' model <- nlsLM(costFunction(costName = colnames(unidat)[3], outputName = colnames(unidat)[7:11],
#' priceName = colnames(unidat)[4:6], controlName = colnames(unidat)[12:24],
#' form = "FFCQ-M"), start = list(b0 = 600, b1 = 0, b2 = 0,
#'                                b3 = 0, b4 = 0, b5 = 0, b11 = 0, b22 = 0, b33 = 0, b44 = 0,
#'                                b55 = 0, b12 = 0, b13 = 0, b14 = 0, b15 = 0, b23 = 0, b24 = 0,
#'                                b25 = 0, b34 = 0, b35 = 0, b45 = 0, bp2 = 0, bp3 = 0, bz1 = 0,
#'                                bz2 = 0, bz3 = 0, bz4 = 0, bz5 = 0, bz6 = 0, bz7 = 0, bz8 = 0,
#'                                bz9 = 0, bz10 = 0, bz11 = 0, bz12 = 0, bz13 = 0), data = unidat,
#'                                trace = F)
#' vcovCL <- clusterEst(model = model , cluster = unidat$unicode)$vcovCL
#' ##interval plot for GSE
#' intervalPlot(intervalData = intervalData, estimates = intervalData$GSE,
#' meanLevels = intervalData$meanLevels, lowerLevel = intervalData$Lo_GSE,
#' UpperLevel = intervalData$Hi_GSE, ylab = "Degree of economies of scope",
#' h = 0,ylim = c(-1,1))
#'
#' ##interval plot for SRAY
#' intervalPlot(intervalData = intervalData, estimates = intervalData$SRAY,
#' meanLevels = intervalData$meanLevels, lowerLevel = intervalData$Lo_SRAY,
#' UpperLevel = intervalData$Hi_SRAY, ylab = "Degree of economies of scale",
#' h = 1,ylim = c(min(intervalData$Lo_SRAY,1),max(intervalData$Hi_SRAY)))
#' @details
#' This function generates a interval plot (Zhang, 2015) to infer the existence of scale and scope economies.
#' If the intervals of point estimates do not include one for scale economies or zero for the scope economies,
#' it suggests that these estimates are significantly different from the thresholds (one and zero,
#' respectively) at the 5\% significance level. Thus, there is evidence for the existence of scale
#' and scope economies (if their intervals are higher than the threshold) or diseconomies (if their
#' intervals are lower than the threshold).
#' @export
intervalPlot <- function(intervalData = intervalData, estimates,
  meanLevels = intervalData$meanLevels, lowerLevel = intervalData$Lo_GSE,
  UpperLevel = intervalData$Hi_GSE, ylab, h, ylim = c(-1,1)) {

  plot(estimates ~ meanLevels, intervalData, yaxt = "n", type = "b",
    xaxt = "n", ylim = ylim, xlim = c(0.25, 2), pch = 16,
    col = "blue", xlab = "Percentage of output mean", ylab = ylab)
  axis(side = 2, las = 1)
  axis(1, at = seq(0.25, 2, by = 0.25), labels = c("25%", "50%",
    "75%", "100%", "125%", "150%", "175%", "200%"))
  abline(h = h, col = "red")
  lines(intervalData$meanLevels, lowerLevel, col = "blue",
    lty = 2, lwd = 2)
  lines(intervalData$meanLevels, UpperLevel, col = "blue",
    lty = 2, lwd = 2)

}
