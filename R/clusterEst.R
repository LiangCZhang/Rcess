#' Calculate coefficients and covariance with clusting standard deviations
#'
#' @param model The estimated model(nls class object).
#' @param cluster A vector, matrix, or data.frame of cluster variables, where each column is a separate variable.
#' @return An list of estimation results and clustering variance
#' @author
#' Liang-Cheng Zhang
#' @references
#' Arai, M. (2015). Cluster-robust standard errors using R.
#' Retrieved from \href{http://www.ne.su.se/polopoly_fs/1.216115.1426234213!/menu/standard/file/clustering1.pdf}{http://www.ne.su.se/polopoly_fs/1.216115.1426234213!/menu/standard/file/clustering1.pdf}
#'
#' Petersen, M. A. (2009). Estimating standard errors in finance panel data sets: Comparing approaches. Review of Financial Studies, 22(1), 435-480.
#' @examples
#' ##Simple example
#' data(unidat)
#' m1 = nls(c ~ b0+b1*y1,start=list(b0=1,b1=0), data = unidat)
#' cluster.vcov(m2, petersen$firmid)
#'
#' ##Reproduce results of Table 4 in Zhang et al. (in press)
#' data(unidat)
#' library(minpack.lm)
#' model <- nlsLM(costFunction(costName=colnames(unidat)[3],outputName = colnames(unidat)[7:11],priceName = colnames(unidat)[4:6],controlName = colnames(unidat)[12:24], form="FFCQ-M"),
#'                , start=list(b0=600,b1=0,b2=0,b3=0,b4=0,b5=0,b11=0,b22=0,b33=0,b44=0,b55=0,b12=0,b13=0,b14=0,b15=0,b23=0,b24=0,b25=0,b34=0,b35=0,b45=0,bp2=0,bp3 = 0,bz1 = 0,bz2 = 0,bz3 = 0,bz4 = 0,bz5 = 0,bz6 = 0,bz7 = 0,bz8 = 0,bz9 = 0,bz10 = 0,bz11 = 0,bz12 = 0,bz13 = 0)
#'                ,data=unidat,trace=F)
#' clusterEst(model = model , cluster = unidat$unicode)$model #extract summary results
#' clusterEst(model = model , cluster = unidat$unicode)$vcovCL #extract covariance
#' @import lmtest sandwich
#' @export

clusterEst <- function(model, dfcw = 1, cluster){
  library(sandwich)
  library(lmtest)
  K <- length(coef(model)) #K (regressors which should depends on model) should be model$rank. it is done since there is no rank argument in NLS
  M <- length(unique(cluster))
  N <- length(cluster)
  dfc <- (M/(M - 1))*((N - 1)/(N - K))
  u <- apply(estfun(model),2,
             function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich::sandwich(model, meat = crossprod(u)/N)*dfcw
  list(model = lmtest::coeftest(model, vcovCL), vcovCL = vcovCL)

  }

