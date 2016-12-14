#' Generate the right format of cost function for calculating economies of scale and scope.
#'
#' @param costName A character string containing the name of the dependent (cost) variable.
#' @param outputName A vector of strings containing the names of the independent (output) variables.
#' @param priceName A vector of strings containing the names of the independent (price) variables.
#' @param controlName A vector of strings containing the names of the control variables.
#' @param form A cost function character.
#' @return An object of class "formula" consisting of \code{costName}, \code{outputName} and \code{priceName} based on varied functional forms.
#' @author
#' Liang-Cheng Zhang
#' @references
#' Mayo, J. W. (1984). Multiproduct monopoly, regulation, and firm costs. Southern Economic Journal, 51(1), 208-218. doi:10.2307/1058333
#'
#' Zhang, L.-C., Worthington, A. C., & Hu, M. (in press). Cost economies in the provision of higher education for international students: Australian evidence. Higher Education. doi: \href{http://dx.doi.org/10.1007/s10734-016-0078-9}{10.1007/s10734-016-0078-9}
#' @details This function returns flexible fixed cost quadratic (FFCQ) function formula based on the classification of Mayo (1984). You can find the applications in Zhang et al. (in press).
#'
#' @examples
#' ##Specifiy arguments with user-identified names
#' costFunction(costName="c",outputName = c("y1","y2"))
#' costFunction(costName="c",outputName = c("y1","y2","y3"),priceName = c("w1","w2","w3"),controlName = c("z1","z2","z3"), form="FFCQ-M")
#'
#' ##Specifiy arguments with data' column names
#' costFunction(costName=colnames(unidat)[3],outputName = colnames(unidat)[7:11],priceName = colnames(unidat)[4:6],controlName = colnames(unidat)[12:24], form="FFCQ-M")
#' @export
#' @importFrom stats as.formula
#' @importFrom utils combn


costFunction <- function(costName, outputName, priceName, controlName, form=c("FFCQ-M")) {
  form    <- match.arg(form)
  control <-  list() # creat control variables for cost function
  price   <-  list() # creat price items for cost function
  single  <-  list() # creat single items for cost function
  square  <-  list() # creat square items for cost function
  for (i in 2:length(priceName)) { #starting from 2 since w1 is the numeriate price

    price[[i - 1]] <- paste(paste("bp",i, sep = ""),sprintf("log(%s/%s)",priceName[i],priceName[1]), sep =  "*")
  }

  for (i in 1:length(controlName)) {

    control[[i]] <- paste(paste("bz",i, sep = ""),controlName[i], sep =  "*")
  }

  for (i in 1:length(outputName)) {

    single[[i]] <- paste(paste("b",i, sep = ""),outputName[i], sep =  "*")
  }

  for (i in 1:length(outputName)) {

    square[[i]] <- paste(paste("b",i,i, sep = ""),0.5,paste(outputName[i],"^2",sep = ""), sep =  "*")
  }

  Y = combn(outputName,2)
  B = combn(length(outputName),2)
  Multi <- list() # creat multiplication items for cost function
  for (i in 1:ncol(Y)) {

    Multi[[i]] <- paste(paste("b",B[1,i],B[2,i], sep = ""),Y[1,i],Y[2,i], sep = "*")
  }


  RHS <- do.call("paste",
                 c(paste("log(",priceName[1],")",sep = ""),
                   price,
                   control,
                   sprintf("log(%s)",do.call("paste",c("b0",single, square, Multi, sep = "+"))),
                   sep = "+")) # items on the right hand side of ~

  if (form == "FFCQ-M") {

    cost.f <- as.formula(paste(sprintf("log(%s)",costName), RHS , sep = "~"))
    return(cost.f)
  }


}



