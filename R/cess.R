#' Calculate estimates of costs and economies of scale and scope from 25 to 200 \% levels at means
#'
#' @param data The data used for calculating the estimates.
#' @param outputName A vector of strings containing the names of the independent (output) variables.
#' @param priceName A vector of strings containing the names of the independent (price) variables.
#' @param controlName A vector of strings containing the names of the control variables.
#' @param form A cost function character.
#' @param model The estimated model(nls class object).
#' @param vcovCL A variance matrix provided by clusterEst function
#' @return Estimates of scale and scope economies including their standard errors (SE), lower
#'         interval(Lo) and upper interval(Hi)
#' @author
#' Liang-Cheng Zhang
#' @references
#' Zhang, L.-C., Worthington, A. C., & Hu, M. (in press). Cost economies in the provision of
#' higher education for international students: Australian evidence. Higher Education. doi: \href{http://dx.doi.org/10.1007/s10734-016-0078-9}{10.1007/s10734-016-0078-9}
#'
#' Zhang, L.-C., & Worthington, A. C. (2015). Evaluating the accuracy of scope economies: comparisons among
#' delta method, bootstrap, and Bayesian approach. Paper presented at Australian Conference of Economists
#' PhD Colloquium. Retrieved from \href{http://www98.griffith.edu.au/dspace/handle/10072/69409}{http://www98.griffith.edu.au/dspace/handle/10072/69409}
#' @examples
#' ##Reproduce results of Zhang et al. (in press)
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
#' cess(data=data, outputName = colnames(unidat)[7:11],priceName = colnames(unidat)[4:6],
#' controlName = colnames(unidat)[12:24], model=model, vcovCL=vcovCL)
#'
#' @import car
#' @export

cess <- function(data, outputName, priceName, controlName, model, vcovCL=clusterEst(model = model , cluster = unidat$unicode)$vcovCL) {
model <- model
vcovCL <- vcovCL
mOutput <- list()
coefPrice <- vector()
for (i in 2:length(priceName)) { #starting from 2 since w1 is the numeriate price

  coefPrice[i] <- coef(model)[sprintf("%s",paste("bp",i, sep = ""))]
}

coefPrice[1] <- 1 - sum(as.numeric(coefPrice[-1])) #the above code is prepared for bp1 since deltamethod cannot extract it from the model.
cPrice <- as.numeric(coefPrice)

for (i in 1:length(outputName)) {

  mOutput[i] <- mean(data[,outputName[i]])
}
for (i in 1:length(priceName)) {

  priceName[i] <- mean(data[,priceName[i]])
}
for (i in 1:length(controlName)) {

  controlName[i] <- mean(data[,controlName[i]])
}
mOutput <- as.numeric(mOutput)
mPrice <- as.numeric(priceName)
mControl <- as.numeric(controlName)


# Preparing for the formula of cost economies estimates -------------------


control <-  list() # create control variables for cost function
price   <-  list() # create price items for cost function
single  <-  list() # create single items for cost function
square  <-  list() # create square items for cost function
for (i in 1:length(priceName)) { #starting from 2 since w1 is the numeriate price

  price[[i]] <- paste(paste("mw",i, sep = ""), paste("bp",i, sep = ""), sep =  "^")
}

for (i in 1:length(controlName)) {

  control[[i]] <- paste(paste("bz",i, sep = ""),paste("mz",i, sep = ""), sep =  "*")
}

for (i in 1:length(outputName)) {

  single[[i]] <- paste(paste("b",i, sep = ""),outputName[i], sep =  "*")
}

for (i in 1:length(outputName)) {

  square[[i]] <- paste(paste("b",i,i, sep = ""),0.5,paste(outputName[i],"^2",sep = ""), sep =  "*")
}

Y = combn(outputName,2)
B = combn(length(outputName),2)
Multi <- list() # create multiplication items for cost function
for (i in 1:ncol(Y)) {

  Multi[[i]] <- paste(paste("b",B[1,i],B[2,i], sep = ""),Y[1,i],Y[2,i], sep = "*")
}
QCF <- do.call("paste",c("b0",single, square, Multi, sep = "+"))
QCF <- parse(text = QCF)
for (i in 1:length(priceName)) {

  assign( paste("mw",i,sep = ""), mPrice[i],pos = .GlobalEnv)
}

for (i in 1:length(controlName)) {

  assign( paste("mz",i,sep = ""), mControl[i],pos = .GlobalEnv)
}

for (i in 1:length(priceName)) {

  assign( paste("bp",i,sep = ""), coefPrice[i],pos = .GlobalEnv)
}


#the followings codes are loops for calculating estimates from 50% to 200% levels at means----
p <- 0
counter <- 0
repeat {
  counter <- counter + 1
  p <- p + 0.25
  if (p > 2) break;
  m.Output <- mOutput*p #percentage of output mean. For example, if p is 1, it means 100% of output mean

  for (i in 1:length(outputName)) {

    assign( paste("y",i,sep = ""), m.Output[i],pos = .GlobalEnv)
  }
  #Calculate different types of costs with the numbers calculated above------
  #_ Tcost ---------------------------------------------------------------
  Tcost <- do.call("paste",
                   c(price,
                     sprintf("exp(%s)",do.call("paste",c(control, sep = "+"))),
                     sprintf("(%s)",do.call("paste",c("b0",single, square, Multi, sep = "+"))),
                     sep = "*"))  #Total costs
  #_ mcy ---------------------------------------------------------------
  mcy <-  list() # create marginal cost variables
  for (i in 1:length(outputName)) {
    DF <- D(QCF, outputName[i])
    F1 <- as.character(DF)
    mcy[[i]] <- do.call("paste",
                        c(price,
                          sprintf("exp(%s)",do.call("paste",c(control, sep = "+"))),
                          sprintf("(%s)",paste(F1[2],F1[3], sep = "+")),
                          sep = "*"))
  }
  # _ ICy ---------------------------------------------------------------
  # using formula (2) in cost economies of international paper
  ICy <-  list() # create incremental marginal cost variables
  Multi2 <- list()
  Yij <-  list()
  for (i in 1:length(outputName)) {

    Yi <- B[,which(B[1,] == i)]
    Yj <- B[,which(B[2,] == i)]
    Yij[[i]] <- cbind(Yi, Yj)
    Multi2[[i]] <- i }

    for (i in 1:length(outputName)) {
    for (j in 1:ncol(Yij[[i]])) {
      Multi2[[i]][j] <- paste(paste("b",Yij[[i]][1,j],Yij[[i]][2,j], sep = ""),paste("y",Yij[[i]][1,j],sep = ""),paste("y",Yij[[i]][2,j], sep = ""), sep = "*")
      ICy[[i]] <- do.call("paste",
                          c(price,
                            sprintf("exp(%s)",do.call("paste",c(control, sep = "+"))),
                            sprintf("(%s)",do.call("paste",c(single[i],square[i],Multi2[[i]], sep = "+"))),
                            sep = "*"))
    }
  }
  #_ Cy ---------------------------------------------------------------
  Cy <-  list() # create costs of producing a specific output cost variables
  for (i in 1:length(outputName)) {
    Cy[[i]] <- do.call("paste",
                        c(price,
                          sprintf("exp(%s)",do.call("paste",c(control, sep = "+"))),
                          sprintf("(%s)",paste("b0",single[[i]],square[[i]], sep = "+")),
                          sep = "*"))
  }

  #_ Cy_---------------------------------------------------------------
  Cy_ <-  list() # create incremental marginal cost variables

  Multi2 <- list()
  Yij <-  list()
  for (i in 1:length(outputName)) {

    B1  <- B[,which(B[1,] != i)]
    Yij[[i]] <- B1[,which(B1[2,] != i)]
    Multi2[[i]] <- i
  }
  for (i in 1:length(outputName)) {
    for (j in 1:ncol(Yij[[i]])) {

      Multi2[[i]][j] <- paste(paste("b",Yij[[i]][1,j],Yij[[i]][2,j], sep = ""),paste("y",Yij[[i]][1,j],sep = ""),paste("y",Yij[[i]][2,j], sep = ""), sep = "*")
      Cy_[[i]] <- do.call("paste",
                          c(price,
                            sprintf("exp(%s)",do.call("paste",c(control, sep = "+"))),
                            sprintf("(%s)",do.call("paste",c("b0",single[-i],square[-i],Multi2[[i]], sep = "+"))),
                            sep = "*"))
    }
  }

  #Calculate the degree of scale and scope economies with the estimated parameters and sample data-----------
  parameterNames = list(paste("b", 0:(11*length(outputName)), sep = ""),paste("bp", 1:length(priceName), sep = ""),paste("bz", 1:length(controlName), sep = ""))
  #_MC for outputs-------------------
  for (i in 1:length(outputName)) {

    assign( paste("MCy",i,sep = ""), car::deltaMethod(coef(model), mcy[[i]], parameterNames = parameterNames, vcov. = vcovCL ))
  }
  #_AIC for outputs-------------------
  for (i in 1:length(outputName)) {

    assign( paste("AICy",i,sep = ""), car::deltaMethod(coef(model), paste(ICy[[i]],"/",m.Output[[i]], sep = ""), parameterNames = parameterNames, vcov. = vcovCL ))
  }

  #_Ray Scale Economies------------
  mymcy<-list()
  for (i in 1:length(outputName)) {

    mymcy[[i]] <- paste(paste("y",i,sep = ""),mcy[i], sep =  "*")
  }
  SRAY <- car::deltaMethod(coef(model), paste(Tcost,"/(",do.call("paste",c(mymcy, sep = "+")),")", sep = ""), parameterNames = parameterNames, vcov. = vcovCL)
  K = sum(lengths(parameterNames))
  #_product-specific scale economies for outputs-------
  for (i in 1:length(outputName)) {

    assign( paste("PSCEy",i,sep = ""), car::deltaMethod(coef(model), paste(ICy[[i]],"/(",m.Output[[i]],"*",mcy[[i]],")", sep = ""), parameterNames = parameterNames, vcov. = vcovCL ))
  }

  #_Global Scope Economies-------------
  GSE <- car::deltaMethod(coef(model), paste("((",do.call("paste",c(Cy, sep = "+")),")-(",Tcost,"))/(",Tcost,")", sep = ""), parameterNames = parameterNames, vcov. = vcovCL)
  #_Product-specific scope economies for outputs---------
  for (i in 1:length(outputName)) {

    assign( paste("PSOEy",i,sep = ""), car::deltaMethod(coef(model), paste("(",Cy[[i]],"+",Cy_[[i]],"-",Tcost,")/(",Tcost,")", sep = ""), parameterNames = parameterNames, vcov. = vcovCL ))
  }

  pointEstimates <- list()
  for (i in 1:length(outputName)) {
    pointEstimates[[i                       ]] <- get(paste("PSCEy",i,sep = ""))$Estimate
    pointEstimates[[i + 1*length(outputName)]] <- get(paste("PSCEy",i,sep = ""))[[2]]
    pointEstimates[[i + 2*length(outputName)]] <- get(paste("PSCEy",i,sep = ""))[[3]]
    pointEstimates[[i + 3*length(outputName)]] <- get(paste("PSCEy",i,sep = ""))[[4]]
    pointEstimates[[i + 4*length(outputName)]] <- get(paste("PSOEy",i,sep = ""))$Estimate
    pointEstimates[[i + 5*length(outputName)]] <- get(paste("PSOEy",i,sep = ""))[[2]]
    pointEstimates[[i + 6*length(outputName)]] <- get(paste("PSOEy",i,sep = ""))[[3]]
    pointEstimates[[i + 7*length(outputName)]] <- get(paste("PSOEy",i,sep = ""))[[4]]
    pointEstimates[[1 + 8*length(outputName)]] <- SRAY$Estimate
    pointEstimates[[2 + 8*length(outputName)]] <- SRAY[[2]]
    pointEstimates[[2 + 8*length(outputName)]] <- SRAY[[3]]
    pointEstimates[[3 + 8*length(outputName)]] <- SRAY[[4]]
    pointEstimates[[4 + 8*length(outputName)]] <- GSE$Estimate
    pointEstimates[[5 + 8*length(outputName)]] <- GSE[[2]]
    pointEstimates[[5 + 8*length(outputName)]] <- GSE[[3]]
    pointEstimates[[6 + 8*length(outputName)]] <- GSE[[4]]

    names(pointEstimates[[i                       ]]) <- paste("PSCEy",i,sep = "")
    names(pointEstimates[[i + 1*length(outputName)]]) <- paste("SE_PSCEy",i,sep = "")
    names(pointEstimates[[i + 2*length(outputName)]]) <- paste("Lo_PSCEy",i,sep = "")
    names(pointEstimates[[i + 3*length(outputName)]]) <- paste("Hi_PSCEy",i,sep = "")
    names(pointEstimates[[i + 4*length(outputName)]]) <- paste("PSOEy",i,sep = "")
    names(pointEstimates[[i + 5*length(outputName)]]) <- paste("SE_PSOEy",i,sep = "")
    names(pointEstimates[[i + 6*length(outputName)]]) <- paste("Lo_PSOEy",i,sep = "")
    names(pointEstimates[[i + 7*length(outputName)]]) <- paste("Hi_PSOEy",i,sep = "")
    names(pointEstimates[[1 + 8*length(outputName)]]) <- "SRAY"
    names(pointEstimates[[2 + 8*length(outputName)]]) <- "SE_SRAY"
    names(pointEstimates[[2 + 8*length(outputName)]]) <- "Lo_SRAY"
    names(pointEstimates[[3 + 8*length(outputName)]]) <- "Hi_SRAY"
    names(pointEstimates[[4 + 8*length(outputName)]]) <- "GSE"
    names(pointEstimates[[5 + 8*length(outputName)]]) <- "SE_GSE"
    names(pointEstimates[[5 + 8*length(outputName)]]) <- "Lo_GSE"
    names(pointEstimates[[6 + 8*length(outputName)]]) <- "Hi_GSE"
  }
  PointEstimates <- t(do.call(rbind, lapply(pointEstimates, data.frame, stringsAsFactors = FALSE)))
  write.table(PointEstimates, file = "point Estimates of scale and scope economies.csv", row.names = p, col.names = NA, sep = ",",append = TRUE)
 }
 Estimates <- read.csv("point Estimates of scale and scope economies.csv", header = T,stringsAsFactors = FALSE)
 Estimates <- Estimates[seq(from = 1,to = 15,by = 2),]
 colnames(Estimates) <- c("meanLevels",colnames(Estimates)[2:(7 + 8*length(outputName))])
 Estimates <- as.data.frame(apply(Estimates,2,as.numeric))
 file.remove("point Estimates of scale and scope economies.csv")
 return(Estimates)
}
