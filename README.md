<!-- README.md is generated from README.Rmd. Please edit that file -->
Rcess
=====

This package utilizes the theory of economies of scale and scope (developed by Baumol, Panzar, & Willig (1982)) to generate the estimates of average output costs, economies of scale, and economies of scope for different types of multi-output production industries. So far, this package can reproduce the estimates using FFCQ-M cost function (see Zhang, Worthington, and Hu (2017) for details). This package also offers a quick plotting method (Zhang, 2015) to infer the existence of economies of scale and scope. A quick demonstration is also shown.

Brief introduction
==================

Estimating scale and scope economies will provide firms with valuable information about the modifications in scale and scope. If there are economies of scale, all else constant, unit costs are lower in larger firms, resulting from the spreading of fixed costs over additional units of production. In other words, the existence of scale economies implies that the company can benefit from expanding its size. Except for deciding how large scale a firm could be, the owner also has to decide the scope of firm’s operation, including the extent of vertical or horizontal integration as well as the diversity of products. The estimates of global economies of scope (GSE) could help make this decision. When a firm enjoys the cost savings from the joint production of these varied products, GSE will be greater than zero. When there are economies of scope, it implies the cost of producing different types of products is less than the cost of producing them separately. On the contrary, when a company does not experience economies of scope, a multiproduct firm could be torn up into several specialized firms to save costs. Therefore, GSE could offer the nature of multiproduct production processes and the cost synergy that exist among outputs.

However, estimating the cost economies above is generally not easy. The investigation of scale and scope economies involves understanding their theories and the cost structure, including the types of outputs and the cost functional form needed in the estimation process. Once the estimates are gained, it will need to have the right tool to infer the existence of cost economies. We can clearly see that the investigation involves so many procedures which complicates the whole estimation process. Therefore, in this package (Rcess), we try to overcome these difficulties by easily generating the cost function forms (FFCQ-M) and providing a quick plotting method (Zhang, 2015) to infer the existence of cost economies.

Demo
====

In this section, we use Zhang, Worthington, and Hu (2017)'s study as an example. The first part teaches you how to install this package on R. The second part generates the estimates of cost economies (which are their results in Table 4 to Table 8) Notice that they only provides estimates of cost economies. In the remaining part of this section, we write a function to show the intervals of cost economies for inferring the existence of economies of scale and scope.

#### Installation

This package is currently hosted on Github (<https://github.com/LiangCZhang/Rcess>) where you will find its source code and manual. To install it, you need to type in the following code on your R console.

``` r
devtools::install_github("LiangCZhang/Rcess")
```

#### Generate estimates of costs and scale and scope economies

The following code chunk will produce the estimates of all costs and scale and scope economies (including their 95% confidence intervals) stored in the intervalData object.

``` r
library(Rcess)
data(unidat)
data = unidat
library(minpack.lm)
model <- nlsLM(costFunction(costName = colnames(unidat)[3], outputName = 
   colnames(unidat)[7:11], priceName = colnames(unidat)[4:6], controlName = 
   colnames(unidat)[12:24],    form = "FFCQ-M"), start = list(b0 = 600, 
   b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b11 = 0, b22 = 0, b33 = 0, b44 = 0,
   b55 = 0, b12 = 0, b13 = 0, b14 = 0, b15 = 0, b23 = 0, b24 = 0,
   b25 = 0, b34 = 0, b35 = 0, b45 = 0, bp2 = 0, bp3 = 0, bz1 = 0,
   bz2 = 0, bz3 = 0, bz4 = 0, bz5 = 0, bz6 = 0, bz7 = 0, bz8 = 0,
   bz9 = 0, bz10 = 0, bz11 = 0, bz12 = 0, bz13 = 0), data = unidat,
   trace = F)
vcovCL <- clusterEst(model = model, cluster = unidat$unicode)$vcovCL
intervalData <- cess(data = data, outputName = colnames(unidat)[7:11], 
                     priceName = colnames(unidat)[4:6], controlName = 
                    colnames(unidat)[12:24], model = model, vcovCL = vcovCL)
```

The following two sections demonstrate how to use the interval plots to infer the existence of cost economies. If the intervals of point estimates do not include one for scale economies or zero for the scope economies,it suggests that these estimates are significantly different from the thresholds (one and zero, respectively) at the 5% significance level. Thus, there is evidence for the existence of scale and scope economies (if their intervals are higher than the threshold) or diseconomies (if their intervals are lower than the threshold).

#### Interval plot for ray economies of scale (SRAY)

Running following code will produce a interval plot for ray economies of scale.

``` r
intervalPlot(intervalData = intervalData, estimates = intervalData$SRAY, meanLevels =
             intervalData$meanLevels, lowerLevel = intervalData$Lo_SRAY,UpperLevel = 
             intervalData$Hi_SRAY, ylab = "Degree of economies of scale", h = 1,ylim =  
             c(min(intervalData$Lo_SRAY,1),max(intervalData$Hi_SRAY)))
```

![Interval plot for ray economies of scale (SRAY)](README-fig1-1.png) The plot above clearly indicates that economies of scale exist up to 200% of mean output. This implies that we could achieve improvements in cost efficiency by expanding all outputs up to at least 200% of the current mean output.

#### Interval plot for economies of scope (GSE)

Running following code will produce a interval plot for economies of scope

``` r

intervalPlot(intervalData = intervalData, estimates = intervalData$GSE, meanLevels = 
             intervalData$meanLevels, lowerLevel = intervalData$Lo_GSE,
             UpperLevel = intervalData$Hi_GSE, ylab = "Degree of economies of scope",
             h = 0,ylim = c(-1,1))
```

![Interval plot for economies of scope (GSE)](README-fig2-1.png) From the plot above, we can infer that estimates of GSE exist but at only 25% of mean output. The value of 0.34 indicated at least a 34% reduction in total costs from joint production at 25% of mean output. The negative estimates appear once the mean output exceeds 50%.

References
==========

Baumol, W. J., Panzar, J. C., & Willig, R. D. (1982). Contestable markets and the theory of industry structure. New York, NY: Harcourt Brace Jovanovich.

Zhang, L.-C.(2015). Inferring (dis)economies of scope with a proper procedure: using an interval plot as an alternative solution. Paper presented at the 2015 AFE PhD Research Symposium in Economics, Economics and Business Statistics Discipline, Griffith Business School, Griffith University. [PDF Link](https://www.researchgate.net/publication/279849568_Inferring_diseconomies_of_scope_with_a_proper_procedure_Using_an_interval_plot_as_an_alternative_solution)

Zhang, L.-C., Worthington, A. C., & Hu, M. (2017). Cost economies in the provision of higher education for international students: Australian evidence. Higher Education, 74(4), 717-734. doi: [10.1007/s10734-016-0078-9](http://link.springer.com/article/10.1007/s10734-016-0078-9)
