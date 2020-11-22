library(knitr)
library(kableExtra)
library(lubridate)
library(tidyverse)
library(dplyr)
library(expss)
library(ggplot2)
library(ggmap)
library(leaflet)
#Sources for functions below
#
#
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/var.test.html
#
#
The_Linear_Regr=function (Data1, Data2) {
  fit=glm(formula=Data1,data=Data2,family=binomial())
  return(plot(fit))
}

F_Test=function(DataVec1, DataVec2) {
var.test(DataVec1, DataVec2, ratio = 1,
         alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95, ...)
  return()
}

ResVsFit=function(Var1, Var2) {

  plot(lm(Var1~Var2,data=cars))
  return()
}
