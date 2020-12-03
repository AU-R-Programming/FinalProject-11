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
#"An Introduction to Statistical Programming Methods with R"
#Matthew Beckman, Stéphane Guerrier, Justin Lee, Roberto Molinari,
#Samuel Orso & Iegor Rudnytskyi (2020-10-20)
#https://smac-group.github.io/ds/section-functions.html
# "Draw Residual Plot in R Example Tutorial" from 'The Data Science Show'
# https://www.youtube.com/watch?v=WBc40XRGluU
#"Statistical modelling in R", JJ Valletta and TJ McKinley (22 November 2018)
#https://exeter-data-analytics.github.io/StatModelling/linear-models.html


n=1000
p=2
X=matrix(NA, nrow=n,ncol=p)
for (i in 1:p) {
  set.seed(i+200)
  X[,i]=sample(x, n, replace=T)
}
set.seed(742)
beta=rpois(p, lambda = 10)
beta
set.seed(165)
error=rnorm(n, sd=1)
y=X%*%beta+error

#Outputes all linear regression material including plots in a certain order
Linear_Regr_Plot=function(beta, y, X, alpha=0.05) {
  #Conform both y and X as respective objects to avoid any conformity programming issues
  y <- as.vector(y)
  X <- as.matrix(X)
#First we will construct the least square estimator for y and X for any given beta paramater vector
Least_Sq_Loss=function (betaVal, yVal, XVal) {
  
  if (length(XVal[1,]) != length(beta) || length(XVal%*%betaVal) != length(y)) {
    stop("Length of vectors are not compatible to each other. Please correct")
  }  
  t(yVal-XVal%*%betaVal)%*%(yVal-XVal%*%betaVal)
}
#Here we optimize Least_Sq_Loss and place that as a variable 
Optim_Least_Sq=function (Beta, yVal, XVal) {
  betamin=optim(Beta, Least_Sq_Loss, yVal=y, XVal=X)$par
  return(betamin)
}
OptimBeta=Optim_Least_Sq(rep(0,p), yVal=y, XVal=X)

#Calculates degrees of freedom
FirstDF=length(beta)-1
df=length(y)-length(OptimBeta)
# Compute residuals
ResidVal <- y - X%*%as.matrix(OptimBeta) 
VarHat <- as.numeric((1/df)*(t(y)%*%y))
# Estimate of the variance of the estimated beta
VarBetaHat=VarHat*(t(X)%*%X)
#Calculates fitted values
FittedVal=as.matrix(X%*%OptimBeta)

#Calculates confidence interval for beta in linear regression
ConfidInter.LinRegr=function(OptimBeta, VarBetaHat, alpha) {
  Quadrant=1-alpha/2
  ConfINT=c(OptimBeta - qnorm(p = Quadrant)*sqrt(VarBetaHat), OptimBeta + 
              qnorm(p = Quadrant)*sqrt(VarBetaHat))
  return(ConfINT)
}
CIMod=ConfidInter.LinRegr(OptimBeta=OptimBeta, VarBetaHat=VarBetaHat, alpha=alpha)

#Residual vs Fitted Plot: Creates and plots out residual vs fitted plot, QQ plot and residual histogram
ModelFitvResid=function(FitValues, ResValues) {
  ModelData=data.frame(FitValues=FitValues, ResValues=ResValues)
  LinModRegr = lm(FitValues~ResValues, data=ModelData)
  return(LinModRegr)
}
MainLinRegr=ModelFitvResid(FittedVal, ResidVal)

#Creating the Mean Square Prediction Error (MSPE) computed in matrix form:
MSPE_Function=function(y, X, FittedVal) {
  PartMSPE=0
  DummyVar=0
  for (i in 1:n) {
         DummyVar=(y[i]-FittedVal[i])^2
         PartMSPE=PartMSPE+DummyVar
  }
  MSPEFinal=n^{-1}*PartMSPE
return(MSPEFinal)
}
MSPE_Val=MSPE_Function(y, X, FittedVal) 

F_Calc=function(y, X, FitValues) {
  SampleMean=mean(y)
  SSM_Val=0
  SSE_Val=0
  DummyVarSSM_Val_1=0
  DummyVarSSE_Val_2=0
  for (i in 1:n) {
    DummyVarSSM_Val_1=(FitValues[i]-SampleMean)^2
    DummyVarSSE_Val_2=(y[i]-FitValues[i])^2
    SSM_Val=SSM_Val+DummyVarSSM_Val_1
    SSE_Val=SSE_Val+DummyVarSSE_Val_2
  }
  DFM=FirstDF
  DFE=df
  MSM=SSM_Val/DFM
  MSE=SSE_Val/DFE
  #The actual F statistics will be computed here
  F_Test_Stat=MSM/MSE
  #The stat wil be compared to the real one, retrieved from the linear model previously created
  F_Real_Stat=summary(MainLinRegr)$fstatistic[1]
  ProbTestF=pf(F_Test_Stat, DFM, DFE, 1)
  F.Info=rbind(F_Test_Stat, DFM, DFE,ProbTestF)
  rownames(F.Info)=c("F Stat: ","Lower DF: ","Higher DF:","P-Value: ")
 return(F.Info) 
}
F_Data=F_Calc(y, X, FittedVal)

#AllData=rbind(CIMod, MSPE_Val, F_Data)
par(mfrow=c(1,3),oma = c(1,0,1,0))
HistResVFit=hist(ResidVal, xlab = "Residual Values", ylab="Frequency")
title(line = -2)
#plot(HistResVFit)
plot(MainLinRegr,c(2,1))



return(list(beta = OptimBeta, MSPE = MSPE_Val, 
            variance_beta = VarBetaHat, ci = CIMod, F.Stat=F_Data))

}

#Example Value to validate results closely match
Reg_Lin=lm(y~X-1)
Our_Lin_Regr=Linear_Regr_Plot(beta, y, X, alpha=0.05)

# Compare outputs
manual_results = c(Our_Lin_Regr$beta, Our_Lin_Regr$variance_beta)
base_results = c(Reg_Lin$coefficients, 
                 (1/Reg_Lin$df.residual)*t(Reg_Lin$residuals)%*%Reg_Lin$residuals)
results = cbind(manual_results, base_results)
row.names(results) = c("Beta", "Sigma")
results


