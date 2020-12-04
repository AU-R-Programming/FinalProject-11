#Residual vs Fitted Plot: Creates and plots out residual vs fitted plot, QQ plot and residual histogram
ModelFitvResid=function(y, X) {
  OptimBeta=Least_Sq_Loss(y, X)
  ResidVals <- y - X%*%as.matrix(OptimBeta)
  FitValues=X%*%OptimBeta
  #AllData=rbind(CIMod, MSPE_Val, F_Data)
  par(mfrow=c(1,3),oma = c(1,0,1,0))
  plot(ResidVals~FitValues, xlab="Residual Values",ylab="Fitted Values",main="Residuals vs Fitted")
  qqnorm(ResidVals)
  HistResVFit=hist(ResidVal, xlab = "Residual Values", ylab="Frequency")
  title(line = -2)
}
