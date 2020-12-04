#Calculates confidence interval for beta in linear regression
ConfidInter.LinRegr=function(Beta, y, X, alpha=0.05, Bootstrap=TRUE) {
#Takes size of parameter vector
  Bootstrap=as.logical(Bootstrap)
   n=length(y)
  p=length(Beta)
  size=1000
  df=n-p
   # Compute residuals
  ResidVals <- y - X%*%as.matrix(Beta)
  SigmaHat <- as.numeric((1/df)*t(ResidVals)%*%ResidVals)
  # Estimate of the variance of the estimated beta from Eq. (1.2)
  VarBetaHat <- SigmaHat*solve(t(X)%*%X)
  Quadrant=1-alpha/2
  ConfINTBeta=rep(NA, p)
  if (Bootstrap==TRUE) {
    Vec=rep(NA,size)
    for (i in 1:size) {
      ConfidInter=Beta[sample(1:p, replace = TRUE)]
  Vec[i]=mean(ConfidInter)
    }
    print("This is Vec")
    print(Vec)
    ConfINTBeta=quantile(Vec, c(1-Quadrant, Quadrant))
   #The method selected if Bootsratp is FALSE is asymptotic, conducted below
   } else {
     ConfINTBeta=c(Beta - qnorm(p = Quadrant)*sqrt(VarBetaHat), Beta + 
                 qnorm(p = Quadrant)*sqrt(VarBetaHat))
     print("This is Bootstrap")
     print(ConfINTBeta)
   }
  #After method chosen runs, then the resulting ConfINTBeta will be outputted by function at the end
   return(ConfINTBeta)
}