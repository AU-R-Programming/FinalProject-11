#Creating the Mean Square Prediction Error (MSPE) computed in matrix form:
MSPE_Function=function(y, X) {
  OptimBeta=Least_Sq_Loss(y, X)
  FittedVal=X%*%OptimBeta
  PartMSPE=0
  DummyVar=0
  for (i in 1:n) {
    DummyVar=(y[i]-FittedVal[i])^2
    PartMSPE=PartMSPE+DummyVar
  }
  MSPEFinal=n^{-1}*PartMSPE
  return(MSPEFinal)
}