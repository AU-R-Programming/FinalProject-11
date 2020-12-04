#Calculates the F statistic for the linear model as well as the associated p-value
F_Calc=function(y, X) {
  OptimBeta=Least_Sq_Loss(y, X)
  FitValues=X%*%OptimBeta
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
  DFM=p-1
  DFE=n-p
  MSM=SSM_Val/DFM
  MSE=SSE_Val/DFE
  #The actual F statistics will be computed here
  F_Test_Stat=MSM/MSE
  #The stat wil be compared to the real one, retrieved from the linear model previously created
  #F_Real_Stat=summary(MainLinRegr)$fstatistic[1]
  ProbTestF=pf(F_Test_Stat, DFM, DFE, 1)
  F.Info=rbind(F_Test_Stat, DFM, DFE,ProbTestF)
  rownames(F.Info)=c("F Stat: ","Lower DF: ","Higher DF:","P-Value: ")
  return(F.Info) 
}