#First we will construct the least square estimator for y and X for any given beta paramater vector
Least_Sq_Loss=function (yVal, XVal) {
#The approximated value of beta from section 6.4 of the STAT-6210 textbook is calculated
  betamin=solve(t(XVal)%*%XVal)%*%t(XVal)%*%y
  return(betamin)
}