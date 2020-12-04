#' @title Linear Model Function for Group11 - AU STAT6210
#'
#' @description 
#' @param response A \code{vector} .
#' @param covariates A \code{matrix} .
#' @param alpha A \code{numeric} (double) .
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{beta}{explanation}
#'      \item{sigma2}{explanation}
#'      \item{variance_beta}{explanation}
#'      \item{ci}{explanation}
#' }
#' @author Group11
#' @importFrom 
#' @export
#' @examples
#' Using data(hubble) from libary(gamair)
#' g11_lm(hubble$y, hubble$x, alpha = 0.01, method = "Bootstrap")
#' g11_lm(hubble$y, hubble$x, method = "Asymptotic")
#' 
#' From here, is the ACTUAL function:


g11_lm <- function (response, covariates, alpha = 0.05, method = "Asymptotic") {
     
     # could not make the check below work, so using default params above
     # making sure the user is asking with all parameters
     # params <- list(...)
     # params_check <- list("a","b","c","d")
     # params_unused <- setdiff(names(params),params_check)
     # if(length(params_unused))
     #   stop("Hey, you have some unused parameters..! Can you come back and check? See documentation for help: ?g11_lm",
     #        paste(params_unuseds,collapse = ', '))
     
     # in case user gets something that is not a vector for 'response'
     #  or a matrix for 'covariates', coerce data:
     response <- as.vector(response)
     covariates <- as.matrix(covariates)
     
     # Define base parameters:
     n <- length(response)
     p <- dim(covariates)[2]
     degrees_freedom <- n - p
     
     # feedback for x's and y's:
     if(n != dim(covariates)[1])
          stop("Can you check the correspondence between x and y values? It seems we do not have proper pairs! If in doubt, check the documentation using ?g11_lm")
     
     # feedback for alpha value:
     if(alpha >= 1 | alpha <= 0) 
          stop("The alpha here has to be somewhere between 0 and 1... For biological applications we usually aim for 0.05, for example. If in doubt, check the documentation using ?g11_lm")
     
     #
     beta_hat <- solve(t(covariates)%*%covariates)%*%t(covariates)%*%response
     
     #
     residual <- response - covariates%*%as.matrix(beta_hat)
     
     #
     sigma2_hat <- (1/degrees_freedom)*t(residual)%*%residual
     
     #
     var_beta <- sigma2_hat*solve(t(covariates)%*%covariates)
     
     # CI based on alpha:
     z <- alpha/2
     quantile <- 1 - z
     
     # feedback for method:
     # try to fix any misspelling of 'method' argument.
     # put an warning to let the user be aware.
     method_check_a <- stringsim("Asymptotic", tolower(method))
     method_check_b <- stringsim("Bootstrap", tolower(method))
     # 
     if(method_check_a < 0.7 & method_check_b < 0.7) {
          stop("Oops! We are sorry, but we have not developed the method you are asking just yet! How do you feel about the 'Asymptotic' method, or maybe 'Bootstrap'?. If in doubt, you can always check the documentation using ?g11_lm :)")
          
     } else if(method_check_a >= 0.7) {
          method <- "Asymptotic"
          warning("Did you mean 'Asymptotic'? If yes, just roll with it! If no, please disregard the results and begin again! Check ?g11_glm for documentation")
          
     } else if(method_check_b >= 0.7) {
          method <- "Bootstrap"
          warning("Did you mean 'Bootstrap'? If yes, just roll with it! If no, please disregard the results and begin again! Check ?g11_glm for documentation")
     }
     
     if(method == "Asymptotic") {
          ci_beta <- c(beta_hat - qnorm(p = quantile)*sqrt(var_beta),
                       beta_hat + qnorm(p = quantile)*sqrt(var_beta))
          
     } else {
          # bootstrap goes here: Eyoel:
          ci_beta <- 
          
     }
     
     
     # Return all estimated values
     
     g11_lm_results <- list(beta = beta_hat, 
                     sigma2 = sigma2_hat, 
                     variance_beta = var_beta,
                     ci = ci_beta)
     
     # still missing proper show of CI:
     # results_names <- list("Beta", "Sigma", "Variance", "CI")
     # g11_lm_results <- cbind(results_names, results)
     
     return(g11_lm_results)
     
}


#Calculates confidence interval for beta in linear regression
ConfidInter.LinRegr=function(Beta, y, X, alpha=0.05, Bootstrap=TRUE) {
#Takes size of parameter vector
  Bootstrap=as.logical(Bootstrap)
  n=length(y)
  p=length(Beta)
  size=1000
  df=n-p
   # Compute residuals
  ResidVals <- y - X%*%as.matrix(Beta) #residual
  # Beta = beta_hat (na LIN REGRESS OPTIM)

  SigmaHat <- as.numeric((1/df)*t(ResidVals)%*%ResidVals) #sigma_hat

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
     ConfINTBeta=c(Beta - qnorm(p = Quadrant)*sqrt(VarBetaHat),
                   Beta + qnorm(p = Quadrant)*sqrt(VarBetaHat))
     print("This is Bootstrap")
     print(ConfINTBeta)
   }
  #After method chosen runs, then the resulting ConfINTBeta will be outputted by function at the end
   return(ConfINTBeta)
}


#First we will construct the least square estimator for y and X for any given beta paramater vector
Least_Sq_Loss=function (yVal, XVal) {
#The approximated value of beta from section 6.4 of the STAT-6210 textbook is calculated
  betamin=solve(t(XVal)%*%XVal)%*%t(XVal)%*%y
  return(betamin)
}



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


























# https://www.tutorialspoint.com/r/r_linear_regression.htm
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm
# https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/lm.R
# http://r-statistics.co/Linear-Regression.html


#
#

library(gamair)
library(stringdist)
data(hubble)


# for testing:
# response <- as.vector(hubble$y)
# covariates <- as.vector(hubble$x)
# alpha <- 0.05
# method <- "asymptotic"

g11_lm <- function (response, covariates, alpha = 0.05, method = "Asymptotic") {
  
  # could not make the check below work, so using default params above
  # making sure the user is asking with all parameters
  # params <- list(...)
  # params_check <- list("a","b","c","d")
  # params_unused <- setdiff(names(params),params_check)
  # if(length(params_unused))
  #   stop("Hey, you have some unused parameters..! Can you come back and check? See documentation for help: ?g11_lm",
  #        paste(params_unuseds,collapse = ', '))
  
  # in case user gets something that is not a vector for 'response'
  #  or a matrix for 'covariates', coerce data:
  response <- as.vector(response)
  covariates <- as.matrix(covariates)
  
  # Define base parameters:
  n <- length(response)
  p <- dim(covariates)[2]
  degrees_freedom <- n - p
  
  # feedback for x's and y's:
  if(n != dim(covariates)[1])
    stop("Can you check the correspondence between x and y values? It seems we do not have proper pairs! If in doubt, check the documentation using ?g11_lm")
  
  # feedback for alpha value:
  if(alpha >= 1 | alpha <= 0) 
    stop("The alpha here has to be somewhere between 0 and 1... For biological applications we usually aim for 0.05, for example. If in doubt, check the documentation using ?g11_lm")

  #
  beta_hat <- solve(t(covariates)%*%covariates)%*%t(covariates)%*%response
  
  #
  residual <- response - covariates%*%as.matrix(beta_hat)
  
  #
  sigma2_hat <- (1/degrees_freedom)*t(residual)%*%residual
  
  #
  var_beta <- sigma2_hat*solve(t(covariates)%*%covariates)
  
  # CI based on alpha:
  z <- alpha/2
  quantile <- 1 - z
  
  # asymptotic method:
  ci_beta <- c(beta_hat - qnorm(p = quantile)*sqrt(var_beta), beta_hat + 
                 qnorm(p = quantile)*sqrt(var_beta))
  
  # feedback for method:
  # try to fix any misspelling of 'method' argument.
  # put an warning to let the user be aware.
  method_check_a <- stringsim("Asymptotic", tolower(method))
  method_check_b <- stringsim("Bootstrap", tolower(method))
  # 
  if(method_check_a < 0.7 & method_check_b < 0.7) {
    stop("Oops! We are sorry, but we have not developed the method you are sking just yet! How do you feel about the 'Asymptotic' method, or maybe 'Bootstrap'?. If in doubt, you can always check the documentation using ?g11_lm :)")
    
  } else if(method_check_a >= 0.7) {
    method <- "Asymptotic"
    warning("Did you mean 'Asymptotic'? If yes, just roll with it! If no, please disregard the results and begin again! Check ?g11_glm for documentation")
    
  } else if(method_check_b >= 0.7) {
    method <- "Bootstrap"
    warning("Did you mean 'Bootstrap'? If yes, just roll with it! If no, please disregard the results and begin again! Check ?g11_glm for documentation")
  }

  if(method == "Asymptotic") {
    ci_beta <- c(beta_hat - qnorm(p = quantile)*sqrt(var_beta),
                beta_hat + qnorm(p = quantile)*sqrt(var_beta))
    
  } else {
    # bootstrap method goes in here
  #   

  }
  

  
  # Return all estimated values

  results <- list(beta = beta_hat, 
                  sigma2 = sigma2_hat, 
                  variance_beta = var_beta,
                  ci = ci_beta)
 
  # still missing proper show of CI:
  # results_names <- list("Beta", "Sigma", "Variance", "CI")
  # g11_lm_results <- cbind(results_names, results)
  
  return(g11_lm_results)
  
}






# Function: argmin. 
# From "Mediana" package, available at https://rdrr.io/cran/Mediana/
# Argument: p, Vector of p-values (1 x m)
#           w, Vector of hypothesis weights (1 x m)
#           processed, Vector of binary indicators (1 x m) [1 if processed and 0 otherwise].
# Description: Hidden function used in the Chain function. Find the index of the smallest weighted p-value among the non-processed null hypotheses with a positive weight (index=0 if
# the smallest weighted p-value does not exist) in a chain procedure

argmin = function(p, w, processed) {

  index = 0
  m = length(p)
  for (i in 1:m) {
    if (w[i] > 0 & processed[i] == 0) {
      if (index == 0) {
        pmin = p[i]/w[i]
        index = i
      }
      if (index > 0 & p[i]/w[i] < pmin) {
        pmin = p[i]/w[i]
        index = i
      }
    }
  }
  return(index)
}

# End of argmin



# Function: MSPE
#
#
#
#

mspe = function(p, w) {

  
  return(index)
}


