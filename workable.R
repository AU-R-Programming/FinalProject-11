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

mspe = function(p, w, processed) {

  
  return(index)
}
