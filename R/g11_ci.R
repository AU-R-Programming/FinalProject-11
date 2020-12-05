#' @title Linear Model Function for Group11 - AU STAT6210
#'
#' @description Function that 
#' @param response A \code{vector} with the values for the dependent variable (also called outcome).
#' @param covariates A \code{matrix} with the values for the independent variable (also called predictors, or explanatory variable).
#' @param Beta A \code{matrix} that can be calculated using \code{g11_lm} function.!
#' @param alpha A \code{numeric} (double) that sets the alpha coefficient to be used. Has to be between 0 and 1.
#' @param method A \code{string} that defines the method used. Options are "Asymptotic" and "Bootstrap", accepts minor misspellings with a warning - which can be both good and bad.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{beta}{Estimated coefficients, Linear Regression Model.}
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


g11_ci <- function (response, covariates, alpha = 0.05, method = "Asymptotic") {
     
     #--------------------------------------------------------------------------
     #-------------------------------- preamble --------------------------------
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
     #  
     response <- as.vector(response)
     covariates <- as.matrix(covariates)
     #--------------------------------------------------------------------------
     # Define base and dummy parameters:
     # 
     n <- length(response)
     p <- dim(covariates)[2]
     degrees_freedom <- n - p
     ci_beta <- rep(NA, p)
     size <- 1000
     beta <- g11_beta(response, covariates)
     #--------------------------------------------------------------------------
     # check and warnings:
     # 
     if(n != dim(covariates)[1])
          
          stop("Can you check the correspondence between x and y values? It seems we do not have proper pairs! If in doubt, check the documentation using ?g11_least_sq")
     #--------------------------------------------------------------------------
     # feedback for alpha value:
     # 
     if(alpha >= 1 | alpha <= 0) 
          
          stop("The alpha here has to be somewhere between 0 and 1... For biological applications we usually aim for 0.05, for example. If in doubt, check the documentation using ?g11_lm")
     #--------------------------------------------------------------------------
     # feedback for method: try to fix any misspelling of 'method' argument:
     # 
     method_check_a <- stringsim("Asymptotic", tolower(method))
     method_check_b <- stringsim("Bootstrap", tolower(method))
     
     if(method_check_a <= 0.7 & method_check_b <= 0.7) {
          stop("Oops! We are sorry, but we have not developed the method you are asking just yet! How do you feel about the 'Asymptotic' method, or maybe 'Bootstrap'?. If in doubt, you can always check the documentation using ?g11_lm :)")
          
     } else if(method_check_a > 0.7) {
          method <- "Asymptotic"
          warning("Did you mean 'Asymptotic'? If yes, just roll with it! If no, please disregard the results and begin again! Check ?g11_lm for documentation")
          
     } else if(method_check_b > 0.7) {
          method <- "Bootstrap"
          warning("Did you mean 'Bootstrap'? If yes, just roll with it! If no, please disregard the results and begin again! Check ?g11_lm for documentation")
     }
     #--------------------------------------------------------------------------
     
     #
     residual <- response - covariates%*%as.matrix(beta)
     
     #
     sigma_hat <- as.numeric((1/degrees_freedom)*t(residual)%*%residual)
     
     # Estimate of the variance of the estimated beta from Eq. (1.2)
     var_beta <- sigma_hat*solve(t(covariates)%*%covariates)
     
     # CI based on alpha:
     z <- alpha/2
     quad <- 1 - z
     
     if(method == "Asymptotic") {
          
          ci_beta <- c(beta - qnorm(p = quad)*sqrt(abs(var_beta)),
                       beta + qnorm(p = quad)*sqrt(abs(var_beta))
          
     } else {
          
          # bootstrap goes here: (Eyoel)
          # define dummy params
          vec <- rep(NA, size)
          allofit <- cbind(response, covariates)
          
          for (i in 1:size) {
               ci_beta <- allofit[sample(1:n, replace = TRUE),]
               vec[i]=mean(ci_beta)
          }
          
          # magnifico !
          
          ci_beta <- quantile(vec, c(quad, z))
          
     }
     
     
     # Return all estimated values
     
     g11_ci_results <- list(Beta = beta,
                            Sigma = sigma_hat,
                            Variance_Beta = var_beta,
                            Confidence_Interval = ci_beta)
     
     # still missing proper show of CI:
     # results_names <- list("Beta", "Sigma", "Variance", "CI")
     # g11_lm_results <- cbind(results_names, results)
     
     return(g11_ci_results)
     
}
