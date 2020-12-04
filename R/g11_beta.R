#' @title Least Square
#'
#' @description Function that provides the least square estimator (beta_hat)
#' @param response A \code{vector} with the values for the dependent variable (also called outcome).
#' @param covariates A \code{matrix} with the values for the independent variable (also called predictors, or explanatory variable).
#' @return A \code{numeric}.
#' @author Group11
#' @importFrom 
#' @export
#' @examples
#' Using data(hubble) from libary(gamair)
#' g11_lm(hubble$y, hubble$x)


g11_beta <- function (response, covariates) {
     
     #-------------------------------- preamble -------------------------------------
     # in case user gets something that is not a vector for 'response'
     #  or a matrix for 'covariates', coerce data:
     response <- as.vector(response)
     covariates <- as.matrix(covariates)
     #-------------------------------------------------------------------------------
     # Define base and dummy parameters:
     n <- length(response)
     p <- dim(covariates)[2]
     degrees_freedom <- n - p
     #-------------------------------------------------------------------------------
     # check and warnings:
     if(n != dim(covariates)[1])
          stop("Can you check the correspondence between x and y values? It seems we do not have proper pairs! If in doubt, check the documentation using ?g11_least_sq")
     #-------------------------------------------------------------------------------
     
     beta <- solve(t(covariates)%*%covariates)%*%t(covariates)%*%response

     return(beta)
}