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


g11_ftest <- function (response, covariates) {
     
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
     beta <- g11_beta(response, covariates)
     fit <- covariates%*%beta
     sample_mean <- mean(response)
     SSM <- 0
     SSE <- 0
     dSSM <- 0
     dSSE <- 0
     #--------------------------------------------------------------------------
     # check and warnings:
     # 
     if(n != dim(covariates)[1])
          
          stop("Can you check the correspondence between x and y values? It seems we do not have proper pairs! If in doubt, check the documentation using ?g11_least_sq")
     #--------------------------------------------------------------------------
     
     for (i in 1:n) {
          dSSM <- (fit[i] - sample_mean)^2
          dSSE <- (response[i] - fit[i])^2
          SSM <- SSM + dSSM
          SSE <- SSE + dSSE
     }
     
     DFM <- p
     DFE <- n - p
     MSM <- SSM/DFM
     MSE <- SSE/DFE
     
     #The actual F statistics will be computed here
     f_stat <- MSM/MSE
     
     #The stat will be compared to the real one, retrieved from the linear model previously created
     #F_Real_Stat=summary(MainLinRegr)$fstatistic[1]
     #
     # Bootiful:
     #
     f_prob <- pf(f_stat, DFM, DFE, 1)
     f_info <- rbind(f_stat, DFM, DFE, f_prob)
     rownames(f_info) <- c("F Stat: ","Lower DF: ","Higher DF:","P-Value: ")
     return(f_info) 
     
     }
     
     
     
     
     
     
     