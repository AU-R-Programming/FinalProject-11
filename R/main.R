## file for the main function: g11_lm
## 
##  
##  
##  must include
##        - [ ] Confidence intervals: the user must be able to choose the significance level α to obtain for the 1−α confidence intervals for β and whether to use the asymptotic or bootstrap approach for this.
##        - [ ] Plots (with e.g. ggplot2) including:
##             [ ] Residuals vs fitted-values (fitted values are ŷ =Xβ̂ ).
##             [ ] qq-plot of residuals
##             [ ] Histogram (or density) of residuals
##        - [ ] Mean Square Prediction Error (MSPE)
##        - [ ] F-test: compute the statistic in matrix form and output the corresponding p-value. SSM, SSE, F∗. P(F>F∗) (p-value).
##        
#
#

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

