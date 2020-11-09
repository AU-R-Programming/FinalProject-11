#' @title Simple Monte-Carlo integration
#'
#' @description Compute an approximation of the integral of the function f(x)
#' with respect to dx in the range [a, b] by Monte-Carlo integration using
#' uniform sampling.
#' @param x_range A \code{vector} of dimension 2 used to denote the integration
#' region of interest, i.e. [a, b].
#' @param fun A \code{string} containing the function to be integrated. It
#' is assumed that \code{x} is used as the variable of interest.
#' @param B A \code{numeric} (integer) used to denote the number of simulations.
#' @param seed A \code{numeric} used to control the seed of the random number
#' generator used by this function.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{I}{Estimated value of the integral}
#'      \item{var}{Estimated variance of the estimator}
#' }
#' @author Stephane Guerrier
#' @importFrom stats runif
#' @export
#' @examples
#' mc_int(x_range = c(0,1), fun = "x^2", B = 10^5)
#' mc_int(x_range = c(0,1), fun = "x^2*sin(x^2/pi)", B = 10^5)
#' 
#' From here, is the ACTUAL function:


mc_int = function(x_range, fun, B, seed = 1291){
     # A few checks
     # Check x_range
     if (length(x_range) != 2 || x_range[1] >= x_range[2]){
          stop("x_range is incorrectly specified")
     }
     
     # Check fun
     if (class(fun) != "character"){
          stop("fun is incorrectly specified and should be a character")
     }
     
     x = mean(x_range)
     test_fun = try(eval(parse(text = fun)), silent = TRUE)
     if (class(test_fun) == "try-error"){
          stop("fun cannot be evaluated")
     }
     
     # Check B
     if (B < 1){
          error("B is incorrectly specified")
     }
     
     # Set seed
     set.seed(seed)
     
     # Compute the length of the interval, i.e. (b-a)
     interval_length = diff(x_range)
     
     # Let's draw some uniforms to get Ui and Xi
     Ui = runif(B)
     Xi = x_range[1] + Ui*interval_length
     
     # Compute \hat{I}
     x = Xi
     I_hat = interval_length*mean(eval(parse(text = fun)))
     
     # Compute \hat{I}_2
     I2_hat = interval_length*mean((eval(parse(text = fun)))^2)
     var_I_hat = (interval_length*I2_hat - I_hat^2)/B
     
     # Output list
     out = list(I = I_hat, var = var_I_hat,
                fun = fun, x_range = x_range, B = B)
     class(out) = "MCI"
     out
}