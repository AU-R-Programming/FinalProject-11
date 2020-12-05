











g11_plot <- function (response, covariates) {
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
        residual <- response - covariates%*%as.matrix(beta)
        fit <- covariates%*%beta
        #--------------------------------------------------------------------------
        # check and warnings:
        # 
        if(n != dim(covariates)[1])
                
                stop("Can you check the correspondence between x and y values? It seems we do not have proper pairs! If in doubt, check the documentation using ?g11_least_sq")
        #--------------------------------------------------------------------------
        #
        
        par(mfrow=c(1,3),oma = c(1,0,1,0))
        plot(residual~fit, xlab="Residual Values",ylab="Fitted Values",main="Residuals vs Fitted")
        qqnorm(residual)
        HistResVFit=hist(residual, xlab = "Residual Values", ylab="Frequency")
        title(line = -2)
        
}