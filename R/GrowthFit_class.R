
#' GrowthFit class
#' 
#' @description 
#' `r lifecycle::badge("stable")`
#' 
#' The `GrowthFit` class contains a growth model fitted to data under
#' static or dynamic conditions. Its constructor is [fit_growth()].
#' 
#' It is a subclass of list with the items:
#' 
#' - environment: type of environment as in [fit_growth()]
#' - algorithm: type of algorithm as in [fit_growth()]
#' - data: data used for model fitting
#' - start: initial guess of the model parameters
#' - known: fixed model parameters
#' - primary_model: a character describing the primary model
#' - fit_results: an instance of modFit or modMCMC with the results of the fit
#' - best_prediction: Instance of [GrowthPrediction] with the best growth fit
#' - sec_models: a named vector with the secondary models assigned for each 
#' environmental factor. `NULL` for `environment="constant"`
#' - env_conditions: a tibble with the environmental conditions used for model
#' fitting. `NULL` for `environment="constant"`
#' - niter: number of iterations of the Markov chain. `NULL` if `algorithm != "MCMC"`
#' - logbase_mu: base of the logarithm for the definition of parameter mu 
#' (check the relevant vignette)
#' - logbase_logN: base of the logarithm for the definition of the population size 
#' (check the relevant vignette)
#' 
#' @name GrowthFit
#'   
NULL

#' @describeIn GrowthFit print of the model
#' 
#' @param x An instance of [GrowthFit].
#' @param ... ignored
#' 
#' @export
#' 
print.GrowthFit <- function(x, ...) {
    
    if (x$environment == "constant") {
        
        cat("Primary growth model fitted to data\n\n")
        
        cat(paste("Growth model:", x$primary_model, "\n\n"))
        
        cat("Estimated parameters:\n")
        print(coef(x))
        
        cat("\nFixed parameters:\n")
        print(x$known)
        
        logbase <- x$logbase_mu
        
        if ( abs(logbase - exp(1)) < .1 ) {
            logbase <- "e"
        }
        cat("\n")
        cat(paste0("Parameter mu defined in log-", logbase, " scale\n"))
        
        logbase <- x$logbase_logN
        
        if ( abs(logbase - exp(1)) < .1 ) {
            logbase <- "e"
        }
        cat(paste0("Population size defined in log-", logbase, " scale\n"))
        
    } else {
        
        cat("Growth model fitted to data gathered under dynamic environmental conditions using ")
        cat(x$algorithm)
        cat("\n\n")
        
        env <- names(x$best_prediction$sec_models)
        cat(paste("Environmental factors included:", paste(env, collapse = ", "), "\n\n"))
        
        for (i in 1:length(x$best_prediction$sec_models)) {
            cat(paste("Secondary model for ", names(x$best_prediction$sec_models)[i], ": ",
                      x$best_prediction$sec_models[[i]]$model, sep = ""))
            cat("\n")

        }
        
        cat("\n")
        
        cat("Parameter estimates:\n")
        print(coef(x))
        
        cat("\nFixed parameters:\n")
        print(unlist(x$known))
        
        logbase <- x$logbase_mu

        if ( abs(logbase - exp(1)) < .1 ) {
            logbase <- "e"
        }

        cat(paste0("Parameter mu defined in log-", logbase, " scale"))
        cat("\n")
        
        logbase <- x$logbase_logN
        
        if ( abs(logbase - exp(1)) < .1 ) {
            logbase <- "e"
        }
        cat(paste0("Population size defined in log-", logbase, " scale\n"))
        
    }
    
    
}

#' @describeIn GrowthFit vector of fitted model parameters.
#'
#' @param object an instance of [GrowthFit].
#' @param ... ignored
#'
#' @importFrom stats coef
#'
#' @export
#'
coef.GrowthFit <- function(object, ...) {

    if (object$algorithm == "regression") {
        
        coef(object$fit_results)
        
    } else {
        object$fit_results$bestpar
    }
    
}

#' @describeIn GrowthFit statistical summary of the fit.
#'
#' @param object Instance of [GrowthFit]
#' @param ... ignored
#'
#' @export
#'
summary.GrowthFit <- function(object, ...) {
    
    out <- summary(object$fit_results)
    
    if (object$algorithm != "MCMC") {  # The summary of MCMC is a data.frame, so this would add a column
        out$logbase_mu <- object$logbase_mu
        out$logbase_logN <- object$logbase_logN
    }
    
    out
    
}

#' @describeIn GrowthFit vector of model predictions.
#' 
#' @param object an instance of [GrowthFit]
#' @param ... ignored
#' @param times numeric vector describing the time points for the prediction.
#' If `NULL` (default), uses the same points as those used for fitting.
#' @param env_conditions tibble describing the environmental conditions as in [fit_growth()].
#' If `NULL` (default), uses the environmental condition of the fitting. Ignored
#' if `environment="constant"`
#' 
#' @export
#' 
predict.GrowthFit <- function(object, times = NULL, env_conditions = NULL, ...) {
    
    if (is.null(times)) {  ## Used the times of the data if NULL
        
        times <- object$data$time
        
    }
    
    if (object$environment == "constant") {  # Prediction under constant environment
        
        pars <- c(coef(object), object$known)
        my_model <- as.list(pars)
        my_model$model <- object$primary_model
        
        pred <- predict_growth(times, my_model, check = FALSE,
                               logbase_mu = object$logbase_mu,
                               logbase_logN = object$logbase_logN)
        
        pred$simulation$logN
        
    } else {  ## Prediction under dynamic conditions
        
        if (is.null(env_conditions)) {  # Used the environment of the data if NULL
            
            env_conditions <- object$env_conditions
            
        }

        pred <- predict_growth(environment = "dynamic",
                               times,
                               object$best_prediction$primary_model,
                               object$best_prediction$sec_models,
                               env_conditions,
                               logbase_mu = object$logbase_mu,
                               logbase_logN = object$logbase_logN
        )
        
        pred$simulation$logN

    }
    
    
}

#' @describeIn GrowthFit vector of model residuals.
#'
#' @param object Instance of [GrowthFit]
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.GrowthFit <- function(object, ...) {
    
    if (object$algorithm == "MCMC") {
        
        pred <- predict(object)
        
        pred - object$data$logN
        
    } else {
        
        residuals(object$fit_results)
        
    }
    
}

#' @describeIn GrowthFit variance-covariance matrix of the model, estimated
#' as 1/(0.5*Hessian) for regression and as the variance-covariance of the draws
#' for MCMC
#'
#' @param object an instance of [GrowthFit]
#' @param ... ignored
#'
#' @export
#'
vcov.GrowthFit <- function(object, ...) {
    
    if (object$algorithm == "MCMC") {
        
        cov(object$fit_results$pars)
         
    } else {
        
        # The code has been adapted from the one of summary.modFit
        
        covar  <- try(solve(0.5*object$fit_results$hessian), silent = TRUE)
        
        if (!is.numeric(covar)) {
            warning("Cannot estimate covariance; system is singular")
            
            param  <- object$par
            p      <- length(param)
            
            covar <- matrix(data = NA, nrow = p, ncol = p)
            
            return(covar)
        }
        
        ## Scale the variance
        
        rdf <- object$fit_results$df.residual
        resvar <- object$fit_results$ssr/rdf
        
        covar*resvar
        
    }
}

#' @describeIn GrowthFit deviance of the model.
#'
#' @param object an instance of [GrowthFit]
#' @param ... ignored
#' 
#' @importFrom stats deviance
#'
#' @export
#'
deviance.GrowthFit <- function(object, ...) {
    
    if (object$algorithm == "MCMC") {
        
        sum(residuals(object)^2)

    } else {
        deviance(object$fit_results)
    }

}

#' @describeIn GrowthFit vector of fitted values.
#' 
#' @param object an instance of [GrowthFit]
#' @param ... ignored
#' 
#' @export
#' 
fitted.GrowthFit <- function(object, ...) {
    
    predict(object)
    
}

#' @describeIn GrowthFit loglikelihood of the model
#' 
#' @param object an instance of GrowthFit
#' @param ... ignored
#' 
#' @export
#' 
logLik.GrowthFit <- function(object, ...) {
    
    if (object$algorithm == "regression") {
        
        n <- nrow(object$data)
        sigma <- sqrt(object$fit_results$ssr/object$fit_results$df.residual)
        
        lL <- - n/2*log(2*pi) -n/2 * log(sigma^2) - 1/2/sigma^2*object$fit_results$ssr
        
        lL    
        
    } else {
        
        n <- nrow(object$data)
        SS <- min(object$fit_results$SS, na.rm = TRUE)
        
        df <- n - length(coef(object))
        
        sigma <- sqrt(SS/df)
        
        lL <- - n/2*log(2*pi) -n/2 * log(sigma^2) - 1/2/sigma^2*SS
        
        lL
        
    }
    
    
    
}

#' @describeIn GrowthFit Akaike Information Criterion
#'
#' @param object an instance of GrowthFit
#' @param ... ignored
#' @param k penalty for the parameters (k=2 by default)
#' 
#' @importFrom stats logLik
#'
#' @export
#'
AIC.GrowthFit <- function(object, ..., k=2) {
    
    ## Normal AIC
    
    p <- length(coef(object))
    
    lL <- logLik(object) 
    
    AIC <- 2*p - 2*lL
    
    ## Calculate the penalty
    
    n <- nrow(object$data)
    
    penalty <- (k*p^2 + k*p)/(n - p - 1)
    
    ## Return
    
    AIC + penalty
    
}

#' @describeIn GrowthFit compares the fitted model against the data.
#'
#' @param x The object of class [GrowthFit] to plot.
#' @param y ignored
#' @param ... ignored.
#' @param add_factor whether to plot also one environmental factor.
#' If `NULL` (default), no environmental factor is plotted. If set
#' to one character string that matches one entry of x$env_conditions,
#' that condition is plotted in the secondary axis. Ignored if `environment="constant"`
#' @param ylims A two dimensional vector with the limits of the primary y-axis. 
#' `NULL` by default
#' @param label_y1 Label of the primary y-axis.
#' @param label_y2 Label of the secondary y-axis. Ignored if `environment="constant"`
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: [ggplot2::geom_line()]
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: [ggplot2::geom_line()]
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: [ggplot2::geom_line()]
#' @param point_col Aesthetic parameter to change the colour of the point geom, see: [ggplot2::geom_point()]
#' @param point_size Aesthetic parameter to change the size of the point geom, see: [ggplot2::geom_point()]
#' @param point_shape Aesthetic parameter to change the shape of the point geom, see: [ggplot2::geom_point()]
#' @param line_col2 Same as lin_col, but for the environmental factor. Ignored if `environment="constant"`
#' @param line_size2 Same as line_size, but for the environmental factor. Ignored if `environment="constant"`
#' @param line_type2 Same as lin_type, but for the environmental factor. Ignored if `environment="constant"`
#' @param label_x Label of the x-axis
#' 
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom rlang .data
#' @importFrom graphics plot
#' @importFrom cowplot theme_cowplot
#'
plot.GrowthFit <- function(x, y=NULL, ...,
                           add_factor = NULL,
                           line_col = "black",
                           line_size = 1,
                           line_type = 1,
                           point_col = "black",
                           point_size = 3,
                           point_shape = 16,
                           ylims = NULL,
                           label_y1 = NULL,
                           label_y2 = add_factor,
                           label_x = "time",
                           line_col2 = "black",
                           line_size2 = 1,
                           line_type2 = "dashed") {
    
    ## Get the label for the y-axis
    
    logbase <- x$logbase_logN
    
    if ( abs(logbase - exp(1)) < .1 ) {
        logbase <- "e"
    }
    
    if (is.null(label_y1)) {
        label_y1 <- paste0("logN (in log-", logbase, ")")
    } else {
        label_y1 <- label_y1
    }
    
    if (x$environment == "constant") {
        
        p <- plot(x$best_prediction,
                  line_col = line_col,
                  line_size = line_size,
                  line_type = line_type,
                  ylims = ylims,
                  label_y1 = label_y1,
                  label_x = label_x)

    } else {
        
        p <- plot(x$best_prediction,
                  add_factor = add_factor,
                  ylims = ylims,
                  label_y1 = label_y1,
                  label_y2 = label_y2,
                  line_col = line_col,
                  line_size = line_size,
                  line_type = line_type,
                  line_col2 = line_col2,
                  line_size2 = line_size2,
                  line_type2 = line_type2,
                  label_x = label_x
                  )
        
    }
    
    p + geom_point(aes(x = .data$time, y = .data$logN), data = x$data,
                   col = point_col,  size = point_size, shape = point_shape) +
        theme_cowplot()

}





