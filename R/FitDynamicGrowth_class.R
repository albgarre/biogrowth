#' FitDynamicGrowth class
#' 
#' @description 
#' `r lifecycle::badge("superseded")`
#' 
#' The class [FitDynamicGrowth] has been superseded by the top-level
#' class [GrowthFit], which provides a unified approach for growth modelling.
#' 
#' Still, it is still returned if the superseded [fit_dynamic_growth()] is called.
#' 
#' It is a subclass of list with the items:
#'      \itemize{
#'          \item fit_results: the object returned by `modFit`.
#'          \item best_prediction: the model prediction for the fitted parameters.
#'          \item env_conditions: environmental conditions for the fit.
#'          \item data: data used for the fit.
#'          \item starting: starting values for model fitting
#'          \item known: parameter values set as known.
#'          \item sec_models: a named vector with the secondary model
#'          for each environmental factor
#'          }
#' 
#' @name FitDynamicGrowth
#'   
NULL

#' @describeIn FitDynamicGrowth comparison between the fitted model and the data.
#' 
#' 
#' @param x An instance of `FitDynamicGrowth`.
#' @param ... ignored
#' 
#' @export
#' 
print.FitDynamicGrowth <- function(x, ...) {
    
    cat("Growth model fitted to data under dynamic conditions\n\n")
    
    env <- names(x$env_conditions)
    cat(paste("Environmental factors included:", paste(env, collapse = ", "), "\n\n"))
    
    cat("Parameters of the primary model:\n")
    print(unlist(x$best_prediction$primary_pars))
    cat("\n")
    
    
    logbase <- x$logbase_mu
    
    if ( abs(logbase - exp(1)) < .1 ) {
        logbase <- "e"
    }
    
    cat(paste0("Parameter mu defined in log-", logbase, " scale"))
    cat("\n\n")
    
    for (i in 1:length(x$best_prediction$sec_models)) {
        cat(paste("Secondary model for ", names(x$best_prediction$sec_models)[i], ":\n", sep = ""))
        print(unlist(x$best_prediction$sec_models[[i]]))
        cat("\n")
    }
    
}

#' @describeIn FitDynamicGrowth comparison between the fitted model and the data.
#'
#' @param x The object of class `FitDynamicGrowth` to plot.
#' @param y ignored
#' @param ... ignored.
#' @param add_factor whether to plot also one environmental factor.
#' If `NULL` (default), no environmental factor is plotted. If set
#' to one character string that matches one entry of x$env_conditions,
#' that condition is plotted in the secondary axis
#' @param ylims A two dimensional vector with the limits of the primary y-axis.
#' @param label_y1 Label of the primary y-axis.
#' @param label_y2 Label of the secondary y-axis.
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: [ggplot2::geom_line()]
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: [ggplot2::geom_line()]
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: [ggplot2::geom_line()]
#' @param point_col Aesthetic parameter to change the colour of the point geom, see: [ggplot2::geom_point()]
#' @param point_size Aesthetic parameter to change the size of the point geom, see: [ggplot2::geom_point()]
#' @param point_shape Aesthetic parameter to change the shape of the point geom, see: ggplot2::[ggplot2::geom_point()]
#' @param line_col2 Same as lin_col, but for the environmental factor.
#' @param line_size2 Same as line_size, but for the environmental factor.
#' @param line_type2 Same as lin_type, but for the environmental factor.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom graphics plot
#' @importFrom rlang .data
#' @importFrom cowplot theme_cowplot
#'
plot.FitDynamicGrowth <- function(x, y=NULL, ...,
                                  add_factor = NULL,
                                  ylims = NULL,
                                  label_y1 = "logN",
                                  label_y2 = add_factor,
                                  line_col = "black",
                                  line_size = 1,
                                  line_type = 1,
                                  point_col = "black",
                                  point_size = 3,
                                  point_shape = 16,
                                  line_col2 = "black",
                                  line_size2 = 1,
                                  line_type2 = "dashed"
) {
    
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
              line_type2 = line_type2
    )
    
    p + geom_point(aes(x = .data$time, y = .data$logN), data = x$data,
                   inherit.aes = FALSE, col = point_col,
                   size = point_size, shape = point_shape) +
        theme_cowplot()
    
}

#' @describeIn FitDynamicGrowth statistical summary of the fit.
#'
#' @param object Instance of FitDynamicGrowth.
#' @param ... ignored.
#'
#' @export
#'
summary.FitDynamicGrowth <- function(object, ...) {
    
    out <- summary(object$fit)
    out$logbase_mu <- object$logbase_mu
    
    out

}

#' @describeIn FitDynamicGrowth residuals of the model.
#'
#' @param object Instance of FitDynamicGrowth
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitDynamicGrowth <- function(object, ...) {
    residuals(object$fit_results)
}

#' @describeIn FitDynamicGrowth vector of fitted parameters.
#'
#' @param object an instance of `FitDynamicGrowth`.
#' @param ... ignored
#'
#' @importFrom stats coef
#'
#' @export
#'
coef.FitDynamicGrowth <- function(object, ...) {
    
    coef(object$fit_results)
    
}

#' @describeIn FitDynamicGrowth (unscaled) variance-covariance matrix of the model,
#' calculated as 1/(0.5*Hessian)
#'
#' @param object an instance of `FitDynamicGrowth`.
#' @param ... ignored
#'
#' @export
#'
vcov.FitDynamicGrowth <- function(object, ...) {
    
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

#' @describeIn FitDynamicGrowth deviance of the model.
#'
#' @param object an instance of `FitDynamicGrowth`.
#' @param ... ignored
#' 
#' @importFrom stats deviance
#'
#' @export
#'
deviance.FitDynamicGrowth <- function(object, ...) {
    
    deviance(object$fit_results)
    
}

#' @describeIn FitDynamicGrowth fitted values.
#' 
#' @param object an instance of `FitDynamicGrowth`.
#' @param ... ignored
#' 
#' @export
#' 
fitted.FitDynamicGrowth <- function(object, ...) {
    
    object$data$logN + residuals(object)
    
}

#' @describeIn FitDynamicGrowth model predictions.
#' 
#' @param object an instance of `FitDynamicGrowth`.
#' @param ... ignored
#' @param times A numeric vector with the time points for the simulations. `NULL`
#' by default (using the same time points as those for the simulation).
#' @param newdata a tibble describing the environmental conditions (as `env_conditions`)
#' in [predict_dynamic_growth()]. 
#' If `NULL` (default), uses the same conditions as those for fitting.
#' 
#' @export
#' 
predict.FitDynamicGrowth <- function(object, times = NULL, newdata = NULL, ...) {
    
    if (is.null(newdata)) {
        
        newdata <- object$env_conditions
        
    }
    
    if (is.null(times)) {
        times <- object$data$time
    }
    
    
    # pred <- predict_dynamic_growth(
    #     times,
    #     newdata,
    #     object$best_prediction$primary_pars,
    #     object$best_prediction$sec_models
    # )
    
    pred <- predict_growth(environment = "dynamic",
                           times,
                           object$best_prediction$primary_pars,
                           object$best_prediction$sec_models,
                           newdata,
                           logbase_mu = object$logbase_mu 
    )
    
    pred$simulation$logN
    
}

#' @describeIn FitDynamicGrowth loglikelihood of the model
#' 
#' @param object an instance of FitDynamicGrowth
#' @param ... ignored
#' 
#' @export
#' 
logLik.FitDynamicGrowth <- function(object, ...) {
    
    ## AIC without penalty
    n <- nrow(object$data)
    sigma <- sqrt(object$fit_results$ssr/object$fit_results$df.residual)
    
    lL <- - n/2*log(2*pi) -n/2 * log(sigma^2) - 1/2/sigma^2*object$fit_results$ssr
    
    lL
    
}

#' @describeIn FitDynamicGrowth Akaike Information Criterion
#'
#' @param object an instance of FitDynamicGrowth
#' @param ... ignored
#' @param k penalty for the parameters (k=2 by default)
#' 
#' @importFrom stats logLik
#'
#' @export
#'
AIC.FitDynamicGrowth <- function(object, ..., k=2) {
    
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




