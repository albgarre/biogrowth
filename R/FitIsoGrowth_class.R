#' FitIsoGrowth class
#' 
#' @description 
#' `r lifecycle::badge("superseded")`
#' 
#' The class [FitIsoGrowth] has been superseded by the top-level
#' class [GrowthFit], which provides a unified approach for growth modelling.
#' 
#' Still, it is still returned if the superseded [fit_isothermal_growth()] is called.
#' 
#' It is a subclass of list with the items:
#'      \itemize{
#'          \item data: data used for model fitting
#'          \item model: name of the primary inactivation model
#'          \item starting_point: initial value of the model parameters
#'          \item known: fixed model parameters
#'          \item fit: object returned by [modFit()]
#'          \item best_prediction: model prediction for the model fitted.
#'          }
#' 
#' @name FitIsoGrowth
#'   
NULL

#' @describeIn FitIsoGrowth print of the model
#' 
#' @param x An instance of `FitIsoGrowth`.
#' @param ... ignored
#' 
#' @export
#' 
print.FitIsoGrowth <- function(x, ...) {
    
    cat("Primary growth model built from data\n\n")
    
    cat(paste("Growth model:", x$model, "\n\n"))
    
    cat("Model parameters:\n")
    print(unlist(x$best_prediction$pars))
    
    logbase <- x$logbase_mu
    
    if ( abs(logbase - exp(1)) < .1 ) {
        logbase <- "e"
    }
    cat("\n")
    cat(paste0("Parameter mu defined in log-", logbase, " scale"))
    
}

#' @describeIn FitIsoGrowth compares the fitted model against the data.
#'
#' @param x The object of class `FitIsoGrowth` to plot.
#' @param y ignored
#' @param ... ignored.
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: [geom_line()]
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: [geom_line()]
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: [geom_line()]
#' @param point_col Aesthetic parameter to change the colour of the point geom, see: [geom_point()]
#' @param point_size Aesthetic parameter to change the size of the point geom, see: [geom_point()]
#' @param point_shape Aesthetic parameter to change the shape of the point geom, see: [geom_point()]
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom rlang .data
#' @importFrom graphics plot
#' @importFrom cowplot theme_cowplot
#'
plot.FitIsoGrowth <- function(x, y=NULL, ...,
                              line_col = "black",
                              line_size = 1,
                              line_type = 1,
                              point_col = "black",
                              point_size = 3,
                              point_shape = 16) {
    
    p <- plot(x$best_prediction,
              line_col = line_col,
              line_size = line_size,
              line_type = line_type)
    
    p + geom_point(aes(x = .data$time, y = .data$logN), data = x$data,
                   col = point_col,  size = point_size, shape = point_shape) +
        theme_cowplot()
    
    
}

#' @describeIn FitIsoGrowth statistical summary of the fit.
#'
#' @param object Instance of `FitIsoGrowth`.
#' @param ... ignored
#'
#' @export
#'
summary.FitIsoGrowth <- function(object, ...) {
    
    out <- summary(object$fit)
    out$logbase_mu <- object$logbase_mu
    
    out

}

#' @describeIn FitIsoGrowth vector of model residuals.
#'
#' @param object Instance of `FitIsoGrowth`.
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitIsoGrowth <- function(object, ...) {
    residuals(object$fit)
}

#' @describeIn FitIsoGrowth vector of fitted model parameters.
#'
#' @param object an instance of `FitIsoGrowth`.
#' @param ... ignored
#'
#' @importFrom stats coef
#'
#' @export
#'
coef.FitIsoGrowth <- function(object, ...) {
    
    coef(object$fit)
    
}

#' @describeIn FitIsoGrowth variance-covariance matrix of the model, estimated
#' as 1/(0.5*Hessian)
#'
#' @param object an instance of `FitIsoGrowth`.
#' @param ... ignored
#'
#' @export
#'
vcov.FitIsoGrowth <- function(object, ...) {
    
    # The code has been adapted from the one of summary.modFit
    
    covar  <- try(solve(0.5*object$fit$hessian), silent = TRUE)
    
    if (!is.numeric(covar)) {
        warning("Cannot estimate covariance; system is singular")
        
        param  <- object$par
        p      <- length(param)
        
        covar <- matrix(data = NA, nrow = p, ncol = p)
    }
    
    covar
    
}

#' @describeIn FitIsoGrowth deviance of the model.
#'
#' @param object an instance of `FitIsoGrowth`.
#' @param ... ignored
#' 
#' @importFrom stats deviance
#'
#' @export
#'
deviance.FitIsoGrowth <- function(object, ...) {
    
    deviance(object$fit)
    
}

#' @describeIn FitIsoGrowth vector of fitted values.
#' 
#' @param object an instance of `FitIsoGrowth `.
#' @param ... ignored
#' 
#' @export
#' 
fitted.FitIsoGrowth <- function(object, ...) {
    
    object$data$logN + residuals(object)
    
}

#' @describeIn FitIsoGrowth vector of model predictions.
#' 
#' @param object an instance of FitIsoGrowth
#' @param ... ignored
#' @param times numeric vector describing the time points for the prediction.
#' If `NULL` (default), uses the same points as those used for fitting.
#' 
#' @export
#' 
predict.FitIsoGrowth <- function(object, times = NULL, ...) {
    
    if (is.null(times)) {
        
        times <- object$data$time
        
    }
    
    # browser()
    
    pars <- c(coef(object), object$known)
    my_model <- as.list(pars)
    my_model$model <- object$model
    
    pred <- predict_growth(times, my_model, check = FALSE,
                           logbase_mu = object$logbase_mu)
    
    
    # pred <- predict_isothermal_growth(object$model,
    #                                   times,
    #                                   object$best_prediction$pars,
    #                                   check=FALSE)
    # 
    pred$simulation$logN
    
}

#' @describeIn FitIsoGrowth loglikelihood of the model
#' 
#' @param object an instance of FitIsoGrowth
#' @param ... ignored
#' 
#' @export
#' 
logLik.FitIsoGrowth <- function(object, ...) {
    
    ## AIC without penalty
    n <- nrow(object$data)
    sigma <- sqrt(object$fit$ssr/object$fit$df.residual)
    
    lL <- - n/2*log(2*pi) -n/2 * log(sigma^2) - 1/2/sigma^2*object$fit$ssr
    
    lL
    
}

#' @describeIn FitIsoGrowth Akaike Information Criterion
#'
#' @param object an instance of FitIsoGrowth
#' @param ... ignored
#' @param k penalty for the parameters (k=2 by default)
#' 
#' @importFrom stats logLik
#'
#' @export
#'
AIC.FitIsoGrowth <- function(object, ..., k=2) {

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








