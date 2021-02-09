#' FitDynamicGrowth class
#' 
#' @description 
#' The \code{FitDynamicGrowth} class contains a model fitted based on growth data
#' under dynamic conditions. Its constructor is \code{\link{fit_dynamic_growth}}.
#' 
#' It is a subclass of list with the items:
#'      \itemize{
#'          \item fit_results: the object returned by \code{modFit}.
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
#' @param x The object of class \code{FitDynamicGrowth} to plot.
#' @param y ignored
#' @param ... ignored.
#' @param add_factor whether to plot also one environmental factor.
#' If \code{NULL} (default), no environmenta factor is plotted. If set
#' to one character string that matches one entry of x$env_conditions,
#' that condition is plotted in the secondary axis
#' @param ylims A two dimensional vector with the limits of the primary y-axis.
#' @param label_y1 Label of the primary y-axis.
#' @param label_y2 Label of the secondary y-axis.
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: \code{\link{geom_line}}
#' @param point_col Aesthetic parameter to change the colour of the point geom, see: \code{\link{geom_point}}
#' @param point_size Aesthetic parameter to change the size of the point geom, see: \code{\link{geom_point}}
#' @param point_shape Aesthetic parameter to change the shape of the point geom, see: \code{\link{geom_point}}
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
    
    summary(object$fit_results)
    
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
#' @param object an instance of \code{FitDynamicGrowth}.
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
#' @param object an instance of \code{FitDynamicGrowth}.
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
    }
    
    covar
    
}

#' @describeIn FitDynamicGrowth deviance of the model.
#'
#' @param object an instance of \code{FitDynamicGrowth}.
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
#' @param object an instance of \code{FitDynamicGrowth}.
#' @param ... ignored
#' 
#' @export
#' 
fitted.FitDynamicGrowth <- function(object, ...) {
    
    object$data$logN + residuals(object)
    
}

#' @describeIn FitDynamicGrowth model predictions.
#' 
#' @param object an instance of \code{FitDynamicGrowth}.
#' @param ... ignored
#' @param newdata a tibble describing the environmental conditions (as \code{env_conditions})
#' @param times a numeric vector of times for the predictions
#' @param 
#' in \code{\link{predict_dynamic_growth}}. 
#' If \code{NULL} (default), uses the same conditions as those for fitting.
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
    
    
    pred <- predict_dynamic_growth(
        times,
        newdata,
        object$best_prediction$primary_pars,
        object$best_prediction$sec_models
    )
    
    pred$simulation$logN
    
}




