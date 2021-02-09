
#' FitMultipleDynamicGrowth class
#' 
#' @description 
#' The \code{FitMultipleDynamicGrowth} class contains a model fitted to a set
#' of experiments gathered under dynamic conditions. Its constructor is 
#' \code{\link{fit_multiple_growth}}.
#' 
#' It is a subclass of list with the items:
#'      \itemize{
#'          \item fit_results: the object returned by \code{modFit}.
#'          \item best_prediction: a list with the models predictions for each condition.
#'          \item data: a list with the data used for the fit.
#'          \item starting: starting values for model fitting
#'          \item known: parameter values set as known.
#'          \item sec_models: a named vector with the secondary model
#'          for each environmental factor.
#'          }
#' 
#' @name FitMultipleDynamicGrowth
#'   
NULL

#' @describeIn FitMultipleDynamicGrowth comparison between the fitted model and
#' the experimental data.
#'
#' @inheritParams plot.DynamicGrowth
#' @param x an instance of FitMultipleDynamicGrowth.
#' @param point_size Size of the data points
#' @param point_shape shape of the data points
#' @param subplot_labels labels of the subplots according to \code{plot_grid}.
#' @param label_x label of the x-axis
#'
#' @importFrom ggplot2 geom_point
#' @importFrom cowplot plot_grid
#' @importFrom rlang .data
#'
#' @export
#'
plot.FitMultipleDynamicGrowth <- function(x, y=NULL, ...,
                                          add_factor = NULL,
                                          ylims = NULL,
                                          label_x = "time",
                                          label_y1 = "logN",
                                          label_y2 = add_factor,
                                          line_col = "black",
                                          line_size = 1,
                                          line_type = "solid",
                                          line_col2 = "black",
                                          line_size2 = 1,
                                          line_type2 = "dashed",
                                          point_size = 3,
                                          point_shape = 16,
                                          subplot_labels = "AUTO"
) {
    
    my_plots <- lapply(1:length(x$data), function(i) {
        
        this_d <- x$data[[i]]$data
        this_sim <- x$best_prediction[[i]]
        
        plot(this_sim, add_factor = add_factor, ylims = ylims,
             label_y1 = label_y1, label_y2 = label_y2,
             line_col = line_col, line_size = line_size,
             line_type = line_type, line_col2 = line_col2,
             line_size2 = line_size2, line_type2 = line_type2) +
            geom_point(aes(x = .data$time, y = .data$logN), data = this_d,
                       size = point_size, shape = point_shape) +
            xlab(label_x)
        
    })
    
    plot_grid(plotlist = my_plots, labels = subplot_labels)
    
}

#' @describeIn FitMultipleDynamicGrowth statistical summary of the fit.
#'
#' @param object Instance of \code{FitMultipleDynamicGrowth}.
#' @param ... ignored
#'
#' @export
#'
summary.FitMultipleDynamicGrowth <- function(object, ...) {
    
    summary(object$fit_results)
    
}

#' @describeIn FitMultipleDynamicGrowth calculates the model residuals. Returns a 
#' tibble with 4 columns: time (storage time), logN (observed count),
#' exp (name of the experiment) and res (residual).
#'
#' @param object Instance of \code{FitMultipleDynamicGrowth}.
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitMultipleDynamicGrowth <- function(object, ...) {
    
    residuals(object$fit_results)
    
    object$data %>%
        map(~ .$data) %>%
        imap_dfr(~ mutate(.x, exp = .y)) %>%
        mutate(res = residuals(object$fit_results))
    
}

#' @describeIn FitMultipleDynamicGrowth vector of fitted parameters.
#'
#' @param object an instance of \code{FitMultipleDynamicGrowth}.
#' @param ... ignored
#'
#' @importFrom stats coef
#'
#' @export
#'
coef.FitMultipleDynamicGrowth <- function(object, ...) {
    
    coef(object$fit_results)
    
}

#' @describeIn FitMultipleDynamicGrowth (unscaled) variance-covariance matrix, 
#' estimated as 1/(0.5*Hessian).
#'
#' @param object an instance of \code{FitMultipleDynamicGrowth}.
#' @param ... ignored
#'
#' @export
#'
vcov.FitMultipleDynamicGrowth <- function(object, ...) {
    
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

#' @describeIn FitMultipleDynamicGrowth deviance of the model.
#'
#' @param object an instance of \code{FitMultipleDynamicGrowth}.
#' @param ... ignored
#' 
#' @importFrom stats deviance
#'
#' @export
#'
deviance.FitMultipleDynamicGrowth <- function(object, ...) {
    
    deviance(object$fit_results)
    
}

#' @describeIn FitMultipleDynamicGrowth fitted values. They are returned as a
#' tibble with 3 columns: time (storage time), exp (experiment 
#' identifier) and fitted (fitted value).
#' 
#' @param object an instance of \code{FitMultipleDynamicGrowth}.
#' @param ... ignored
#' 
#' @importFrom rlang .data
#' @importFrom dplyr select mutate
#' @importFrom purrr %>%
#' 
#' @export
#' 
fitted.FitMultipleDynamicGrowth <- function(object, ...) {
    
    residuals(object) %>%
        mutate(fitted = .data$logN + .data$res) %>%
        select("exp", "time", "fitted")
    
}

#' @describeIn FitMultipleDynamicGrowth model predictions. They are returned as 
#' a tibble with 3 columns: time (storage time), logN (observed count),
#' and exp (name of the experiment).
#'
#' @param object Instance of \code{FitMultipleDynamicGrowth}.
#' @param ... ignored
#' @param times A numeric vector with the time points for the simulations. \code{NULL}
#' by default (using the same time points as those for the simulation).
#' @param newdata a tibble describing the environmental conditions (as \code{env_conditions})
#' in \code{\link{fit_multiple_growth}}. 
#' If \code{NULL} (default), uses the same conditions as those for fitting.
#' 
#' @importFrom dplyr bind_rows
#'
#' @export
#'
predict.FitMultipleDynamicGrowth <- function(object, times=NULL, newdata=NULL, ...) {
    
    if (is.null(newdata)) {
        
        newdata <- object$data
        
    }
    
    out <- lapply(1:length(newdata), function(i) {
        
        if (is.null(times)) {
            times <- newdata[[i]]$data$time
        } 
        
        pred <- predict_dynamic_growth(
            times,
            newdata[[i]]$conditions,
            object$best_prediction[[1]]$primary_pars,
            object$best_prediction[[1]]$sec_models
        )
        
        exp_name <- names(newdata)[[i]]
        
        if (is.null(exp_name)) {
            exp_name <- paste0("exp", i)
        }
        
        tibble(time = times,
               exp = exp_name,
               logN = pred$simulation$logN)
        
    }) 
    
    bind_rows(out)
    
}









