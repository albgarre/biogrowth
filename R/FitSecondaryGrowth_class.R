
#' FitSecondaryGrowth class
#' 
#' @description 
#' The \code{FitSecondaryGrowth} class contains a model fitted to a set of growth rates
#' gathered under a variety of static conditions. 
#' Its constructor is \code{\link{fit_secondary_growth}}.
#' 
#' It is a subclass of list with the items:
#' \itemize{
#' \item fit_results: object returned by \code{\link{modFit}}.
#' \item secondary_model: secondary model fitted to the data.
#' \item mu_opt_fit: estimated growth rate under optimum conditions.
#' \item data: data used for the fit.
#' \item transformation: type of transformation of \code{mu} for the fit.
#' }
#' 
#' @name FitSecondaryGrowth
#'   
NULL

#' @describeIn FitSecondaryGrowth plots to evaluate the goodness of the fit.
#'
#' @param x An instance of FitSecondaryGrowth.
#' @param y ignored.
#' @param ... ignored.
#' @param which A numeric with the type of plot. 1 for obs versus predicted (default),
#' 2 for gamma curve
#' @param add_trend Whether to add a trend line (only for which=2)
#'
#' @importFrom purrr %>%
#' @importFrom rlang .data
#' @importFrom dplyr mutate select rename
#' @importFrom ggplot2 ggplot geom_point geom_abline geom_smooth xlab ylab
#' @importFrom cowplot theme_cowplot
#' @importFrom ggplot2 theme_bw element_blank facet_wrap theme
#' @importFrom tidyr pivot_longer
#' @importFrom stats residuals
#'
#' @export
#'
plot.FitSecondaryGrowth <- function(x, y=NULL, ..., which = 1, add_trend = FALSE) {
    
    obs_data <- x$data
    obs_data$res <- residuals(x)
    
    ## Convert according to transformation
    
    obs_data <- if (x$transformation == "sq") {
        
        obs_data %>%
            select(-"log_mu", -"mu") %>%
            rename(observed = "sq_mu")
        
    } else if (x$transformation == "none") {
        
        obs_data %>%
            select(-"log_mu", -"sq_mu") %>%
            rename(observed = "mu")
        
    } else if (x$transformation == "log") {
        
        obs_data %>%
            select(-"sq_mu", "mu") %>%
            rename(observed = "mu")
        
    }
    
    if (which == 1) { # Prediction vs observation
        
        label_end <- switch(x$transformation,
                         sq = "square root of the growth rate",
                         log = "logarithm of the growth rate",
                         none = "growth rate"
        )

        p1 <- obs_data %>%
            mutate(predicted = .data$observed + .data$res) %>%
            ggplot(aes(x = .data$observed, y = .data$predicted)) +
            geom_point() +
            geom_abline(slope = 1, intercept = 0, linetype = 2) +
            geom_smooth(method = "lm", se = FALSE, colour = "grey") +
            xlab(paste("Observed", label_end)) +
            ylab(paste("Fitted", label_end)) +
            theme_cowplot()
        
        
    } else if(which == 2) {  # Gamma curve
        
        ylabel <- switch(x$transformation,
                         sq = "Square root of the growth rate",
                         log = "Logarithm of the growth rate",
                         none = "Growth rate"
                         )
        
        p1 <- obs_data %>%
            mutate(predicted = .data$observed + .data$res) %>%
            select(-"res") %>%
            pivot_longer(-c("observed", "predicted"),
                         names_to = "env_factor", values_to = "value") %>%
            pivot_longer(-c("env_factor", "value"),
                         names_to = "point_type", values_to = "growth") %>%
            ggplot(aes(x = .data$value)) +
            geom_point(aes(y = .data$growth, colour = .data$point_type)) +
            facet_wrap("env_factor", scales = "free_x") +
            ylab(ylabel) + xlab("") +
            theme_bw() +
            theme(legend.title = element_blank())
        
        if (isTRUE(add_trend)) {
            p1 <- p1 + geom_smooth(aes(y = .data$growth, colour = .data$point_type))
        }
        
    }
    
    p1
    
    
}

#' @describeIn FitSecondaryGrowth statistical summary of the fit.
#'
#' @param object Instance of \code{FitSecondaryGrowth}.
#' @param ... ignored
#'
#' @export
#'
summary.FitSecondaryGrowth <- function(object, ...) {
    
    summary(object$fit_results)
    
}

#' @describeIn FitSecondaryGrowth vector of model residuals.
#'
#' @param object an instance of \code{FitSecondaryGrowth}.
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitSecondaryGrowth <- function(object, ...) {
    residuals(object$fit_results)
}

#' @describeIn FitSecondaryGrowth vector of fitted model parameters.
#'
#' @param object an instance of \code{FitSecondaryGrowth}.
#' @param ... ignored
#'
#' @importFrom stats coef
#'
#' @export
#'
coef.FitSecondaryGrowth <- function(object, ...) {
    
    coef(object$fit_results)
    
}

#' @describeIn FitSecondaryGrowth variance-covariance matrix of the model, estimated
#' as 1/(0.5*Hessian)
#'
#' @param object an instance of \code{FitSecondaryGrowth}.
#' @param ... ignored
#'
#' @export
#'
vcov.FitSecondaryGrowth <- function(object, ...) {
    
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

#' @describeIn FitSecondaryGrowth deviance of the model.
#'
#' @param object an instance of \code{FitSecondaryGrowth}.
#' @param ... ignored
#' 
#' @importFrom stats deviance
#'
#' @export
#'
deviance.FitSecondaryGrowth <- function(object, ...) {
    
    deviance(object$fit_results)
    
}

#' @describeIn FitSecondaryGrowth vector of fitted values.
#' 
#' The fitted values are returned in the same scale as the one
#' used for the fitting (sqrt, log or none).
#' 
#' @param object an instance of \code{FitSecondaryGrowth}.
#' @param ... ignored
#' 
#' @export
#' 
fitted.FitSecondaryGrowth <- function(object, ...) {
    
    observed <- switch(object$transformation,
                       sq = object$data$sq_mu,
                       log = object$data$log_mu,
                       none = object$data$mu
    )
    
    
    observed + residuals(object)
    
}

#' @describeIn FitSecondaryGrowth vector of model predictions.
#' 
#' @param object an instance of \code{FitSecondaryGrowth}.
#' @param ... ignored
#' @param newdata A tibble describing the environmental conditions as in
#' \code{\link{fit_secondary_growth}}. If \code{NULL}, it uses the same
#' conditions as for model fitting (default).
#' 
#' @importFrom purrr map_chr
#' 
#' @export
#' 
predict.FitSecondaryGrowth <- function(object, newdata=NULL, ...) {
    
    if (is.null(newdata)) {
        
        newdata <- object$data
        
    }
    
    sec_model_names <- object$secondary_model %>% 
        map_chr(~ .$model)
    
    
    gammas <- calculate_gammas_secondary(sec_model_names,
                                         newdata, 
                                         object$secondary_model) 
    
    gammas*object$mu_opt_fit
    
}










