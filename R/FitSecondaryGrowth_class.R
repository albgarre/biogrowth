
#' FitSecondaryGrowth class
#' 
#' @description 
#' The `FitSecondaryGrowth` class contains a model fitted to a set of growth rates
#' gathered under a variety of static conditions. 
#' Its constructor is [fit_secondary_growth()].
#' 
#' It is a subclass of list with the items:
#' \itemize{
#' \item fit_results: object returned by [modFit()].
#' \item secondary_model: secondary model fitted to the data.
#' \item mu_opt_fit: estimated growth rate under optimum conditions.
#' \item data: data used for the fit.
#' \item transformation: type of transformation of `mu` for the fit.
#' }
#' 
#' @name FitSecondaryGrowth
#'   
NULL

#' @describeIn FitSecondaryGrowth print of the model
#' 
#' @param x An instance of `FitSecondaryGrowth`.
#' @param ... ignored
#' 
#' @export
#' 
print.FitSecondaryGrowth <- function(x, ...) {
    
    cat("Secondary model estimated from data\n\n")
    
    env <- names(x$secondary_model)
    cat(paste("Environmental factors included:", paste(env, collapse = ", "), "\n\n"))
    
    cat(paste("mu_opt:", x$mu_opt_fit, "\n\n"))
    

    for (i in 1:length(x$secondary_model)) {
        cat(paste("Secondary model for ", names(x$secondary_model)[i], ":\n", sep = ""))
        print(unlist(x$secondary_model[[i]]))
        cat("\n")
    }
    
}

#' @describeIn FitSecondaryGrowth plots to evaluate the goodness of the fit.
#'
#' @param x An instance of FitSecondaryGrowth.
#' @param y ignored.
#' @param ... ignored.
#' @param which A numeric with the type of plot. 1 for obs versus predicted (default),
#' 2 for gamma curve
#' @param add_trend Whether to add a trend line (only for which=2)
#' @param add_segment Whether to join the observed and fitted points (only for which=2)
#'
#' @importFrom purrr %>%
#' @importFrom rlang .data
#' @importFrom dplyr mutate select rename
#' @importFrom ggplot2 ggplot geom_point geom_abline geom_smooth xlab ylab
#' @importFrom cowplot theme_cowplot
#' @importFrom ggplot2 theme_bw element_blank facet_wrap theme geom_segment
#' @importFrom tidyr pivot_longer
#' @importFrom stats residuals
#' 
#'
#' @export
#'
plot.FitSecondaryGrowth <- function(x, y=NULL, ..., 
                                    which = 1, 
                                    add_trend = FALSE,
                                    add_segment = FALSE) {
    
    obs_data <- x$data
    
    ## Select only the environmental factors (otherwise, which=2 would make a facet for each)
 
    env_factors <- names(x$secondary_model)
    
    obs_data <- x$data %>%
        select(matches(env_factors))
    
    obs_data$observed <- switch(
        x$transformation,
        sq = x$data$sq_mu,
        none = x$data$mu,
        log = x$data$log_mu
    )
    
    ## Add a column with the residuals
    
    obs_data$res <- residuals(x)
    
    ## Plotting
    
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
            ggplot(aes(x = .data$value)) +
            geom_point(aes(y = .data$predicted)) +
            geom_point(aes(y = .data$observed), colour = "darkgrey") 
        
        if (add_segment) {
        
            p1 <- p1 +
                geom_segment(aes(xend = .data$value, y = .data$observed, yend = .data$predicted), colour = "darkgrey")
                 
        }
        
        
        if (add_trend) {
            p1 <- p1 +
                geom_smooth(aes(y = .data$predicted), colour = "black") + 
                geom_smooth(aes(y = .data$observed), colour = "darkgrey")
        }
        
        p1 <- p1 + 
            facet_wrap("env_factor", scales = "free_x") + 
            xlab("") + 
            ylab(ylabel) +
            theme_bw()
        
    }
    
    p1
    
}

#' @describeIn FitSecondaryGrowth statistical summary of the fit.
#'
#' @param object Instance of `FitSecondaryGrowth`.
#' @param ... ignored
#'
#' @export
#'
summary.FitSecondaryGrowth <- function(object, ...) {
    
    summary(object$fit_results)
    
}

#' @describeIn FitSecondaryGrowth vector of model residuals.
#'
#' @param object an instance of `FitSecondaryGrowth`.
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
#' @param object an instance of `FitSecondaryGrowth`.
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
#' @param object an instance of `FitSecondaryGrowth`.
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
#' @param object an instance of `FitSecondaryGrowth`.
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
#' @param object an instance of `FitSecondaryGrowth`.
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
#' @param object an instance of `FitSecondaryGrowth`.
#' @param ... ignored
#' @param newdata A tibble describing the environmental conditions as in
#' [fit_secondary_growth()]. If `NULL`, it uses the same
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

#' @describeIn FitSecondaryGrowth loglikelihood of the model
#' 
#' @param object an instance of FitSecondaryGrowth
#' @param ... ignored
#' 
#' @export
#' 
logLik.FitSecondaryGrowth <- function(object, ...) {
    
    n <- nrow(object$data)
    sigma <- sqrt(object$fit_results$ssr/object$fit_results$df.residual)
    
    lL <- - n/2*log(2*pi) -n/2 * log(sigma^2) - 1/2/sigma^2*object$fit_results$ssr
    
    lL
    
}

#' @describeIn FitSecondaryGrowth Akaike Information Criterion
#'
#' @param object an instance of FitSecondaryGrowth
#' @param ... ignored
#' @param k penalty for the parameters (k=2 by default)
#'
#' @export
#'
AIC.FitSecondaryGrowth <- function(object, ..., k=2) {
    
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









