
#' FitMultipleDynamicGrowth class
#' 
#' @description 
#' `r lifecycle::badge("superseded")`
#' 
#' The class [FitMultipleDynamicGrowth] has been superseded by the top-level
#' class [GlobalGrowthFit], which provides a unified approach for growth modelling.
#' 
#' Still, it is still returned if the superseded [fit_multiple_growth()] is called.
#' 
#' It is a subclass of list with the items:
#'      \itemize{
#'          \item fit_results: the object returned by `modFit`.
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

#' @describeIn FitMultipleDynamicGrowth print of the model
#' 
#' @param x An instance of `FitMultipleDynamicGrowth`.
#' @param ... ignored
#' 
#' @export
#' 
print.FitMultipleDynamicGrowth <- function(x, ...) {
    
    cat("Growth model fitted to various growth experiments\n\n")
    
    cat(paste("Number of experiments:", length(x$data), "\n\n"))
    
    env <- names(x$data[[1]]$conditions)
    cat(paste("Environmental factors included:", paste(env, collapse = ", "), "\n\n"))
    
    cat("Parameters of the primary model:\n")
    print(unlist(x$best_prediction[[1]]$primary_pars))
    cat("\n")
    
    logbase <- x$logbase_mu
    
    if ( abs(logbase - exp(1)) < .1 ) {
        logbase <- "e"
    }
    
    cat(paste0("Parameter mu defined in log-", logbase, " scale"))
    cat("\n\n")
    
    for (i in 1:length(x$best_prediction[[1]]$sec_models)) {
        cat(paste("Secondary model for ", names(x$best_prediction[[1]]$sec_models)[i], ":\n", sep = ""))
        print(unlist(x$best_prediction[[1]]$sec_models[[i]]))
        cat("\n")
    }
    
}

#' @describeIn FitMultipleDynamicGrowth comparison between the fitted model and
#' the experimental data.
#'
#' @inheritParams plot.DynamicGrowth
#' @param x an instance of FitMultipleDynamicGrowth.
#' @param point_size Size of the data points
#' @param point_shape shape of the data points
#' @param subplot_labels labels of the subplots according to `plot_grid`.
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
#' @param object Instance of `FitMultipleDynamicGrowth`.
#' @param ... ignored
#'
#' @export
#'
summary.FitMultipleDynamicGrowth <- function(object, ...) {
    
    out <- summary(object$fit)
    out$logbase_mu <- object$logbase_mu
    
    out
    
}

#' @describeIn FitMultipleDynamicGrowth calculates the model residuals. Returns a 
#' tibble with 4 columns: time (storage time), logN (observed count),
#' exp (name of the experiment) and res (residual).
#'
#' @param object Instance of `FitMultipleDynamicGrowth`.
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitMultipleDynamicGrowth <- function(object, ...) {
    
    object$data %>%
        map(~ .$data) %>%
        imap_dfr(~ mutate(.x, exp = .y)) %>%
        mutate(res = residuals(object$fit_results))
    
}

#' @describeIn FitMultipleDynamicGrowth vector of fitted parameters.
#'
#' @param object an instance of `FitMultipleDynamicGrowth`.
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
#' @param object an instance of `FitMultipleDynamicGrowth`.
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
        
        return(covar)
    }
    
    
    ## Scale the variance
    
    rdf <- object$fit_results$df.residual
    resvar <- object$fit_results$ssr/rdf
    
    covar*resvar
    
}

#' @describeIn FitMultipleDynamicGrowth deviance of the model.
#'
#' @param object an instance of `FitMultipleDynamicGrowth`.
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
#' @param object an instance of `FitMultipleDynamicGrowth`.
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

#' @describeIn FitMultipleDynamicGrowth vector of model predictions
#'
#' @param object Instance of `FitMultipleDynamicGrowth`.
#' @param ... ignored
#' @param times A numeric vector with the time points for the simulations. `NULL`
#' by default (using the same time points as the ones defined in `env_conditions`).
#' @param env_conditions a tibble describing the environmental conditions (as
#' in [fit_multiple_growth()]. 
#' 
#' @importFrom dplyr bind_rows
#'
#' @export
#'
predict.FitMultipleDynamicGrowth <- function(object, env_conditions, times=NULL, ...) {
    
    if (is.null(times)) {
        
        times <- env_conditions$time
        
    }
    
    my_model <- object$best_prediction[[1]]  # Index does not matter, parameters are the same
    
    # pred <- predict_dynamic_growth(
    #     times,
    #     env_conditions,
    #     my_model$primary_pars,
    #     my_model$sec_models
    # )
    
    pred <- predict_growth(environment = "dynamic",
                           times,
                           my_model$primary_pars,
                           my_model$sec_models,
                           env_conditions,
                           logbase_mu = object$logbase_mu 
    )
    
    pred$simulation$logN

}

#' @describeIn FitMultipleDynamicGrowth loglikelihood of the model
#' 
#' @param object an instance of FitMultipleDynamicGrowth
#' @param ... ignored
#' 
#' @export
#' 
logLik.FitMultipleDynamicGrowth <- function(object, ...) {
    
    n <- object$data %>% map_dfr(~.$data) %>% nrow()
    
    df <- n - length(coef(object))
    SS <- sum(residuals(object)$res^2)
    
    sigma <- sqrt(SS/df)

    lL <- - n/2*log(2*pi) -n/2 * log(sigma^2) - 1/2/sigma^2*SS
    
    lL
    
}


#' @describeIn FitMultipleDynamicGrowth Akaike Information Criterion
#'
#' @param object an instance of FitMultipleDynamicGrowth
#' @param ... ignored
#' @param k penalty for the parameters (k=2 by default)
#' 
#' @importFrom stats logLik
#'
#' @export
#'
AIC.FitMultipleDynamicGrowth <- function(object, ..., k=2) {
    
    ## Normal AIC
    
    p <- length(coef(object))
    
    lL <- logLik(object) 
    
    AIC <- 2*p - 2*lL
    
    ## Calculate the penalty
    
    n <- object$data %>% map_dfr(~.$data) %>% nrow()
    
    penalty <- (k*p^2 + k*p)/(n - p - 1)
    
    ## Return
    
    AIC + penalty
    
}







