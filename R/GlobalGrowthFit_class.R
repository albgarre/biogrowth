
#' GlobalGrowthFit class
#' 
#' @description 
#' `r lifecycle::badge("stable")`
#' 
#' The `GlobalGrowthFit` class contains a growth model fitted to data 
#' using a global approach. Its constructor is [fit_growth()].
#' 
#' It is a subclass of list with the items:
#' 
#' - algorithm: type of algorithm as in [fit_growth()]
#' - data: data used for model fitting
#' - start: initial guess of the model parameters
#' - known: fixed model parameters
#' - primary_model: a character describing the primary model
#' - fit_results: an instance of modFit or modMCMC with the results of the fit
#' - best_prediction: Instance of [GrowthPrediction] with the best growth fit
#' - sec_models: a named vector with the secondary models assigned for each 
#' environmental factor. `NULL` for `environment="constant"`
#' - env_conditions: a list with the environmental conditions used for model
#' fitting. `NULL` for `environment="constant"`
#' - niter: number of iterations of the Markov chain. `NULL` if `algorithm != "MCMC"`
#' - logbase_mu: base of the logarithm for the definition of parameter mu 
#' (check the relevant vignette)
#' 
#' @name GlobalGrowthFit
#'   
NULL

#' @describeIn GlobalGrowthFit print of the model
#' 
#' @param x An instance of [GlobalGrowthFit].
#' @param ... ignored
#' 
#' @export
#' 
print.GlobalGrowthFit <- function(x, ...) {
    
    cat("Growth model fitted to data following a global approach conditions using ")
    cat(x$algorithm)
    cat("\n\n")
    
    cat("Number of experiments: ")
    cat(length(x$data))
    cat("\n\n")
    
    env <- names(x$best_prediction[[1]]$sec_models)  # Index does not matter, they are all the same
    cat(paste("Environmental factors included:", paste(env, collapse = ", "), "\n\n"))
    
    for (i in 1:length(x$best_prediction[[1]]$sec_models)) {
        cat(paste("Secondary model for ", names(x$best_prediction[[1]]$sec_models)[i], ": ",
                  x$best_prediction[[1]]$sec_models[[i]]$model, sep = ""))
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

}

#' @describeIn GlobalGrowthFit vector of fitted model parameters.
#'
#' @param object an instance of [GlobalGrowthFit].
#' @param ... ignored
#'
#' @importFrom stats coef
#'
#' @export
#'
coef.GlobalGrowthFit <- function(object, ...) {
    
    if (object$algorithm == "regression") {
        
        coef(object$fit_results)
        
    } else {
        object$fit_results$bestpar
    }
    
}

#' @describeIn GlobalGrowthFit statistical summary of the fit.
#'
#' @param object Instance of [GlobalGrowthFit]
#' @param ... ignored
#'
#' @export
#'
summary.GlobalGrowthFit <- function(object, ...) {
    
    out <- summary(object$fit_results)
    
    if (object$algorithm != "MCMC") {  # The summary of MCMC is a data.frame, so this would add a column
        out$logbase_mu <- object$logbase_mu
    }
    
    out
    
}

#' @describeIn GlobalGrowthFit vector of model predictions
#'
#' @param object Instance of `GlobalGrowthFit`.
#' @param ... ignored
#' @param times A numeric vector with the time points for the simulations. `NULL`
#' by default (using the same time points as the ones defined in `env_conditions`).
#' @param env_conditions a tibble describing the environmental conditions (as
#' in [predict_growth()]. 
#' 
#' @importFrom dplyr bind_rows
#'
#' @export
#'
predict.GlobalGrowthFit <- function(object, env_conditions, times=NULL, ...) {
    
    if (is.null(times)) {
        
        times <- env_conditions$time
        
    }
    
    my_model <- object$best_prediction[[1]]  # Index does not matter, parameters are the same

    pred <- predict_growth(environment = "dynamic",
                           times,
                           my_model$primary_model,
                           my_model$sec_models,
                           env_conditions,
                           logbase_mu = object$logbase_mu 
                           )
    
    pred$simulation$logN
    
}

#' @describeIn GlobalGrowthFit model residuals. They are returned as a tibble
#' with 4 columns: time (storage time), logN (observed count),
#' exp (name of the experiment) and res (residual).
#'
#' @param object Instance of [GlobalGrowthFit]
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.GlobalGrowthFit <- function(object, ...) {
    
    if (object$algorithm == "MCMC") {
        
        out <- lapply(names(object$data), function(each_sim) {
            
            simulations <- object$best_prediction[[each_sim]]$simulation %>%
                select("time", "logN") %>%
                as.data.frame()
            
            modCost(model = simulations,
                    obs = as.data.frame(object$data[[each_sim]]$data))$residuals %>%
                select(time = "x", logN = "obs", res = "res") %>%
                mutate(exp = each_sim)
            
        })
        
        bind_rows(out)
        
    } else {
        
        object$data %>%
            map(~ .$data) %>%
            imap_dfr(~ mutate(.x, exp = .y)) %>%
            mutate(res = residuals(object$fit_results))
        
    }
    
}

#' @describeIn GlobalGrowthFit variance-covariance matrix of the model, estimated
#' as 1/(0.5*Hessian) for regression and as the variance-covariance of the draws
#' for MCMC
#'
#' @param object an instance of [GlobalGrowthFit]
#' @param ... ignored
#'
#' @export
#'
vcov.GlobalGrowthFit <- function(object, ...) {
    
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
        }
        
        covar
        
    }
}

#' @describeIn GlobalGrowthFit deviance of the model.
#'
#' @param object an instance of [GlobalGrowthFit]
#' @param ... ignored
#' 
#' @importFrom stats deviance
#'
#' @export
#'
deviance.GlobalGrowthFit <- function(object, ...) {
    
    if (object$algorithm == "MCMC") {
        
        sum(residuals(object)$res^2)
        
    } else {
        deviance(object$fit_results)
    }
    
}

#' @describeIn GlobalGrowthFit fitted values. They are returned as a
#' tibble with 3 columns: time (storage time), exp (experiment 
#' identifier) and fitted (fitted value).
#' 
#' @param object an instance of [GlobalGrowthFit]
#' @param ... ignored
#' 
#' @importFrom rlang .data
#' @importFrom dplyr select mutate
#' @importFrom purrr %>%
#' 
#' @export
#' 
fitted.GlobalGrowthFit <- function(object, ...) {
    
    residuals(object) %>%
        mutate(fitted = .data$logN + .data$res) %>%
        select("exp", "time", "fitted")
    
}

#' @describeIn GlobalGrowthFit loglikelihood of the model
#' 
#' @param object an instance of [GlobalGrowthFit]
#' @param ... ignored
#' 
#' @export
#' 
logLik.GlobalGrowthFit <- function(object, ...) {
    
    n <- object$data %>% map_dfr(~.$data) %>% nrow()
    
    df <- n - length(coef(object))
    SS <- sum(residuals(object)$res^2)
    
    sigma <- sqrt(SS/df)
    
    lL <- - n/2*log(2*pi) -n/2 * log(sigma^2) - 1/2/sigma^2*SS
    
    lL

}

#' @describeIn GlobalGrowthFit Akaike Information Criterion
#'
#' @param object an instance of [GlobalGrowthFit]
#' @param ... ignored
#' @param k penalty for the parameters (k=2 by default)
#'
#' @export
#'
AIC.GlobalGrowthFit <- function(object, ..., k=2) {
    
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

#' @describeIn GlobalGrowthFit comparison between the fitted model and
#' the experimental data.
#'
#' @inheritParams plot.DynamicGrowth
#' @param x an instance of GlobalGrowthFit
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
plot.GlobalGrowthFit <- function(x, y=NULL, ...,
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
        
        plot(this_sim, 
             add_factor = add_factor, ylims = ylims,
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














