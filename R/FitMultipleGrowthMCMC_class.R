
#' FitMultipleGrowthMCMC class
#' 
#' @description 
#' The \code{FitMultipleGrowthMCMC} class contains a model fitted to a set of dynamic
#' experiments using an MCMC algorithm. Its constructor is 
#' \code{\link{fit_multiple_growth_MCMC}}.
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
#' @name FitMultipleGrowthMCMC
#'   
NULL

#' @describeIn FitMultipleGrowthMCMC print of the model
#' 
#' @param x An instance of \code{FitMultipleGrowthMCMC}.
#' @param ... ignored
#' 
#' @export
#' 
print.FitMultipleGrowthMCMC <- function(x, ...) {
    
    cat("Growth model fitted to various growth experiments using MCMC\n\n")
    
    cat(paste("Number of experiments:", length(x$data), "\n\n"))
    
    env <- names(x$data[[1]]$conditions)
    cat(paste("Environmental factors included:", paste(env, collapse = ", "), "\n\n"))
    
    cat("Parameters of the primary model:\n")
    print(unlist(x$best_prediction[[1]]$primary_pars))
    cat("\n")
    
    for (i in 1:length(x$best_prediction[[1]]$sec_models)) {
        cat(paste("Secondary model for ", names(x$best_prediction[[1]]$sec_models)[i], ":\n", sep = ""))
        print(unlist(x$best_prediction[[1]]$sec_models[[i]]))
        cat("\n")
    }
    
}

#' @describeIn FitMultipleGrowthMCMC comparison between the model fitted and the
#' data.
#'
#' @inheritParams plot.FitMultipleDynamicGrowth
#' @param x an instance of FitMultipleGrowthMCMC.
#'
#' @export
#'
plot.FitMultipleGrowthMCMC <- function(x, y=NULL, ...,
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
    
    plot.FitMultipleDynamicGrowth(x,
                                  add_factor = add_factor,
                                  label_x = label_x,
                                  ylims = ylims,
                                  label_y1 = label_y1,
                                  label_y2 = label_y2,
                                  line_col = line_col,
                                  line_size = line_size,
                                  line_type = line_type,
                                  line_col2 = line_col2,
                                  line_size2 = line_size2,
                                  line_type2 = line_type2,
                                  point_size = point_size,
                                  point_shape = point_shape,
                                  subplot_labels = subplot_labels
    )
    
}

#' @describeIn FitMultipleGrowthMCMC statistical summary of the fit.
#'
#' @param object instance of \code{FitMultipleGrowthMCMC}.
#' @param ... ignored.
#'
#' @export
#'
summary.FitMultipleGrowthMCMC <- function(object, ...) {
    
    summary(object$fit_results)
    
}

#' @describeIn FitMultipleGrowthMCMC model residuals. They are returned as a tibble
#' with 4 columns: time (storage time), logN (observed count),
#' exp (name of the experiment) and res (residual).
#'
#' @param object Instance of \code{FitMultipleGrowthMCMC}.
#' @param ... ignored
#'
#' @importFrom dplyr bind_rows select
#' @importFrom FME modCost
#'
#' @export
#'
#'
residuals.FitMultipleGrowthMCMC <- function(object, ...) {
    
    out <- lapply(names(object$data), function(each_sim) {
        
        simulations <- object$best_prediction[[each_sim]]$simulation %>%
            select("time", "logN") %>%
            as.data.frame()
        
        # my_cost <- modCost(model = simulations,
        #                    obs = as.data.frame(object$data[[i]]$data))
        #
        # tibble(residual = my_cost$residuals$res,
        #        experiment = i)
        
        
        modCost(model = simulations,
                obs = as.data.frame(object$data[[each_sim]]$data))$residuals %>%
            select(time = "x", logN = "obs", res = "res") %>%
            mutate(exp = each_sim)
        
    })
    
    bind_rows(out)
    
}

#' @describeIn FitMultipleGrowthMCMC vector of fitted model parameters.
#'
#' @param object an instance of \code{FitMultipleGrowthMCMC}.
#' @param ... ignored
#'
#' @export
#'
coef.FitMultipleGrowthMCMC <- function(object, ...) {
    
    object$fit_results$bestpar
    
}

#' @describeIn FitMultipleGrowthMCMC variance-covariance matrix of the model,
#' estimated as the variance of the samples from the Markov chain.
#'
#' @param object an instance of \code{FitMultipleGrowthMCMC}.
#' @param ... ignored
#'
#' @importFrom stats cov
#'
#' @export
#'
vcov.FitMultipleGrowthMCMC <- function(object, ...) {
    
    cov(object$fit_results$pars)
    
}

#' @describeIn FitMultipleGrowthMCMC deviance of the model, calculated as the sum of
#' squared residuals of the prediction with the lowest standard error.
#'
#' @param object an instance of \code{FitMultipleGrowthMCMC}.
#' @param ... ignored
#'
#' @importFrom dplyr select
#' @importFrom FME modCost
#'
#' @export
#'
deviance.FitMultipleGrowthMCMC <- function(object, ...) {
    
    lapply(1:length(object$best_prediction), function(i) {
        
        model <- object$best_prediction[[i]]$simulation %>%
            select("time", "logN") %>%
            as.data.frame()
        
        obs <- object$data[[i]]$data %>%
            select("time", "logN") %>%
            as.data.frame()
        
        modCost(model, obs)$residuals$res^2
        
    }) %>%
        unlist() %>%
        sum()
    
}

#' @describeIn FitMultipleGrowthMCMC fitted values of the model. They are returned
#' as a tibble with 3 columns: time (storage time), exp (experiment 
#' identifier) and fitted (fitted value).
#' 
#' @param object an instance of \code{FitMultipleGrowthMCMC}.
#' @param ... ignored
#' 
#' @importFrom rlang .data
#' @importFrom dplyr select mutate
#' @importFrom purrr %>%
#' 
#' @export
#' 
fitted.FitMultipleGrowthMCMC <- function(object, ...) {
    
    residuals(object) %>%
        mutate(fitted = .data$logN + .data$res) %>%
        select("exp", "time", "fitted")
    
}

#' @describeIn FitMultipleGrowthMCMC model predictions. They are returned as a tibble
#' with 3 columns: time (storage time), logN (observed count),
#' and exp (name of the experiment).
#'
#' @param object Instance of \code{FitMultipleGrowthMCMC}.
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
predict.FitMultipleGrowthMCMC <- function(object, times=NULL, newdata=NULL, ...) {
    
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

