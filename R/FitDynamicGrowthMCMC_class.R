
#' FitDynamicGrowthMCMC class
#' 
#' @description 
#' The \code{FitDynamicGrowthMCMC} a model fitted based on a dynamic growth experiment
#' using an MCMC algorithm. Its constructor is \code{\link{fit_MCMC_growth}}.
#' 
#' It is a subclass of list with the items:
#'      \itemize{
#'          \item fit_results: the object returned by \code{modMCMC}.
#'          \item best_prediction: the model prediction for the fitted parameters.
#'          \item env_conditions: environmental conditions for the fit.
#'          \item data: data used for the fit.
#'          \item starting: starting values for model fitting
#'          \item known: parameter values set as known.
#'          \item sec_models: a named vector with the secondary model
#'          for each environmental factor
#'          }
#' 
#' @name FitDynamicGrowthMCMC
#'   
NULL

#' @describeIn FitDynamicGrowthMCMC compares the model fitted against the data.
#'
#' @param x The object of class \code{FitDynamicGrowthMCMC} to plot.
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
plot.FitDynamicGrowthMCMC <- function(x, y=NULL, ...,
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

#' @describeIn FitDynamicGrowthMCMC statistical summary of the fit.
#'
#' @param object Instance of \code{FitDynamicGrowthMCMC}.
#' @param ... ignored
#'
#' @export
#'
summary.FitDynamicGrowthMCMC <- function(object, ...) {
    
    summary(object$fit_results)
    
}

#' @describeIn FitDynamicGrowthMCMC model residuals.
#'
#' @param object Instance of \code{FitDynamicGrowthMCMC}.
#' @param ... ignored.
#'
#' @importFrom dplyr select
#' @importFrom FME modCost
#' 
#' @importFrom stats predict
#'
#' @export
#'
residuals.FitDynamicGrowthMCMC <- function(object, ...) {
    
    pred <- predict(object)
    
    pred - object$data$logN

}

#' @describeIn FitDynamicGrowthMCMC vector of fitted model parameters.
#'
#' @param object an instance of \code{FitDynamicGrowthMCMC}.
#' @param ... ignored
#'
#' @export
#'
coef.FitDynamicGrowthMCMC <- function(object, ...) {
    
    object$fit_results$bestpar
    
}

#' @describeIn FitDynamicGrowthMCMC variance-covariance matrix of the model,
#'  estimated as the variance of the samples from the Markov chain.
#'
#' @param object an instance of \code{FitDynamicGrowthMCMC}.
#' @param ... ignored
#'
#' @importFrom stats cov
#'
#' @export
#'
vcov.FitDynamicGrowthMCMC <- function(object, ...) {
    
    cov(object$fit_results$pars)
    
}

#' @describeIn FitDynamicGrowthMCMC deviance of the model, calculated as the sum
#' of squared residuals for the parameter values resulting in the best fit.
#'
#' @param object an instance of \code{FitDynamicGrowthMCMC}.
#' @param ... ignored
#'
#' @importFrom dplyr select
#' @importFrom FME modCost
#'
#' @export
#'
deviance.FitDynamicGrowthMCMC <- function(object, ...) {
    
    model <- object$best_prediction$simulation %>%
        select("time", "logN") %>%
        as.data.frame()
    
    obs <- object$data %>%
        select("time", "logN") %>%
        as.data.frame()
    
    modCost(model, obs)$residuals$res^2 %>% sum()
    
}

#' @describeIn FitDynamicGrowthMCMC vector of fitted values.
#' 
#' @param object an instance of \code{FitDynamicGrowthMCMC}.
#' @param ... ignored
#' 
#' @export
#' 
fitted.FitDynamicGrowthMCMC <- function(object, ...) {
    
    object$data$logN + residuals(object)
    
}

#' @describeIn FitDynamicGrowthMCMC vector of model predictions.
#' 
#' @param object an instance of \code{FitDynamicGrowthMCMC}.
#' @param ... ignored
#' @param newdata a tibble describing the environmental conditions (as \code{env_conditions})
#' in \code{\link{predict_dynamic_growth}}. 
#' If \code{NULL} (default), uses the same conditions as those for fitting.
#' 
#' @export
#' 
predict.FitDynamicGrowthMCMC <- function(object, newdata = NULL, ...) {
    
    if (is.null(newdata)) {
        
        newdata <- object$env_conditions
        
    }
    
    
    pred <- predict_dynamic_growth(
        object$data$time,
        newdata,
        object$best_prediction$primary_pars,
        object$best_prediction$sec_models
    )
    
    pred$simulation$logN
    
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
#' @param newdata a tibble describing the environmental conditions (as \code{env_conditions})
#' in \code{\link{fit_multiple_growth}}. 
#' If \code{NULL} (default), uses the same conditions as those for fitting.
#' 
#' @importFrom dplyr bind_rows
#'
#' @export
#'
predict.FitMultipleGrowthMCMC <- function(object, newdata=NULL, ...) {
    
    if (is.null(newdata)) {
        
        newdata <- object$data
        
    }
    
    out <- lapply(1:length(newdata), function(i) {
        
        times <- newdata[[i]]$data$time
        
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











