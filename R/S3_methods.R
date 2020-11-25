
## "is." methods

#' Test of FitDynamicGrowth object
#'
#' Tests if an object is of class \code{FitDynamicGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{FitDynamicGrowth}
#'
#' @export
#'
is.FitDynamicGrowth <- function(x) inherits(x, "FitDynamicGrowth")

#' Test of FitDynamicGrowthMCMC object
#'
#' Tests if an object is of class \code{FitDynamicGrowthMCMC}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{FitDynamicGrowthMCMC}
#'
#' @export
#'
is.FitDynamicGrowthMCMC <- function(x) inherits(x, "FitDynamicGrowthMCMC")

#' Test of FitIsoGrowth object
#'
#' Tests if an object is of class \code{FitIsoGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{FitIsoGrowth}
#'
#' @export
#'
is.FitIsoGrowth <- function(x) inherits(x, "FitIsoGrowth")

#' Test of FitSecondaryGrowth object
#'
#' Tests if an object is of class \code{FitSecondaryGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{FitSecondaryGrowth}
#'
#' @export
#'
is.FitSecondaryGrowth <- function(x) inherits(x, "FitSecondaryGrowth")

#' Test of DynamicGrowth object
#'
#' Tests if an object is of class \code{DynamicGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{DynamicGrowth}
#'
#' @export
#'
is.DynamicGrowth <- function(x) inherits(x, "DynamicGrowth")

#' Test of IsothermalGrowth object
#'
#' Tests if an object is of class \code{IsothermalGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{IsothermalGrowth}
#'
#' @export
#'
is.IsothermalGrowth <- function(x) inherits(x, "IsothermalGrowth")

#' Test of MCMCgrowth object
#'
#' Tests if an object is of class \code{MCMCgrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{MCMCgrowth}
#'
#' @export
#'
is.MCMCgrowth <- function(x) inherits(x, "MCMCgrowth")

#' Test of StochasticGrowth object
#'
#' Tests if an object is of class \code{StochasticGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{StochasticGrowth}
#'
#' @export
#'
is.StochasticGrowth <- function(x) inherits(x, "StochasticGrowth")

#' Test of FitMultipleDynamicGrowth object
#'
#' Tests if an object is of class \code{FitMultipleDynamicGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{FitMultipleDynamicGrowth}
#'
#' @export
#'
is.FitMultipleDynamicGrowth <- function(x) inherits(x, "FitMultipleDynamicGrowth")

#' Test of FitMultipleDynamicGrowthMCMC object
#'
#' Tests if an object is of class \code{FitMultipleDynamicGrowthMCMC}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{FitMultipleDynamicGrowthMCMC}
#'
#' @export
#'
is.FitMultipleDynamicGrowthMCMC <- function(x) inherits(x, "FitMultipleGrowthMCMC")


#------------------------------------------------------------------------------

## "summary" methods

#' Summary of a FitDynamicGrowth object
#'
#' @param object Instance of FitDynamicGrowth
#' @param ... ignored
#'
#' @export
#'
summary.FitDynamicGrowth <- function(object, ...) {

    summary(object$fit_results)

}

#' Summary of a FitDynamicGrowthMCMC object
#'
#' @param object Instance of FitDynamicGrowthMCMC
#' @param ... ignored
#'
#' @export
#'
summary.FitDynamicGrowthMCMC <- function(object, ...) {

    summary(object$fit_results)

}

#' Summary of a FitIsoGrowth object
#'
#' @param object Instance of FitIsoGrowth
#' @param ... ignored
#'
#' @export
#'
summary.FitIsoGrowth <- function(object, ...) {

    summary(object$fit)

}

#' Summary of a FitSecondaryGrowth object
#'
#' @param object Instance of FitSecondaryGrowth
#' @param ... ignored
#'
#' @export
#'
summary.FitSecondaryGrowth <- function(object, ...) {

    summary(object$fit_results)

}

#' Summary of a FitSecondaryGrowth object
#'
#' @param object Instance of FitSecondaryGrowth
#' @param ... ignored
#'
#' @export
#'
summary.FitSecondaryGrowth <- function(object, ...) {

    summary(object$fit_results)

}

#' Summary of a FitMultipleDynamicGrowth object
#'
#' @param object Instance of FitMultipleDynamicGrowth
#' @param ... ignored
#'
#' @export
#'
summary.FitMultipleDynamicGrowth <- function(object, ...) {

    summary(object$fit_results)

}

#' Summary of a FitMultipleGrowthMCMC object
#'
#' @param object instance of FitMultipleGrowthMCMC.
#' @param ... ignored.
#'
#' @export
#'
summary.FitMultipleGrowthMCMC <- function(object, ...) {

    summary(object$fit_results)

}

#---------------------------------------------------------

#' Residuals of a FitSecondaryGrowth object
#'
#' @param object Instance of FitSecondaryGrowth
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitSecondaryGrowth <- function(object, ...) {
    residuals(object$fit_results)
}

#' Residuals of FitIsoGrowth object
#'
#' @param object Instance of FitIsoGrowth
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitIsoGrowth <- function(object, ...) {
    residuals(object$fit)
}

#' Residuals of FitDynamicGrowth
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


#' Residuals of FitDynamicGrowthMCMC
#'
#' @param object Instance of FitDynamicGrowthMCMC.
#' @param ... ignored.
#'
#' @importFrom dplyr select
#' @importFrom FME modCost
#'
#' @export
#'
residuals.FitDynamicGrowthMCMC <- function(object, ...) {
    
    pred <- predict(object)
    
    pred - my_MCMC_fit$data$logN

    # simulations <- object$best_prediction$simulation %>%
    #     select("time", "logN") %>%
    #     as.data.frame()
    # 
    # my_cost <- modCost(model = simulations,
    #             obs = as.data.frame(object$data))
    # 
    # my_cost$residuals$res

}

#' Residuals of FitMultipleDynamicGrowth
#'
#' @param object Instance of FitMultipleDynamicGrowth
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @return A table with 4 columns: time (storage time), logN (observed count),
#' exp (name of the experiment) and res (residual).
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

#' Residuals of FitMultipleGrowthMCMC
#'
#' @param object Instance of FitMultipleGrowthMCMC
#' @param ... ignored
#'
#' @importFrom dplyr bind_rows select
#' @importFrom FME modCost
#'
#' @return A table with 4 columns: time (storage time), logN (observed count),
#' exp (name of the experiment) and res (residual).
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

#-----------------------------------------------

#' Fitted parameters of an isothermal fit
#'
#' @param object an instance of \code{FitIsoGrowth}.
#' @param ... ignored
#'
#' @importFrom stats coef
#'
#' @export
#'
coef.FitIsoGrowth <- function(object, ...) {
    
    coef(object$fit)
    
}

#' Fitted parameters of a dynamic fit
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

#' Fitted parameters of an MCMC fit
#'
#' @param object an instance of \code{FitDynamicGrowthMCMC}.
#' @param ... ignored
#'
#' @export
#'
coef.FitDynamicGrowthMCMC <- function(object, ...) {
    
    object$fit_results$bestpar
    
}

#' Fitted parameters of a global fit
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

#' Fitted parameters of a global fit using MCMC
#'
#' @param object an instance of \code{FitMultipleGrowthMCMC}.
#' @param ... ignored
#'
#' @export
#'
coef.FitMultipleGrowthMCMC <- function(object, ...) {
    
    object$fit_results$bestpar
    
}

#' Fitted parameters of a secondary model fit
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

#--------------------------------------------------------------

#' Variance matrix of an isothermal fit
#'
#' Returns the unscaled covariance matrix, calculated as 1/(0.5*Hessian)
#'
#' @param object an instance of \code{FitIsoGrowth}.
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

#' Variance matrix of a dynamic fit
#'
#' Returns the unscaled covariance matrix, calculated as 1/(0.5*Hessian)
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

#' Variance matrix of an MCMC dynamic fit
#'
#' Returns the covariance, estimated as the variance of the samples
#' from the Markov chain.
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

#' Variance matrix of a global fit
#'
#' Returns the unscaled covariance matrix, calculated as 1/(0.5*Hessian)
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

#' Variance matrix of a global fit using MCMC
#'
#' Returns the covariance, estimated as the variance of the samples
#' from the Markov chain.
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

#' Variance matrix of a cardinal fit
#'
#' Returns the unscaled covariance matrix, calculated as 1/(0.5*Hessian)
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

#-------------------------------------------------------------------

#' Deviance of an isothermal fit
#'
#' @param object an instance of \code{FitIsoGrowth}.
#' @param ... ignored
#'
#' @export
#'
deviance.FitIsoGrowth <- function(object, ...) {
    
    deviance(object$fit)
    
}

#' Deviance of a dynamic fit
#'
#' @param object an instance of \code{FitDynamicGrowth}.
#' @param ... ignored
#'
#' @export
#'
deviance.FitDynamicGrowth <- function(object, ...) {
    
    deviance(object$fit_results)
    
}

#' Deviance of an MCMC fit
#'
#' Returns the deviance of the best fit, calculated as the sum of
#' squared residuals.
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

#' Deviance of a global fit
#'
#' @param object an instance of \code{FitMultipleDynamicGrowth}.
#' @param ... ignored
#'
#' @export
#'
deviance.FitMultipleDynamicGrowth <- function(object, ...) {
    
    deviance(object$fit_results)
    
}

#' Deviance of a global fit using MCMC
#'
#' Returns the deviance of the best fit, calculated as the sum of
#' squared residuals.
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

#' Deviance of a cardinal fit
#'
#' @param object an instance of \code{FitSecondaryGrowth}.
#' @param ... ignored
#'
#' @export
#'
deviance.FitSecondaryGrowth <- function(object, ...) {
    
    deviance(object$fit_results)
    
}

#--------------------------------------------------------------------

#' Fitted values of static fit
#' 
#' @param object an instance of \code{itIsoGrowth }.
#' @param ... ignored
#' 
#' @export
#' 
fitted.FitIsoGrowth <- function(object, ...) {
    
    object$data$logN + residuals(object)
    
}

#' Fitted values of dynamic fit
#' 
#' @param object an instance of \code{FitDynamicGrowth}.
#' @param ... ignored
#' 
#' @export
#' 
fitted.FitDynamicGrowth <- function(object, ...) {
    
    object$data$logN + residuals(object)
    
}

#' Fitted values of MCMC fit
#' 
#' @param object an instance of \code{FitDynamicGrowthMCMC}.
#' @param ... ignored
#' 
#' @export
#' 
fitted.FitDynamicGrowthMCMC <- function(object, ...) {
    
    object$data$logN + residuals(object)
    
}

#' Fitted values of global fit
#' 
#' @param object an instance of \code{FitMultipleDynamicGrowth}.
#' @param ... ignored
#' 
#' @importFrom rlang .data
#' @importFrom dplyr select mutate
#' @importFrom purrr %>%
#' 
#' @return A tibble with 3 columns: time (storage time), exp (experiment 
#' identifier) and fitted (fitted value).
#' 
#' @export
#' 
fitted.FitMultipleDynamicGrowth <- function(object, ...) {
    
    residuals(object) %>%
        mutate(fitted = .data$logN + .data$res) %>%
        select("exp", "time", "fitted")
    
}

#' Fitted values of global fit with MCMC
#' 
#' @param object an instance of \code{FitMultipleGrowthMCMC}.
#' @param ... ignored
#' 
#' @importFrom rlang .data
#' @importFrom dplyr select mutate
#' @importFrom purrr %>%
#' 
#' @return A tibble with 3 columns: time (storage time), exp (experiment 
#' identifier) and fitted (fitted value).
#' 
#' @export
#' 
fitted.FitMultipleGrowthMCMC <- function(object, ...) {
    
    residuals(object) %>%
        mutate(fitted = .data$logN + .data$res) %>%
        select("exp", "time", "fitted")
    
}

#' Fitted values of cardinal fit
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

#------------------------------------------------------------------------

#' Model prediction for static fit
#' 
#' 
#' @param object an instance of FitIsoGrowth
#' @param ... ignored
#' @param times numeric vector describing the time points for the prediction.
#' If \code{NULL} (default), uses the same points as those used for fitting.
#' 
#' @export
#' 
predict.FitIsoGrowth <- function(object, times = NULL, ...) {
    
    if (is.null(times)) {
        
        times <- object$data$time
        
    }
    
    
    pred <- predict_isothermal_growth(object$model,
                              times,
                              object$best_prediction$pars,
                              check=FALSE)
    
    pred$simulation$logN
    
}

#' Model prediction for dynamic fit
#' 
#' 
#' @param object an instance of FitDynamicGrowth
#' @param ... ignored
#' @param newdata a tibble describing the environmental conditions (as \code{env_conditions})
#' in \code{\link{predict_dynamic_growth}}. 
#' If \code{NULL} (default), uses the same conditions as those for fitting.
#' 
#' @export
#' 
predict.FitDynamicGrowth <- function(object, newdata = NULL, ...) {
    
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

#' Model prediction for dynamic fit using MCMC
#' 
#' 
#' @param object an instance of FitDynamicGrowthMCMC
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

#' Model predictions of FitMultipleDynamicGrowth
#'
#' @param object Instance of FitMultipleDynamicGrowth
#' @param ... ignored
#' @param newdata a tibble describing the environmental conditions (as \code{env_conditions})
#' in \code{\link{fit_multiple_growth}}. 
#' If \code{NULL} (default), uses the same conditions as those for fitting.
#' 
#' @importFrom dplyr bind_rows
#'
#' @return A table with 3 columns: time (storage time), logN (observed count),
#' and exp (name of the experiment).
#'
#' @export
#'
predict.FitMultipleDynamicGrowth <- function(object, newdata=NULL, ...) {
    
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

#' Model predictions of FitMultipleGrowthMCMC
#'
#' @param object Instance of FitMultipleGrowthMCMC
#' @param ... ignored
#' @param newdata a tibble describing the environmental conditions (as \code{env_conditions})
#' in \code{\link{fit_multiple_growth}}. 
#' If \code{NULL} (default), uses the same conditions as those for fitting.
#' 
#' @importFrom dplyr bind_rows
#'
#' @return A table with 3 columns: time (storage time), logN (observed count),
#' and exp (name of the experiment).
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

#' Model predictions for FitSecondaryGrowth
#' 
#' @param object an instance of FitSecondaryGrowth
#' @param ... ignored
#' @param newdata
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



















