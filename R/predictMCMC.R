
#' Generic for calculating predictions with uncertainty from fits
#' 
#' @param model Fit object
#' @param times see specific methods for each class
#' @param env_conditions see specific methods for each class
#' @param niter see specific methods for each class
#' @param newpars see specific methods for each class
#' @param formula A formula stating the column named defining the elapsed time in 
#' `env_conditions`. By default, . ~ time.
#' 
#' @export
#' 
predictMCMC <- function(model,
                        times, 
                        env_conditions, 
                        niter,
                        newpars = NULL,
                        formula = . ~ time
                        ) {
    UseMethod("predictMCMC", model)
}

#' @describeIn GrowthFit prediction including parameter uncertainty
#' 
#' @param model An instance of [GrowthFit]
#' @param times Numeric vector of storage times for the predictions.
#' @param env_conditions Tibble with the (dynamic) environmental conditions
#' during the experiment. It must have one column named 'time' with the
#' storage time and as many columns as required with the environmental conditions.
#' @param niter Number of iterations.
#' @param newpars A named list defining new values for the some model parameters. 
#' The name must be the identifier of a model already included in the model. 
#' These parameters do not include variation, so defining a new value for a fitted
#' parameters "fixes" it. `NULL` by default (no new parameters).
#' @param formula A formula stating the column named defining the elapsed time in 
#' `env_conditions`. By default, . ~ time.
#'
#' @return An instance of [MCMCgrowth].
#' 
#' @export
#' 
predictMCMC.GrowthFit <- function(model,
                                  times, 
                                  env_conditions, 
                                  niter,
                                  newpars = NULL,
                                  formula = . ~ time
                                  ) {
    
    if (model$algorithm != "MCMC") {
        stop("predictMCMC is only compatible with MCMC fits")
    }
    
    predict_MCMC_growth(model, times, env_conditions, niter, newpars = newpars,
                        formula = formula)
    
}

#' @describeIn GlobalGrowthFit prediction including parameter uncertainty
#' 
#' @param model An instance of [GlobalGrowthFit]
#' @param times Numeric vector of storage times for the predictions.
#' @param env_conditions Tibble with the (dynamic) environmental conditions
#' during the experiment. It must have one column named 'time' with the
#' storage time and as many columns as required with the environmental conditions.
#' @param niter Number of iterations.
#' @param newpars A named list defining new values for the some model parameters. 
#' The name must be the identifier of a model already included in the model. 
#' These parameters do not include variation, so defining a new value for a fitted
#' parameters "fixes" it. `NULL` by default (no new parameters).
#' @param formula A formula stating the column named defining the elapsed time in 
#' `env_conditions`. By default, . ~ time.
#'
#' @return An instance of [MCMCgrowth].
#' 
#' @export
#' 
predictMCMC.GlobalGrowthFit <- function(model,
                                  times, 
                                  env_conditions, 
                                  niter,
                                  newpars = NULL,
                                  formula = . ~ time
                                  ) {
    
    if (model$algorithm != "MCMC") {
        stop("predictMCMC is only compatible with MCMC fits")
    }
    
    predict_MCMC_growth(model, times, env_conditions, niter, newpars = newpars,
                        formula = formula)
    
}

#' @describeIn FitDynamicGrowthMCMC prediction including parameter uncertainty
#' 
#' @param model An instance of [FitDynamicGrowthMCMC]
#' @param times Numeric vector of storage times for the predictions.
#' @param env_conditions Tibble with the (dynamic) environmental conditions
#' during the experiment. It must have one column named 'time' with the
#' storage time and as many columns as required with the environmental conditions.
#' @param niter Number of iterations.
#' @param newpars A named list defining new values for the some model parameters. 
#' The name must be the identifier of a model already included in the model. 
#' These parameters do not include variation, so defining a new value for a fitted
#' parameters "fixes" it. `NULL` by default (no new parameters).
#' @param formula A formula stating the column named defining the elapsed time in 
#' `env_conditions`. By default, . ~ time.
#'
#' @return An instance of [MCMCgrowth()].
#' 
#' @export
#' 
predictMCMC.FitDynamicGrowthMCMC <- function(model,
                                             times, 
                                             env_conditions, 
                                             niter,
                                             newpars = NULL,
                                             formula = . ~ time
                                             ) {
    
    predict_MCMC_growth(model, times, env_conditions, niter, newpars = newpars,
                        formula = formula)
    
}

#' @describeIn FitMultipleGrowthMCMC prediction including parameter uncertainty
#' 
#' @param model An instance of [FitMultipleGrowthMCMC]
#' @param times Numeric vector of storage times for the predictions.
#' @param env_conditions Tibble with the (dynamic) environmental conditions
#' during the experiment. It must have one column named 'time' with the
#' storage time and as many columns as required with the environmental conditions.
#' @param niter Number of iterations.
#' @param newpars A named list defining new values for the some model parameters. 
#' The name must be the identifier of a model already included in the model. 
#' These parameters do not include variation, so defining a new value for a fitted
#' parameters "fixes" it. `NULL` by default (no new parameters).
#' @param formula A formula stating the column named defining the elapsed time in 
#' `env_conditions`. By default, . ~ time.
#'
#' @return An instance of [MCMCgrowth()].
#' 
#' @export
#' 
predictMCMC.FitMultipleGrowthMCMC <- function(model,
                                              times, 
                                              env_conditions, 
                                              niter,
                                              newpars = NULL,
                                              formula = . ~ time
                                              ) {
    
    predict_MCMC_growth(model, times, env_conditions, niter, newpars = newpars,
                        formula = formula)
    
}
