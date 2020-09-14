

#' Residuals of dynamic prediction
#'
#' @importFrom FME modCost
#'
get_dyna_residuals <- function(this_p, fit_data, env_conditions,
                               known_pars, sec_model_names
                               ) {

    ## Build the parameters of the primary model

    primary_pars <- extract_primary_pars(this_p, known_pars)

    ## Build the parameters of the secondary model

    secondary_models <- extract_secondary_pars(this_p, known_pars,
                                               sec_model_names)

    ## Calculate the prediction

    times <- seq(0, max(fit_data$time), length = 1000)

    prediction <- predict_dynamic_growth(times, env_conditions, as.list(primary_pars),
                                         secondary_models)

    ## Calculate residuals

    modCost(model = as.data.frame(prediction$simulation),
            obs = as.data.frame(fit_data)
            )

}

#' Fit dynamic growth models
#'
#' @importFrom FME modFit
#'
#' @export
#'
fit_dynamic_growth <- function(fit_data, env_conditions,
                               starting_point, known_pars,
                               sec_model_names, ...) {

    my_fit <- modFit(get_dyna_residuals, unlist(starting_point),
                     fit_data = fit_data,
                     env_conditions = env_conditions,
                     known_pars = unlist(known_pars),
                     sec_model_names = sec_model_names,
                     ...)

    #- Output the results

    pars_fit <- my_fit$par

    primary_pars <- extract_primary_pars(pars_fit, known_pars)

    secondary_models <- extract_secondary_pars(pars_fit, known_pars,
                                               sec_model_names)



    times <- seq(0, max(fit_data$time), length=100)

    best_prediction <- predict_dynamic_growth(times, env_conditions,
                                              as.list(primary_pars),
                                              secondary_models)

    out <- list(fit_results = my_fit,
                best_prediction = best_prediction,
                data = fit_data,
                starting = starting_point,
                known = known_pars,
                sec_models = sec_model_names
                )

    class(out) <- c("FitDynamicGrowth", class(out))
    return(out)

    }

#' Fit growth models using MCMC
#'
#' @importFrom FME modMCMC
#'
#' @export
#'
fit_MCMC_growth <- function(fit_data, env_conditions,
                               starting_point, known_pars,
                               sec_model_names, niter, ...) {

    my_fit <- modMCMC(get_dyna_residuals, unlist(starting_point),
                     fit_data = fit_data,
                     env_conditions = env_conditions,
                     known_pars = unlist(known_pars),
                     sec_model_names = sec_model_names,
                     niter = niter,
                     ...)

    #- Output the results

    pars_fit <- my_fit$bestpar

    primary_pars <- extract_primary_pars(pars_fit, known_pars)

    secondary_models <- extract_secondary_pars(pars_fit, known_pars,
                                               sec_model_names)



    times <- seq(0, max(fit_data$time), length=100)

    best_prediction <- predict_dynamic_growth(times, env_conditions,
                                              as.list(primary_pars),
                                              secondary_models)

    out <- list(fit_results = my_fit,
                best_prediction = best_prediction,
                data = fit_data,
                starting = starting_point,
                known = known_pars,
                sec_models = sec_model_names
    )

    class(out) <- c("FitDynamicGrowthMCMC", class(out))
    return(out)

}



