

#' Residuals of multiple dynamic predictions
#'
#' Function for calculating residuals of dynamic predictions under
#' different conditions for the same model parameters according
#' to the requirements of \code{\link{modFit}}.
#'
#' @param this_p named vector of model parameters
#' @param known_pars named vector of known model parameters
#' @param sec_model_names named character vector with names the
#' environmental conditions and values the secondary model (see secondary_model_data).
#' @param experiment_data a nested list with the experimental data. Each entry describes
#' one experiment as a list with two elements: data and conditions. \code{data} is a tibble
#' with two columns: time and logN. \code{conditions} is a tibble with one column named time
#' and as many additional columns as environmental factors.
#'
#' @return A vector of residuals.
#'
#' @importFrom FME modCost
#'
get_multi_dyna_residuals <- function(this_p, experiment_data,
                                     known_pars, sec_model_names) {

    residuals <- lapply(experiment_data, function(each_experiment) {


        my_cost <- get_dyna_residuals(unlist(this_p), each_experiment$data,
                                      each_experiment$conditions,
                                      unlist(known_pars), sec_model_names)

        my_cost$residuals$res

    })

    unlist(residuals)

}

#' Fitting growth models to multiple dynamic experiments
#'
#' This functions enables to fit a growth model using a dataset comprised of
#' several experiments with potentially different dynamic experimental conditions.
#'
#' @inheritParams get_multi_dyna_residuals
#' @param ... additional arguments for \code{modFit}.
#'
#' @return An instance of
#'
#' @export
#'
fit_multiple_growth <- function(starting_point, experiment_data,
                                known_pars, sec_model_names,
                                ...) {

    ## Fit the model

    my_fit <- modFit(get_multi_dyna_residuals, unlist(starting_point),
                     experiment_data = experiment_data,
                     known_pars = unlist(known_pars),
                     sec_model_names = sec_model_names,
                     ...)

    #- Output the results

    pars_fit <- my_fit$par

    primary_pars <- extract_primary_pars(pars_fit, known_pars)

    secondary_models <- extract_secondary_pars(pars_fit, known_pars,
                                               sec_model_names)

    best_predictions <- lapply(experiment_data, function(each_experiment) {

        times <- seq(0, max(each_experiment$data$time), length=100)

        best_prediction <- predict_dynamic_growth(times, each_experiment$conditions,
                                                  as.list(primary_pars),
                                                  secondary_models)

    })

    names(best_predictions) <- names(experiment_data)

    out <- list(fit_results = my_fit,
                best_prediction = best_predictions,
                data = experiment_data,
                starting = starting_point,
                known = known_pars,
                sec_models = sec_model_names
    )

    class(out) <- c("FitMultipleDynamicGrowth", class(out))
    return(out)


}












