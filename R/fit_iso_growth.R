
#' Residuals of isothermal prediction
#'
#' @importFrom FME modCost
#'
get_iso_residuals <- function(this_p, fit_data, model_name, known_pars) {

    times <- sort(unique(fit_data$time))

    pars <- c(this_p, known_pars)

    predictions <- predict_isothermal_growth(model_name, times, as.list(pars))

    modCost(model = as.data.frame(predictions$simulation),
            obs = as.data.frame(fit_data))

}


#' Fit isothermal growth models
#'
#' @importFrom FME modFit
#'
#' @export
#'
fit_isothermal_growth <- function(fit_data, model_name, starting_point,
                                  known_pars,
                                  ...) {

    my_fit <- modFit(get_iso_residuals, unlist(starting_point),
                     fit_data = fit_data, model_name = model_name,
                     known_pars = known_pars,
                     ...)

    out <- list(
        data = fit_data,
        model = model_name,
        starting_point = starting_point,
        known = known_pars,
        fit = my_fit
    )

    class(out) <- c("FitIsoGrowth", class(out))
    out

}









