
#' Residuals of isothermal prediction
#'
#' @param this_p named vector of model parameters to fit
#' @param fit_data tibble with the data for the fit
#' @param model_name character defining the primary growth model
#' @param known_pars named vector of fixed model parameters
#'
#' @return An instance of \code{modCost}.
#'
#' @importFrom FME modCost
#'
get_iso_residuals <- function(this_p, fit_data, model_name, known_pars) {

    times <- sort(unique(fit_data$time))

    pars <- c(this_p, known_pars)

    predictions <- predict_isothermal_growth(model_name, times, as.list(pars), check=FALSE)

    modCost(model = as.data.frame(predictions$simulation),
            obs = as.data.frame(fit_data))

}


#' Fit isothermal growth models
#'
#' Fits a primary growth model to data obtained under isothermal conditions.
#'
#' @importFrom FME modFit
#'
#' @param fit_data Tibble of data for the fit. It must have a column named
#' \code{time} with the storage time and another named \code{logN} with the
#' microbial count.
#' @param model_name Character defining the primary growth model
#' @param starting_point Named vector of initial values for the model parameters.
#' @param known_pars Named vector of known model parameters (not fitted).
#' @param ... Additional arguments passed to \code{\link{modFit}}.
#' @param check Whether to do some basic checks (TRUE by default).
#'
#' @return An instance of \code{\link{FitIsoGrowth}}.
#'
#' @export
#'
#' @examples
#'
#' ## Some dummy data
#'
#' library(tibble)
#'
#' my_data <- tibble(time = c(0, 25, 50, 75, 100),
#'     logN = c(2, 2.5, 7, 8, 8))
#'
#' ## Choose the model
#'
#' my_model <- "Baranyi"
#'
#' ## Initial values for the model parameters
#'
#' start = c(logNmax = 8, lambda = 25, logN0 = 2)
#'
#' ## Any model parameter can be fixed
#'
#' known <- c(mu = .2)
#'
#' ## Now, we can call the function
#'
#' static_fit <- fit_isothermal_growth(my_data, my_model, start, known)
#'
#' summary(static_fit)
#'
#' ## We can plot the fitted model against the observations
#'
#' plot(static_fit)
#'
fit_isothermal_growth <- function(fit_data, model_name, starting_point,
                                  known_pars, check = TRUE,
                                  ...) {

    ## Check the model parameters

    if (isTRUE(check)) {

        check_primary_pars(model_name, c(starting_point, known_pars))

    }

    ## Fit the model

    my_fit <- modFit(get_iso_residuals, unlist(starting_point),
                     fit_data = fit_data, model_name = model_name,
                     known_pars = known_pars,
                     ...)

    ## Prepare the output

    times <- seq(0, max(fit_data$time), length = 1000)
    pars <- c(my_fit$par, known_pars)

    best_prediction <- predict_isothermal_growth(model_name, times, as.list(pars), check=FALSE)

    out <- list(
        data = fit_data,
        model = model_name,
        starting_point = starting_point,
        known = known_pars,
        fit = my_fit,
        best_prediction = best_prediction
    )

    class(out) <- c("FitIsoGrowth", class(out))
    out

}









