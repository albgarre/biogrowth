
#' Isothermal Baranyi model
#'
#' Baranyi growth model as defined by Baranyi and Roberts (1994).
#'
#' @param times Numeric vector of storage times
#' @param logN0 Initial log microbial count
#' @param mu Maximum specific growth rate
#' @param lambda Lag phase duration
#' @param logNmax Maximum log microbial count
#'
#' @return Numeric vector with the predicted microbial count.
#'
iso_Baranyi <- function(times, logN0, mu, lambda, logNmax) {

    h0 <- mu*lambda

    A <- times + 1/mu * log(exp(-mu * times) + exp(-h0) - exp(-mu * times - h0))
    logN <- logN0 + mu * A - log(1 + (exp(mu * A) - 1) / exp(logNmax - logN0))

    logN

}

#' Reparameterized Gompertz model
#'
#' Reparameterized Gompertz growth model defined by Zwietering et al. (1990).
#'
#' @inheritParams iso_Baranyi
#' @param C Difference between \code{logN0} and the maximum log-count.
#'
#' @return Numeric vector with the predicted microbial count.
#'
iso_repGompertz <- function(times, logN0, C, mu, lambda) {

    logN <- logN0 + C*(exp(-exp( 2.71*(mu/C)*(lambda-times)+1 )))

    logN

}

#' Trilinear growth model
#'
#' Trilinear growth model defined by Buchanan et al. (1997).
#'
#' @inheritParams iso_Baranyi
#'
#' @return Numeric vector with the predicted microbial count.
#'
trilinear_model <- function(times, logN0, mu, lambda, logNmax) {

    logN <- logN0 + mu*(times - lambda)
    logN[times < lambda] <- logN0
    logN[logN>logNmax] <- logNmax

    logN


}

#' Isothermal microbial growth
#'
#' Predicts microbial growth under isothermal conditions according to
#' models commonly used in predictive microbiology.
#'
#' @param model_name Character defining the growth model.
#' @param times Numeric vector of storage times for the predictions.
#' @param model_pars List defining the values of the model parameters.
#'
#' @return A list of class \code{IsothermalGrowth} with the items:
#' \itemize{
#' \item simulation: A tibble with the model simulation.
#' \item model: The name of the model used for the predictions.
#' \item pars: A list with the values of the model parameters.
#' }
#'
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' ## Define the simulations parameters
#'
#' my_model <- "modGompertz"
#' my_pars <- list(logN0 = 2, C = 6, mu = .2, lambda = 25)
#' my_time <- seq(0, 100, length = 1000)
#'
#' ## Do the simulation
#'
#' static_prediction <- predict_isothermal_growth(my_model, my_time, my_pars)
#'
#' ## Plot the results
#'
#' plot(static_prediction)
#'
#'
predict_isothermal_growth <- function(model_name, times, model_pars) {

    ## Calculate the prediction

    logN <- switch(model_name,
           modGompertz = iso_repGompertz(times, model_pars$logN0, model_pars$C,
                                         model_pars$mu, model_pars$lambda),
           Baranyi = iso_Baranyi(times, model_pars$logN0, model_pars$mu,
                                 model_pars$lambda, model_pars$logNmax),
           Trilinear = trilinear_model(times, model_pars$logN0, model_pars$mu,
                                       model_pars$lambda,model_pars$logNmax),
           stop(paste("Unknown model:", model_name))
           )

    ## Prepare the output

    my_sim <- tibble(time = times, logN = logN)

    out <- list(simulation = my_sim,
                model = model_name,
                pars = model_pars
                )

    class(out) <- c("IsothermalGrowth", class(out))

    out

}








