
#' Reparameterized Gompertz model
#'
iso_repGompertz <- function(times, logN0, C, mu, lambda) {

    logN <- logN0 + C*(exp(-exp( 2.71*(mu/C)*(lambda-times)+1 )))

    logN

}


#' Isothermal Baranyi model
#'
iso_Baranyi <- function(times, logN0, mu, lambda, logNmax, m=1) {

    h0 <- mu*lambda

    A <- times + 1/mu * log(exp(-mu * times) + exp(-h0) - exp(-mu * times - h0))
    logN <- logN0 + mu * A - log(1 + (exp(mu * A) - 1) / exp(logNmax - logN0))

    logN

}

#' Trilinear growth model
#'
trilinear_model <- function(times, logN0, mu, lambda, logNmax) {

    logN <- logN0 + mu*(times - lambda)
    logN[times < lambda] <- logN0
    logN[logN>logNmax] <- logNmax

    logN


}

#' Isothermal microbial growth
#'
#' @importFrom tibble tibble
#'
#' @export
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








