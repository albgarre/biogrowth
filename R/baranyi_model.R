
#' Baranyi growth model
#'
#' Microbial growth model as defined in Baranyi and Roberts (1994). It has
#' been implemented according to the requirements of
#' [deSolve::ode()]. For consistency in the function for isothermal growth,
#' calculations are done assuming the user input for mu is in log10 scale. In other words,
#' the input is multiplied by ln(10).
#'
#' @param time numeric vector (length 1) of storage time
#' @param state named numeric vector with two components: Q and N
#' @param pars named numeric vector of model parameters (Nmax and mu_opt)
#' @param env_func named list of functions returning the values of
#' the environmental conditions for time (t)
#' @param sec_models named list of parameters of the secondary model
#'
#' @return A numeric vector of two components according to the requirements of
#' [deSolve::ode()].
#'
dBaranyi <- function(time, state, pars, env_func, sec_models) {

    pars <- as.list(pars)
    state <- as.list(state)

    alpha <- state$Q/(1 + state$Q)
    beta <- 1 - state$N/pars$Nmax


    gamma <- calculate_gammas(time, env_func, sec_models)
    mu <- pars$mu_opt*prod(gamma)
    
    mu <- mu*log(10)  # Convert to ln CFU/[t]


    dN <- alpha * mu * beta * state$N 
    dQ <- mu*state$Q


    list(c(dQ = dQ,
           dN = dN))
}



















