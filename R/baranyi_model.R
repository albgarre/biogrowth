
#' Baranyi growth model
#'
dBaranyi <- function(time, state, pars, env_func, sec_models, sec_pars) {

    pars <- as.list(pars)
    state <- as.list(state)

    alpha <- state$Q/(1 + state$Q)
    beta <- 1 - state$N/pars$Nmax


    gamma <- calculate_gammas(time, env_func, sec_models)
    mu <- pars$mu_opt*prod(gamma)


    dN <- alpha * mu * beta * state$N
    dQ <- mu*state$Q


    list(c(dQ = dQ,
           dN = dN))
}



















