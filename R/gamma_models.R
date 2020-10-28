
#' Secondary Cardinal Parameter (CPM) model
#'
#' Secondary cardinal parameter model as defined by Rosso et al. (1995).
#'
#' @param x Value of the environmental factor.
#' @param xmin Minimum value for growth.
#' @param xopt Optimum value for growth.
#' @param xmax Maximum value for growth.
#' @param n Order of the CPM model.
#'
#' @return The corresponding gamma factor.
#'
CPM_model <- function(x, xmin, xopt, xmax, n) {

    num <- (x-xmax)*(x-xmin)^n
    den <- (xopt-xmin)^(n-1)*( (xopt-xmin)*(x-xopt) - (xopt-xmax)*((n-1)*xopt + xmin-n*x) )
    gamma <- num/den
    gamma[x < xmin] <- 0
    gamma[x > xmax] <- 0

    return(gamma)

}

#' Zwietering gamma model
#'
#' Gamma model as defined by Zwietering et al. (1992).
#'
#' @param x Value of the environmental factor.
#' @param xmin Minimum value of the environmental factor for growth.
#' @param xopt Maximum value for growth
#' @param n Exponent of the secondary model
#'
#' @return The corresponding gamma factor.
#'
zwietering_gamma <- function(x, xmin, xopt, n) {

    gamma <- ((x-xmin)/(xopt-xmin))^n
    gamma[x < xmin] <- 0

    return(gamma)

}

#' Full Ratkowsky model
#'
#' Gamma model adapted from the one by Ratkowsky et al. (1983).
#'
#' @param x Value of the environmental factor.
#' @param xmin Minimum value for growth
#' @param xmax Maximum value for growth
#' @param c Parameter defining the speed of the decline
#'
#' @importFrom lamW lambertW0
#'
full_Ratkowski <- function(x, xmin, xmax, c) {

    b <- 1 # Does not affect predictions (see supp. material)

    xopt <- (lambertW0(exp(-xmin*c + xmax*c + 1)) + c*xmin - 1)/c

    mu_opt <- b*(xopt - xmin)*(1 - exp(c*(xopt - xmax)))

    gamma <- b*(x - xmin)*(1 - exp(c*(x - xmax)))
    gamma <- gamma/mu_opt

    gamma[x < xmin] <- 0
    gamma[x > xmax] <- 0

    return(gamma)

}

#' Calculates every gamma factor
#'
#' A helper function for \code{\link{predict_dynamic_growth}} that
#' calculates the value of every gamma factor corresponding to some
#' storage time.
#'
#' @param this_t Storage time
#' @param env_func A list of functions (generated using \code{approxfun}) that
#' give the value of each environmental function for some storage time.
#' @param sec_models A nested list describing the secondary models.
#'
#' @return A vector of gamma factors (one per environmental factor).
#'
calculate_gammas <- function(this_t, env_func, sec_models) {

    out <- lapply(names(sec_models), function(this_condition) {

        this_x <- env_func[[this_condition]](this_t)
        this_sec <- sec_models[[this_condition]]

        this_gamma <- switch(this_sec$model,
                             fullRatkowsky = full_Ratkowski(this_x, this_sec$xmin,
                                                            this_sec$xmax, this_sec$c),
                             CPM = CPM_model(this_x, this_sec$xmin,
                                             this_sec$xopt, this_sec$xmax, this_sec$n),
                             Zwietering = zwietering_gamma(this_x, this_sec$xmin, this_sec$xopt, this_sec$n),
                             stop(paste("Model", this_sec$model, "not known."))
        )

        this_gamma

    })

    out <- unlist(out)
    names(out) <- names(sec_models)
    out

}


