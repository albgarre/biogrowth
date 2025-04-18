
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
#' Gamma model as defined by Zwietering et al. (1992). To avoid unreasonable predictions,
#' it has been modified setting gamma=0 for values of x outside (xmin, xopt)
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
    gamma[x > xopt] <- 0

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
    gamma <- gamma^2

    gamma[x < xmin] <- 0
    gamma[x > xmax] <- 0

    return(gamma)

}

#' Secondary Aryani model
#'
#' Secondary model as defined by Aryani et al. (2015).
#'
#' @param x Value of the environmental factor.
#' @param xmin Minimum value for growth.
#' @param xhalf Value where gamma = 0.5
#'
#' @return The corresponding gamma factor.
#'
Aryani_model <- function(x, xmin, xhalf) {
  
  gamma <- 1 - 2^( -(x - xmin)/(xhalf - xmin) )
  
  gamma[x < xmin] <- 0
  
  return(gamma)
  
}

#' Secondary Rosso model for water activity
#'
#' Secondary model for water activity as defined by Aryani et al. (2001).
#'
#' @param x Value of the environmental factor (in principle, aw).
#' @param xmin Minimum value for growth (in principle, aw).
#'
#' @return The corresponding gamma factor.
#'
Rossoaw_model <- function(x, xmin) {
  
  gamma <- (x - xmin)/(1 - xmin)
  
  gamma[x < xmin] <- 0
  gamma[x > 1] <- 0
  
  return(gamma)
  
}

#' Secondary model for inhibitory compounds
#'
#' Secondary model for the effect of inhibitory compounds.
#'
#' @param x Value of the environmental factor (in principle, concentration of compound).
#' @param MIC Minimum Inhibitory Concentration
#' @param alpha shape factor of the miodel
#'
#' @return The corresponding gamma factor.
#'
inhibitory_model <- function(x, MIC, alpha) {
  
  gamma <- 1 - (x/MIC)^alpha
  
  gamma[x > MIC] <- 0
  
  return(gamma)
  
}

#' Calculates every gamma factor
#'
#' A helper function for [predict_dynamic_growth()] that
#' calculates the value of every gamma factor corresponding to some
#' storage time.
#'
#' @param this_t Storage time
#' @param env_func A list of functions (generated using `approxfun`) that
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
                             Aryani = Aryani_model(this_x, this_sec$xmin, this_sec$xhalf),
                             Rosso_aw = Rossoaw_model(this_x, this_sec$xmin),
                             Inhibitory = inhibitory_model(this_x, this_sec$MIC, this_sec$alpha),
                             stop(paste("Model", this_sec$model, "not known."))
        )

        this_gamma

    })

    out <- unlist(out)
    names(out) <- names(sec_models)
    out

}


