
#' Secondary Cardinal Parameter model
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
zwietering_gamma <- function(x, xmin, xopt, n) {

    gamma <- ((x-xmin)/(xopt-xmin))^n
    gamma[x < xmin] <- 0

    return(gamma)

}

#' Calculates every gamma factor
#'
calculate_gammas <- function(this_t, env_func, sec_models) {

    out <- lapply(names(sec_models), function(this_condition) {

        this_x <- env_func[[this_condition]](this_t)
        this_sec <- sec_models[[this_condition]]

        this_gamma <- switch(this_sec$model,
                             # Ratkowsky = ratkowsky_model(this_x, this_sec$b, this_sec$xmin),
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

###########

#' #' Secondary Ratkowsky model
#' #'
#' ratkowsky_model <- function(x, b, xmin) {
#'
#'     sq_gamma <- b*(x - xmin)
#'     sq_gamma[x<xmin] <- 0
#'     return(sq_gamma^2)
#'
#' }

