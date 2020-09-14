
#' Generates functions for linear interpolation of environmental conditions
#'
#' @param env_conditions A tibble describing the variation of the environmental
#' conditions through the storage time. Must contain a column named \code{time}
#' and as many additional columns as environmental factors.
#'
#' @return A list of functions that return the value of each environmental
#' condition for some storage time
#'
approx_env <- function(env_conditions) {

    out <- lapply(names(env_conditions[-1]), function(this_col) {

        x <- env_conditions$time
        y <- env_conditions[[this_col]]

        approxfun(x, y, rule = 2)

    })

    names(out) <- names(env_conditions[-1])
    out

}

#' A helper to build the primary models
#'
#' Most of the functions for fitting mix in the vectors parameters for
#' the primary and secondary models, but the functions for making predictions
#' need that they are separated. This one extracts the parameters of the primary
#' model.
#'
#' @param this_p A named vector of model parameters (usually, the ones fitted).
#' @param known_pars Another named vector of model parameters (usually the
#' known ones).
#'
#' @return A list with the parameters of the primary model
#'
extract_primary_pars <- function(this_p, known_pars) {

    primary_pars <- list(mu_opt = NA,
                         Nmax = NA,
                         N0 = NA,
                         Q0 = NA)

    lapply(names(primary_pars), function(par_name) {

        if (par_name %in% names(this_p)) {
            primary_pars[par_name] <<- this_p[[par_name]]
        } else {
            primary_pars[par_name] <<- known_pars[[par_name]]
        }

    })

    primary_pars

}

#' A helper to build the secondary models
#'
#' Most of the functions for fitting mix in the vectors parameters for
#' the primary and secondary models, but the functions for making predictions
#' need that they are separated. This one extracts the parameters of the primary
#' model.
#'
#' @inheritParams extract_primary_pars
#' @param sec_model_names A named character vector defining for each environmental
#' factor (vector names) the type of secondary model (vector values).
#'
#' @return A nested list defining the secondary models.
#'
extract_secondary_pars <- function(this_p, known_pars, sec_model_names) {

    secondary_models <- list()

    lapply(1:length(sec_model_names), function(this_i) {

        # secondary_models[[this_i]] <<- list(model = sec_model_names[[this_i]])

        new_model <- switch(sec_model_names[[this_i]],
                            Zwietering = list(xmin = NA,
                                              xopt = NA,
                                              n = NA),
                            CPM = list(xmin = NA,
                                       xopt = NA,
                                       xmax = NA,
                                       n = NA),
                            stop(paste("Unknown model:", sec_model_names[[this_i]]))
        )

        lapply(names(new_model), function(par_name) {

            full_name <- paste(names(sec_model_names)[[this_i]], par_name, sep = "_")

            if (full_name %in% names(this_p)) {
                new_model[par_name] <<- this_p[[full_name]]
            } else {
                new_model[par_name] <<- known_pars[[full_name]]
            }

        })

        new_model$model <- sec_model_names[[this_i]]

        secondary_models[[this_i]] <<- new_model

    })

    names(secondary_models) <- names(sec_model_names)

    secondary_models

}
