
#' Generates functions for linear interpolation of environmental conditions
#'
#' @param env_conditions A tibble describing the variation of the environmental
#' conditions through the storage time. Must contain a column named `time`
#' and as many additional columns as environmental factors.
#'
#' @importFrom stats approxfun
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

    my_names <- c("mu_opt", "Nmax", "N0", "Q0")

    primary_pars <- lapply(my_names, function(par_name) {

            if (par_name %in% names(this_p)) {
                this_p[[par_name]]
            } else {
                known_pars[[par_name]]
            }

    })

    names(primary_pars) <- my_names

    primary_pars

}

#' A helper to build the secondary models
#'
#' Most of the functions for fitting mix in the vectors parameters for
#' the primary and secondary models, but the functions for making predictions
#' need that they are separated. This one extracts the parameters of the secondary
#' model.
#'
#' @inheritParams extract_primary_pars
#' @param sec_model_names A named character vector defining for each environmental
#' factor (vector names) the type of secondary model (vector values).
#'
#' @return A nested list defining the secondary models.
#'
extract_secondary_pars <- function(this_p, known_pars, sec_model_names) {

    secondary_models <- lapply(1:length(sec_model_names), function(this_i) {
        
        all_names <- secondary_model_data(sec_model_names[[this_i]])$pars

        new_model <- lapply(all_names, function(par_name) {

            full_name <- paste(names(sec_model_names)[[this_i]], par_name, sep = "_")

            if (full_name %in% names(this_p)) {
                this_p[[full_name]]
            } else {
                known_pars[[full_name]]
            }

        })

        names(new_model) <- all_names

        new_model$model <- sec_model_names[[this_i]]

        new_model

    })

    names(secondary_models) <- names(sec_model_names)

    # print(secondary_models)

    secondary_models

}

#' Lag phase duration from Q0
#' 
#' @description 
#' `r lifecycle::badge("stable")`
#' 
#' Convenience function to calculate the lag phase duration (lambda) of the 
#' Baranyi model from the
#' maximum specific growth rate and the initial value of the variable Q.
#' 
#' Note that this function uses the unit system of biogrowth (i.e. log10). Care must
#' be taken when using parameters obtained from other sources.
#' 
#' @param q0 Initial value of the variable Q.
#' @param mu Specific growth rate in the exponential phase.
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, 10 (i.e. log10). See vignette about units for details. 
#' 
#' @export
#' 
Q0_to_lambda <- function(q0, mu, logbase_mu = 10) {
    
    ## Convert mu to base e
    
    mu <- mu*log(logbase_mu)
    
    ## Make the calculation
    
    log(1 +1/q0)/mu
}

#' Q0 from lag phase duration
#' 
#' @description 
#' `r lifecycle::badge("stable")`
#' 
#' Convenience function to calculate the value of Q0 for the Baranyi model from
#' the duration of the lag phase
#' 
#' @param lambda Duration of the lag phase.
#' @param mu Specific growth rate in the exponential phase.
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, 10 (i.e. log10). See vignette about units for details. 
#' 
#' @export
#' 
lambda_to_Q0 <- function(lambda, mu, logbase_mu = 10) {
    
    ## Convert mu to base e
    
    mu <- mu*log(logbase_mu)
    
    ## Make the calculation
    
    1/(exp(mu*lambda) - 1)
}









