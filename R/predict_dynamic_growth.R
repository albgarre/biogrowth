
#' Growth under dynamic conditions
#'
#' Predicts microbial growth under dynamic conditions based on the
#' Baranyi model (Baranyi and Roberts, 1994) and secondary models
#' based on the gamma concept (Zwietering et al. 1992).
#'
#' Model predictions are done by linear interpolation of the environmental
#' conditions defined in \code{env_conditions}.
#'
#' For consistency with the function for isothermal growth,
#' calculations are done considering mu is in log10 scale. In other words,
#' it is multiplied by ln(10).
#'
#' @param times Numeric vector of storage times to make the predictions
#' @param env_conditions Tibble describing the variation of the environmental
#' conditions during storage. It must have a column named \code{time} with the
#' storage time and as many additional columns as environmental factors.
#' @param primary_pars A named list defining the parameters of the primary model
#' and the initial values of the model variables. That is, with names \code{mu_opt},
#' \code{Nmax}, \code{N0}, \code{Q0}.
#' @param secondary_models A nested list describing the secondary models.
#' @param ... Additional arguments for \code{\link{ode}}.
#' @param check Whether to check the validity of the models. \code{TRUE} by default.
#'
#' @return An instance of \code{\link{DynamicGrowth}}.
#'
#' @importFrom deSolve ode
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_cols
#' @importFrom rlang .data
#' @imoprtFrom purrr map
#'
#' @export
#'
#' @examples
#'
#' ## Definition of the environmental conditions
#'
#' library(tibble)
#'
#' my_conditions <- tibble(time = c(0, 5, 40),
#'     temperature = c(20, 30, 35),
#'     pH = c(7, 6.5, 5)
#'     )
#'
#' ## Definition of the model parameters
#'
#' my_primary <- list(mu_opt = 2,
#'     Nmax = 1e8,N0 = 1e0,
#'     Q0 = 1e-3)
#'
#' sec_temperature <- list(model = "Zwietering",
#'     xmin = 25, xopt = 35, n = 1)
#'
#' sec_pH = list(model = "CPM",
#'     xmin = 5.5, xopt = 6.5,
#'     xmax = 7.5, n = 2)
#'
#' my_secondary <- list(
#'     temperature = sec_temperature,
#'     pH = sec_pH
#'     )
#'
#' my_times <- seq(0, 50, length = 1000)
#'
#' ## Do the simulation
#'
#' dynamic_prediction <- predict_dynamic_growth(my_times,
#'     my_conditions, my_primary,
#'     my_secondary)
#'
#' ## Plot the results
#'
#' plot(dynamic_prediction)
#'
#' ## We can plot some environmental factor with add_factor
#'
#' plot(dynamic_prediction, add_factor = "temperature", ylims= c(0, 8),
#'     label_y1 = "Microbial count (log CFU/ml)",
#'     label_y2 = "Storage temperature (C)")
#'
predict_dynamic_growth <- function(times, env_conditions, primary_pars,
                                   secondary_models, ...,
                                   check = TRUE) {
    
    ## Check model parameters
    
    if (isTRUE(check)) {
        
        sec_model_names <- secondary_models %>%
            map(~ .$model) %>% 
            unlist()
        
        check_pars_names <- lapply(names(secondary_models), function(each_factor) {
            
            this_model <- secondary_models[[each_factor]]
            this_model$model <- NULL
            paste0(each_factor, "_", names(this_model))
        }) %>%
            unlist()
        
        check_pars <- rep(1, length(check_pars_names))  # I do not check values, so it does not matter
        names(check_pars) <- check_pars_names
        
        
        check_secondary_pars(check_pars, unlist(primary_pars), sec_model_names,
                             primary_pars = c("mu_opt", "N0", "Nmax", "Q0"))
        
    }

    ## Prepare stuff

    my_env <- approx_env(env_conditions)

    yini <- c(Q = primary_pars$Q0,
              N = primary_pars$N0
              )

    ## Make the simulation

    my_sim <- ode(yini, times, dBaranyi, primary_pars,
                  env_func = my_env,
                  sec_models = secondary_models,
                  ...
    ) %>%
        as.data.frame() %>%
        as_tibble() %>%
        mutate(logN = log10(.data$N))

    ## Calculate the gammas

    gammas <- lapply(times, function(x) {

        calculate_gammas(x, my_env, secondary_models)

    })

    gammas <- do.call(rbind.data.frame, gammas)
    names(gammas) <- names(secondary_models)
    gammas <- bind_cols(time = times, gammas)


    ## Prepare the output

    out <- list(simulation = my_sim,
                gammas = as_tibble(gammas),
                env_conditions = my_env,
                primary_pars = primary_pars,
                sec_models = secondary_models)

    class(out) <- c("DynamicGrowth", class(out))

    out

}



