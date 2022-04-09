
#' Growth under dynamic conditions
#' 
#' @description 
#' `r lifecycle::badge("superseded")`
#' 
#' The function [predict_dynamic_growth()] has been superseded by the top-level
#' function [predict_growth()], which provides a unified approach for growth modelling.
#'
#' Regardless on that, it can still predict population growth under dynamic conditions based on the
#' Baranyi model (Baranyi and Roberts, 1994) and secondary models
#' based on the gamma concept (Zwietering et al. 1992).
#'
#' Model predictions are done by linear interpolation of the environmental
#' conditions defined in `env_conditions`.
#'
#' @param times Numeric vector of storage times to make the predictions
#' @param env_conditions Tibble (or data.frame) describing the variation of the environmental
#' conditions during storage. It must have with the elapsed time (named `time` 
#' by default; can be changed with the "formula" argument), 
#' and as many additional columns as environmental factors.
#' @param primary_pars A named list defining the parameters of the primary model
#' and the initial values of the model variables. That is, with names `mu_opt`,
#' `Nmax`, `N0`, `Q0`.
#' @param secondary_models A nested list describing the secondary models.
#' @param ... Additional arguments for [ode()].
#' @param check Whether to check the validity of the models. `TRUE` by default.
#' @param formula An object of class "formula" describing the x variable.
#' `. ~ time` as a default.
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, 10 (i.e. log10). See vignette about units for details. 
#'
#' @return An instance of [DynamicGrowth()].
#'
#' @importFrom deSolve ode
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_cols
#' @importFrom rlang .data
#' @importFrom purrr map
#' @importFrom formula.tools rhs
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
                                   secondary_models, 
                                   ...,
                                   check = TRUE,
                                   logbase_logN = 10,
                                   logbase_mu = logbase_logN,
                                   formula = . ~ time
                                   ) {
    
    ## Apply the formula
    
    x_col <- rhs(formula)
    
    env_conditions <- rename(env_conditions, time = x_col)
    
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
    
    ## Apply the logbase transformation to mu
    
    primary_pars_calc <- primary_pars
    
    primary_pars_calc$mu_opt <- primary_pars_calc$mu_opt/log(10, base = logbase_mu)

    ## Prepare stuff

    my_env <- approx_env(env_conditions)

    yini <- c(Q = primary_pars_calc$Q0,
              N = primary_pars_calc$N0
              )

    ## Make the simulation

    my_sim <- ode(yini, times, dBaranyi, primary_pars_calc,
                  env_func = my_env,
                  sec_models = secondary_models,
                  ...
    ) %>%
        as.data.frame() %>%
        as_tibble() %>%
        mutate(logN = log(.data$N, base = logbase_logN))

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
                sec_models = secondary_models,
                logbase_mu = logbase_mu,
                logbase_logN = logbase_logN)

    class(out) <- c("DynamicGrowth", class(out))

    out

}



