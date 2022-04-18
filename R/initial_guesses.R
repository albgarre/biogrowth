
#' Initial guesses for fitting primary growth models
#' 
#' The function uses some heuristics to provide initial guesses for the parameters
#' of the growth model selected.
#' 
#' @param fit_data the experimental data. A tibble (or data.frame) with a column
#' named `time` with the elapsed time and one called `logN` with the logarithm
#' of the population size
#' @param primary_model a string defining the equation of the primary model, 
#' as defined in [primary_model_data()]
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, 10 (i.e. log10). See vignette about units for details. 
#' 
#' @return A named numeric vector of initial guesses for the model parameters
#' 
#' @importFrom tibble tribble
#' 
#' @export
#' 
#' @examples 
#' 
#' ## An example of experimental data
#' 
#' my_data <- data.frame(time = 0:9, 
#'                       logN = c(2, 2.1, 1.8, 2.5, 3.1, 3.4, 4, 4.5, 4.8, 4.7))
#'                       
#' ## We just need to pass the data and the model equation
#' 
#' make_guess_primary(my_data, "Logistic")
#' 
#' ## We can use this together with fit_growth()
#' 
#' fit_growth(my_data,
#'            list(primary = "Logistic"),
#'            make_guess_primary(my_data, "Logistic"),
#'            c()
#'            )
#' 
#' ## The parameters returned by the function are adapted to the model
#' 
#' make_guess_primary(my_data, "Baranyi")
#' 
#' ## It can express mu in other logbases 
#' 
#' make_guess_primary(my_data, "Baranyi", logbase_mu = exp(1))  # natural
#' make_guess_primary(my_data, "Baranyi", logbase_mu = 2)  # base2
#' 
make_guess_primary <- function(fit_data, primary_model,
                               logbase_mu = 10
                               ) {
    
    ## Check that we know the model
    
    if ( ! (primary_model %in% primary_model_data()) ) {
        stop("Unkonwn model: ", primary_model)
    }
    
    ## Guess for logN0
    
    index_t0 <- which(fit_data$time == min(fit_data$time, na.rm = TRUE))[1]
    logN0 <- fit_data$logN[index_t0]
    
    ## Guess for lambda
    
    lambda <- min(fit_data$time[which(fit_data$time > logN0 + .5)], na.rm = TRUE)
    
    ## Gues for logNmax
    
    logNmax <- max(fit_data$logN, na.rm = TRUE)
    
    ## Guess for mu
    
    tmax <- min(fit_data$time[which(fit_data$logN == logNmax)], na.rm = TRUE)
    
    mu <- (logNmax - logN0)/(tmax - lambda)
    
    mu <- mu*log(10, base = logbase_mu)  # convert to base 10
    
    ## Guess for C
    
    C <- logNmax - logN0
    
    ## Guess for nu
    
    nu <- 1
    
    ## return
    
    out <- list(logN0 = logN0, mu = mu, lambda = lambda, 
                logNmax = logNmax, C = C, nu = nu)
    
    par_map <- tribble(
        ~ par,  ~modGompertz, ~Baranyi, ~Trilinear, ~Logistic, ~Richards,
        "logN0", TRUE, TRUE, TRUE, TRUE, TRUE,
        "mu", TRUE, TRUE, TRUE, TRUE, TRUE,
        "lambda", TRUE, TRUE, TRUE, TRUE, TRUE,
        "logNmax", FALSE, TRUE, TRUE, FALSE, FALSE,
        "C", TRUE, FALSE, FALSE, TRUE, TRUE,
        "nu", FALSE, FALSE, FALSE, FALSE, TRUE
        )
    
    my_pars <- par_map$par[par_map[[primary_model]]]
    
    unlist(out[my_pars])
    
}


#' Initial guesses for the secondary model of one factor
#' 
#' @param fit_data Tibble with the data used for the fit. It must have
#' one column with the observed growth rate (named `mu` by default; can be
#' changed using the "formula" argument) and as many columns
#' as needed with the environmental factors.
#' @param sec_model character defining the secondary model equation according
#' to [secondary_model_data()]
#' @param factor character defining the environmental factor
#' 
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#' 
#' 
make_guess_factor <- function(fit_data, sec_model, factor
                                 ) {
    
    ## Check that we know the model
    
    if ( ! (sec_model %in% secondary_model_data()) ) {
        stop("Unkonwn model: ", sec_model)
    }

    
    ## Extract to make life easier
    
    mu <- fit_data$mu
    x <- fit_data[[factor]]
    
    if (is.null(x)) {
        stop(factor, "not in fit_data")
    }
    
    ## Guess for mu_opt
    
    mu_opt <- max(mu, na.rm = TRUE)
    
    ## guess for xopt
    
    xopt <- x[which(mu == mu_opt)]
    
    ## guess for xmin
    
    xmin <- min(x, na.rm = TRUE)
    
    ## guess for xmax
    
    xmax <- max(x, na.rm = TRUE)
    
    if (xmax == xopt) xmax <- xopt*1.2  # We don't want them to be equal
    
    ## guess for n
    
    n <- 2
    
    ## guess for c
    
    c <- (mu_opt - 0)/(xopt - xmin)
    
    ## Take the cardinal parameters
    
    out <- list(xopt = xopt, xmin = xmin, xmax = xmax, n = n, c = c)
    
    par_map <- tribble(
        ~ par,  ~CPM, ~Zwietering, ~fullRatkowsky,
        "xmin", TRUE, TRUE, TRUE,
        "xopt", TRUE, TRUE, FALSE,
        "xmax", TRUE, FALSE, TRUE,
        "n", TRUE, TRUE, FALSE,
        "c", FALSE, FALSE, TRUE
    )
    
    my_pars <- par_map$par[par_map[[sec_model]]]
    out <- out[my_pars]
    
    names(out) <- paste0(factor, "_", names(out))
    
    unlist(out)
}


#' Initial guesses for the parameters of a secondary model
#' 
#' @inheritParams fit_secondary_growth
#' 
#' @importFrom purrr imap flatten_dbl
#'
#' @export
#' 
#' @examples 
#' 
#' ## We can use the example dataset included in the package
#' 
#' data("example_cardinal")
#' 
#' ## We assign model equations to factors as usual
#' 
#' sec_model_names <- c(temperature = "Zwietering", pH = "fullRatkowsky")
#' 
#' ## We can then calculate the initial guesses
#' 
#' make_guess_secondary(example_cardinal, sec_model_names)
#' 
#' ## We can pass these parameters directly to fit_secondary_growth
#' 
#' fit_secondary_growth(example_cardinal, 
#'                      make_guess_secondary(example_cardinal, sec_model_names), 
#'                      c(), 
#'                      sec_model_names)
#' 
make_guess_secondary <- function(fit_data, sec_model_names
                                 ) {
    
    ## Guess for each factor
    
    out <- sec_model_names %>%
        imap(~ make_guess_factor(fit_data, .x, .y)
             ) %>%
        flatten_dbl()
    
    ## Add the guess for mu_opt
    
    mu_opt <- max(fit_data$mu, na.rm = TRUE)
    
    out <- c(mu_opt = mu_opt, out)
    
    ## Return
    
    out
    
}

#' Plot of the initial guess for growth under constant environmental conditions
#' 
#' Compares the prediction corresponding to a guess of the parameters of the primary
#' model against experimental data
#' 
#' @param fit_data Tibble (or data.frame) of data for the fit. It must have two columns, one with
#' the elapsed time (`time` by default) and another one with the decimal logarithm
#' of the populatoin size (`logN` by default). Different column names can be
#' defined using the `formula` argument. 
#' @param model_name Character defining the primary growth model as per [primary_model_data()] 
#' @param guess Named vector with the initial guess of the model parameters
#' @param formula an object of class "formula" describing the x and y variables.
#' `logN ~ time` as a default.
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, 10 (i.e. log10). See vignette about units for details. 
#' 
#' @return A [ggplot()] comparing the model prediction against the data
#' 
#' @examples 
#' ## An example of experimental data
#' 
#' my_data <- data.frame(time = 0:9, 
#'                       logN = c(2, 2.1, 1.8, 2.5, 3.1, 3.4, 4, 4.5, 4.8, 4.7))
#'                       
#' ## We just need to pass the data and the model parameters
#' 
#' show_guess_primary(my_data,
#'                    "Baranyi",
#'                    c(logN0 = 2, mu = .5, logNmax = 6, lambda = 4)
#'                    )
#'                    
#' ## It can be combined with make_guess_primary
#' 
#' show_guess_primary(my_data,
#'                    "Logistic",
#'                    make_guess_primary(my_data, "Logistic")
#'                    )
#'                    
#' ## It accepts different logbases for mu
#' 
#' show_guess_primary(my_data,
#'                    "Baranyi",
#'                    c(logN0 = 2, mu = .5, logNmax = 6, lambda = 4),
#'                    logbase_mu = exp(1)
#'                    )
#'                    
#' ## When combining it with make_guess_primary, carefull to use the same logbase 
#' 
#' show_guess_primary(my_data,
#'                    "Logistic",
#'                    make_guess_primary(my_data, "Logistic"),
#'                    logbase_mu = exp(1)  # Different base
#'                    )
#'                    
#' show_guess_primary(my_data,
#'                    "Logistic",
#'                    make_guess_primary(my_data, "Logistic", 
#'                                       logbase_mu = exp(1)
#'                                       ),  # same base
#'                    logbase_mu = exp(1) 
#'                    )
#' 
#' 
show_guess_primary <- function(fit_data, model_name, guess, 
                               logbase_mu = 10,
                               formula = logN ~ time
                               ) {
    
    ## Build the time vector
    
    x_col <- rhs(formula)
    y_col <- lhs(formula)
    
    my_time <- seq(0, max(fit_data[[x_col]], na.rm = TRUE), length = 1000)
    
    ## Build the model
    
    my_model <- as.list(guess)
    my_model$model <- model_name
    
    ## Make the prediction
    
    my_prediction <- predict_growth(my_time, my_model, environment = "constant",
                                    formula = formula,
                                    logbase_mu = logbase_mu)
    
    ## Plot the prediction and add the data points
    
    plot(my_prediction) +
        geom_point(aes(x = .data[[x_col]],
                       y = .data[[y_col]]),
                   data = fit_data,
                   inherit.aes = FALSE
                   )
}

#' Plot of the initial guess for growth under dynamic environmental conditions
#' 
#' Compares the prediction corresponding to a guess of the parameters of the 
#' model against experimental data
#' 
#' @param fit_data Tibble (or data.frame) of data for the fit. It must have two columns, one with
#' the elapsed time (`time` by default) and another one with the decimal logarithm
#' of the populatoin size (`logN` by default). Different column names can be
#' defined using the `formula` argument. 
#' @param model_keys Named the equations of the secondary model as in [fit_growth()]
#' @param guess Named vector with the initial guess of the model parameters as in [fit_growth()]
#' @param formula an object of class "formula" describing the x and y variables.
#' `logN ~ time` as a default.
#' @param env_conditions Tibble describing the variation of the environmental
#' conditions for dynamic experiments. See [fit_growth()].
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, 10 (i.e. log10). See vignette about units for details. 
#' 
#' @return A [ggplot()] comparing the model prediction against the data
#' 
#' 
show_guess_dynamic <- function(fit_data, model_keys, guess,
                               env_conditions,
                               logbase_mu = 10,
                               formula = logN ~ time
                               ) {

    ## Build the parameters of the primary model
    
    my_primary <- extract_primary_pars(guess, c())
    
    ## Build the parameters of the secondary model
    
    my_secondary <- extract_secondary_pars(guess, c(), unlist(model_keys))
    
    ## Build the time vector
    
    x_col <- rhs(formula)
    y_col <- lhs(formula)
    
    my_times <- seq(0, max(fit_data[[x_col]], na.rm = TRUE), length = 1000)
    
    ## Make the prediction

    dynamic_prediction <- predict_growth(environment = "dynamic",
                                         my_times, my_primary, my_secondary,
                                         env_conditions,
                                         formula = formula,
                                         logbase_mu = logbase_mu
                                         )
    
    ## Make the plot
    
    plot(dynamic_prediction) +
        geom_point(aes(x = .data[[x_col]],
                       y = .data[[y_col]]),
                   data = fit_data,
                   inherit.aes = FALSE
        )
}

#' Compares an initial gues of the model parameters
#' 
#' Generates a plot comparing a set of data points against the model prediction
#' corresponding to an initial guess of the model parameters
#' 
#' @param fit_data Tibble (or data.frame) of data for the fit. It must have two columns, one with
#' the elapsed time (`time` by default) and another one with the decimal logarithm
#' of the populatoin size (`logN` by default). Different column names can be
#' defined using the `formula` argument. 
#' @param model_keys Named the equations of the secondary model as in [fit_growth()]
#' @param guess Named vector with the initial guess of the model parameters as in [fit_growth()]
#' @param formula an object of class "formula" describing the x and y variables.
#' `logN ~ time` as a default.
#' @param env_conditions Tibble describing the variation of the environmental
#' conditions for dynamic experiments. See [fit_growth()]. Ignored when `environment = "constant"`
#' @param environment type of environment. Either "constant" (default) or "dynamic" (see below for details 
#' on the calculations for each condition)
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, 10 (i.e. log10). See vignette about units for details. 
#' 
#' @importFrom purrr map2
#' @importFrom cowplot plot_grid
#' 
#' @return A [ggplot()] comparing the model prediction against the data
#' 
#' @export
#' 
#' @examples 
#' 
#' ## Examples under constant environmental conditions -------------------------
#' 
#' ## We need some data
#' 
#' my_data <- data.frame(time = 0:9,
#'                       logN = c(2, 2.1, 1.8, 2.5, 3.1, 3.4, 4, 4.5, 4.8, 4.7)
#'                       )
#'                       
#' ## We can directly plot the comparison for some values
#' 
#' check_growth_guess(my_data, list(primary = "modGompertz"),
#'                    c(logN0 = 1.5, mu = .8, lambda = 4, C = 3)
#'                    )
#'                    
#' ## Ot it can be combined with the automatic initial guess
#' 
#' check_growth_guess(my_data, list(primary = "modGompertz"),
#'                    make_guess_primary(my_data, "modGompertz")
#'                    )
#'                    
#' ## Examples under dynamic environmental conditions --------------------------
#' 
#' ## We will use the datasets included in the package
#' 
#' data("example_dynamic_growth")
#' data("example_env_conditions")
#' 
#' ## Model equations are assigned as in fit_growth
#' 
#' sec_models <- list(temperature = "CPM", aw = "CPM")
#' 
#' ## Guesses of model parameters are also defined as in fit_growth
#' 
#' guess <- list(Nmax = 1e4, 
#'               N0 = 1e0, Q0 = 1e-3,
#'               mu_opt = 4, 
#'               temperature_n = 1,
#'               aw_xmax = 1, aw_xmin = .9, aw_n = 1,
#'               temperature_xmin = 25, temperature_xopt = 35,
#'               temperature_xmax = 40, aw_xopt = .95
#'               )
#'               
#' ## We can now check our initial guess
#' 
#' check_growth_guess(example_dynamic_growth, sec_models, guess,
#'                    "dynamic",
#'                    example_env_conditions)
#' 
check_growth_guess <- function(fit_data, model_keys, guess,
                              environment = "constant",
                              env_conditions = NULL,
                              approach = "single",
                              logbase_mu = 10,
                              formula = logN ~ time) {
    
    if (environment == "constant") {
        
        if (!is.null(env_conditions)) {
            warning("env_conditions is ignored for constant environment")
        }
        
        show_guess_primary(fit_data, model_keys$primary, guess, 
                           logbase_mu = logbase_mu, 
                           formula = formula
        ) 
        
    } else if (environment == "dynamic") {
        
        if (approach == "single") {
            
            show_guess_dynamic(fit_data, model_keys, guess,
                               env_conditions,
                               logbase_mu = logbase_mu,
                               formula = formula
            )
            
        } else if (approach == "global") {

            # browser()
            
            out <- map2(fit_data, env_conditions,
                 ~ show_guess_dynamic(.x, 
                                      model_keys, 
                                      guess,
                                      .y,
                                      logbase_mu = logbase_mu,
                                      formula = formula)
                 )
            
            plot_grid(plotlist = out, labels = names(fit_data))
            
        } else {
            stop("approach must be 'single' or 'global', got ", approach)
        }
        
    } else {
        
        stop("environment must be 'constant' or 'dynamic', not ", environment)
        
    }
    
}










