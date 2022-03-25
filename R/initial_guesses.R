
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
#' 
make_guess_primary <- function(fit_data, primary_model,
                               logbase = c("log10", "natural")  # TODO
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
make_guess_factor <- function(fit_data, sec_model, factor,
                                 logbase = c("log10", "natural")  # TODO
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
make_guess_secondary <- function(fit_data, sec_model_names,
                                 base = c("log10", "natural")  # TODO
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
#' 
#' @return A [ggplot()] comparing the model prediction against the data
#' 
#' @export
#' 
#' @examples 
#' 
#' ## We need some data
#' 
#' my_data <- data.frame(time = 0:9,
#'                       logN = c(2, 2.1, 1.8, 2.5, 3.1, 3.4, 4, 4.5, 4.8, 4.7)
#'                       )
#'                       
#' ## We can directly plot the comparison
#' 
#' show_guess_primary(my_data, "modGompertz",
#'                    c(logN0 = 1.5, mu = .8, lambda = 4, C = 3)
#'                    )
#'                    
#' ## It can be combined with the automatic initial guess
#' 
#' show_guess_primary(my_data, "modGompertz",
#'                    make_guess_primary(my_data, "modGompertz")
#'                    )
#' 
show_guess_primary <- function(fit_data, model, guess, 
                               logbase = c("natural", "10"),  # TODO
                               formula = logN ~ time
                               ) {
    
    ## Build the time vector
    
    x_col <- rhs(formula)
    y_col <- lhs(formula)
    
    my_time <- seq(0, max(fit_data[[x_col]], na.rm = TRUE), length = 1000)
    
    ## Build the model
    
    my_model <- as.list(guess)
    my_model$model <- model
    
    ## Make the prediction
    
    my_prediction <- predict_growth(my_time, my_model, environment = "constant",
                                    formula = formula)
    
    ## Plot the prediction and add the data points
    
    plot(my_prediction) +
        geom_point(aes(x = .data[[x_col]],
                       y = .data[[y_col]]),
                   data = fit_data,
                   inherit.aes = FALSE
                   )
}

#' AA
#' 
#' @export
#' 
show_guess_dynamic <- function(fit_data, model_keys, guess,
                               env_conditions,
                               logbase = c("natural", "10"),  # TODO
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
                                         formula = formula
                                         )
    
    ## Make the plot
    
    plot(dynamic_prediction) +
        geom_point(aes(x = .data[[x_col]],
                       y = .data[[y_col]]),
                   data = fit_data,
                   inherit.aes = FALSE
        )
}











