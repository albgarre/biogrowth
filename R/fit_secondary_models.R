
#' Gamma factors for fitting secondary models
#'
#' A helper for fitting the secondary gamma models.
#' Calculates the gamma factors corresponding to the models defined
#' and the experimental conditions. In order for it to work, the environmental
#' factors must be named identically in the 3 arguments.
#'
#' @param sec_model_names named character vector defining the type of secondary
#' model. Its names correspond to the environmental conditions and its values
#' define the corresponding type of secondary model.
#' @param my_data Tibble of experimental conditions.
#' @param secondary_models A list defining the parameters of the secondary models.
#'
#' @return a numeric vector of length `nrow(my_data)` with the gamma factor
#' for each experimental condition.
#'
#' @importFrom dplyr bind_cols
#'
calculate_gammas_secondary <- function(sec_model_names, my_data, secondary_models) {

    all_gammas <- lapply(names(sec_model_names), function(this_condition) {

        this_x <- my_data[[this_condition]]
        this_sec <- secondary_models[[this_condition]]

        this_gamma <- switch(this_sec$mode,
                             CPM = CPM_model(this_x, this_sec$xmin,
                                             this_sec$xopt, this_sec$xmax, this_sec$n),
                             Zwietering = zwietering_gamma(this_x, this_sec$xmin, this_sec$xopt, this_sec$n),
                             fullRatkowsky = full_Ratkowski(this_x, this_sec$xmin, this_sec$xmax, this_sec$c),
                             Aryani = Aryani_model(this_x, this_sec$xmin, this_sec$xhalf),
                             Rosso_aw = Rossoaw_model(this_x, this_sec$xmin),
                             Inhibitory = inhibitory_model(this_x, this_sec$MIC, this_sec$alpha),
                             stop(paste("Model", this_sec$model, "not known."))
        )
        
        this_gamma <- tibble(this_gamma)
        names(this_gamma) <- this_condition
        this_gamma

    }) %>%
        bind_cols()

    # print(all_gammas)

    apply(all_gammas, 1, prod)

}

#' Residuals of secondary models
#'
#' Residual function for [fit_secondary_growth()].
#'
#' @param this_p Named vector of model parameter values.
#' @param my_data Tibble with the data used for the fit.
#' @param known_pars Named vector of fixed model paramaters.
#' @param sec_model_names Named character vector defining the secondary model
#' for each environmental factor.
#' @param transformation Character defining the tranformation of `mu` for
#' model fitting. One of `sq` (square root), `log` (log-transform) or
#' `none` (no transformation).
#'
#' @return A numeric vector of residuals.
#'
#' @importFrom dplyr mutate
#'
get_secondary_residuals <- function(this_p, my_data,
                                    known_pars, sec_model_names,
                                    transformation) {

    ## Build secondary model

    secondary_models <- extract_secondary_pars(this_p, known_pars,
                                               sec_model_names)

    if ("mu_opt" %in% names(known_pars)) {
        mu_opt <- as.list(known_pars)$mu_opt
    } else {
        mu_opt <- as.list(this_p)$mu_opt
    }

    ## Calculate gammas

    all_gammas <- calculate_gammas_secondary(sec_model_names, my_data, secondary_models)

    ## Compare prediction and observations

    my_data <- my_data %>%
        mutate(gamma = all_gammas,
               pred_mu = gamma*mu_opt)

    if (transformation == "sq") {
        out_res <- sqrt(my_data$pred_mu) - my_data$sq_mu
    } else if (transformation == "log") {
        out_res <- log(my_data$pred_mu) - my_data$log_mu
    } else if (transformation == "none") {
        out_res <- my_data$pred_mu - my_data$mu
    } else {
        stop(paste("Unknown transformation:", transformation))
    }

    out_res

}

#' Fit secondary growth models
#'
#' @description 
#' `r lifecycle::badge("stable")`
#' 
#' Fits a secondary growth model to a set of growth rates obtained experimentally.
#' Modelling is done according to the gamma concept proposed by Zwietering (1992)
#' and cardinal parameter models.
#'
#' @param fit_data Tibble with the data used for the fit. It must have
#' one column with the observed growth rate (named `mu` by default; can be
#' changed using the "formula" argument) and as many columns
#' as needed with the environmental factors.
#' @param starting_point Named vector with initial values for the model parameters
#' to estimate from the data. The growth rate under optimum conditions must be named
#' `mu_opt`. The rest must be called 'env_factor'+'_'+'parameter'. For instance,
#' the minimum pH for growth is 'pH_xmin'.
#' @param known_pars Named vector of fixed model parameters. Must be named using the
#' same convention as `starting_point`.
#' @param sec_model_names Named character vector defining the secondary model
#' for each environmental factor.
#' @param transformation Character defining the transformation of `mu` for
#' model fitting. One of `sq` (square root; default), `log` (log-transform) or
#' `none` (no transformation).
#' @param ... Additional arguments passed to [modFit()].
#' @param check Whether to do some basic checks (TRUE by default).
#' @param formula an object of class "formula" describing the y variable. The
#' right hand side must be ".". By default `mu ~ .`.
#'
#' @return An instance of [FitSecondaryGrowth()].
#'
#' @importFrom dplyr mutate rename
#' @importFrom FME modFit
#' @importFrom rlang .data
#' @importFrom formula.tools lhs
#'
#' @export
#'
#' @examples
#' ## We use the data included in the package
#'
#' data("example_cardinal")
#'
#' ## Define the models to fit
#'
#' sec_model_names <- c(temperature = "Zwietering", pH = "CPM")
#'
#' ## Any model parameter can be fixed
#'
#' known_pars <- list(mu_opt = 1.2, temperature_n = 1,
#'     pH_n = 2, pH_xmax = 6.8, pH_xmin = 5.2)
#'
#' ## Initial values must be given for every other parameter
#'
#' my_start <- list(temperature_xmin = 5, temperature_xopt = 35,
#'     pH_xopt = 6.5)
#'
#' ## We can now call the fitting function
#'
#' fit_cardinal <- fit_secondary_growth(example_cardinal, my_start, known_pars, sec_model_names)
#'
#' ## With summary, we can look at the parameter estimates
#'
#' summary(fit_cardinal)
#'
#' ## The plot function compares predictions against observations
#'
#' plot(fit_cardinal)
#'
#' ## Passing which = 2, generates a different kind of plot
#'
#' plot(fit_cardinal, which = 2)
#' plot(fit_cardinal, which = 2, add_trend = TRUE)
#' plot(fit_cardinal, which = 2, add_segment = TRUE)
#'
fit_secondary_growth <- function(fit_data, starting_point,
                                 known_pars, sec_model_names,
                                 transformation = "sq",
                                 ..., 
                                 check = TRUE,
                                 formula = mu ~ .) {

    ## Check model parameters

    if (isTRUE(check)) {

        check_secondary_pars(starting_point, known_pars, sec_model_names)

    }
    
    ## Apply the formula
    
    y_col <- lhs(formula)
    
    fit_data <- rename(fit_data, 
                       mu = y_col
                       )

    ## Model fitting

    fit_data <- fit_data %>%
        mutate(log_mu = log10(.data$mu),
               sq_mu = sqrt(.data$mu))

    my_fit <- modFit(get_secondary_residuals, unlist(starting_point),
                     my_data = fit_data, known_pars = known_pars,
                     sec_model_names = sec_model_names,
                     transformation = transformation,
                     ...
                     )

    ## Output


    secondary_models <- extract_secondary_pars(as.list(my_fit$par),
                                               known_pars,
                                               sec_model_names)

    if ("mu_opt" %in% names(known_pars)) {
        mu_opt <- known_pars$mu_opt
    } else {
        mu_opt <- as.list(my_fit$par)$mu_opt
    }

    out <- list(fit_results = my_fit,
                secondary_model = secondary_models,
                mu_opt_fit = mu_opt,
                data = fit_data,
                transformation = transformation
                )

    class(out) <- c("FitSecondaryGrowth", class(out))

    out

}








