
#' Gamma factors for fitting secondary models
#'
#' @importFrom dplyr bind_cols
#'
calculate_gammas_secondary <- function(sec_model_names, my_data, secondary_models) {

    lapply(names(sec_model_names), function(this_condition) {

        this_x <- my_data[[this_condition]]
        this_sec <- secondary_models[[this_condition]]

        this_gamma <- switch(this_sec$mode,
                             # Ratkowsky = ratkowsky_model(this_x, this_sec$b, this_sec$xmin),
                             CPM = CPM_model(this_x, this_sec$xmin,
                                             this_sec$xopt, this_sec$xmax, this_sec$n),
                             Zwietering = zwietering_gamma(this_x, this_sec$xmin, this_sec$xopt, this_sec$n),
                             stop(paste("Model", this_sec$model, "not known."))
        )

        this_gamma

    }) %>%
        bind_cols() %>%
        apply(., 1, prod)

}

#' Residuals of secondary models
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
        mu_opt <- known_pars$mu_opt
    } else {
        mu_opt <- this_p$mu_opt
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
#' @importFrom dplyr mutate
#' @importFrom FME modFit
#'
#' @export
#'
fit_secondary_growth <- function(fit_data, starting_point,
                                 known_pars, sec_model_names,
                                 transformation = "sq",
                                 ...) {

    ## Model fitting

    fit_data <- fit_data %>%
        mutate(log_mu = log10(mu),
               sq_mu = sqrt(mu))

    my_fit <- modFit(get_secondary_residuals, unlist(starting_point),
                     my_data = fit_data, known_pars = known_pars,
                     sec_model_names = sec_model_names,
                     transformation = transformation
                     )

    ## Output


    secondary_models <- extract_secondary_pars(my_fit$par, known_pars,
                                               sec_model_names)

    if ("mu_opt" %in% names(known_pars)) {
        mu_opt <- known_pars$mu_opt
    } else {
        mu_opt <- my_fit$par$mu_opt
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








