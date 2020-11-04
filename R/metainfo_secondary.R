
#' Metainformation of secondary growth models
#'
#' @param model_name The name of the model or \code{NULL} (default).
#'
#' @return
#' If model_name is \code{NULL}, returns a character string with the available models.
#' If is a valid identifier, it returns a list with metainformation about the model.
#' If model_name name is not a valid identifier, raises an error.
#'
#' @export
#'
secondary_model_data <- function(model_name=NULL) {


    model_data <- list(CPM = list(identifier = "CPM",
                                  name = "Cardinal Parameter Model",
                                  pars = c("xmin", "xopt", "xmax", "n"),
                                  model = CPM_model,
                                  ref = paste("Rosso, L., Lobry, J. R., Bajard, S., and Flandrois, J. P. (1995).",
                                  "Convenient Model To Describe the Combined Effects of Temperature and pH on",
                                  "Microbial Growth. Applied and Environmental Microbiology, 61(2), 610-616.")
                                  ),
                       Zwietering = list(identifier = "Zwietering",
                                         name = "Zwietering gamma function",
                                         pars = c("xmin", "xopt", "n"),
                                         model = zwietering_gamma,
                                         ref = paste("Zwietering, Marcel H., Wijtzes, T., De Wit, J. C., and Riet,",
                                         "K. V. (1992). A Decision Support System for Prediction of the Microbial",
                                         "Spoilage in Foods. Journal of Food Protection, 55(12), 973-979.",
                                         "https://doi.org/10.4315/0362-028X-55.12.973")
                                         ),
                       fullRatkowsky = list(identifier = "fullRatkowsky",
                                            name = "(Adapted) Full Ratkowsky model",
                                            pars = c("xmin", "xmax", "c"),
                                            model = full_Ratkowski,
                                            ref = paste("Ratkowsky, D. A., Lowry, R. K., McMeekin, T. A.,",
                                                        "Stokes, A. N., and Chandler, R. E. (1983). Model for",
                                                        "bacterial culture growth rate throughout the entire",
                                                        "biokinetic temperature range. Journal of Bacteriology,",
                                                        "154(3), 1222-1226.")
                                            )
                       )

    if (is.null(model_name)) {
        return(names(model_data))
    }

    my_model <- model_data[[model_name]]

    if (is.null(my_model)) {
        stop(paste("Unknown model name:", model_name))
    } else {
        my_model
    }

}

#' Basic checks of secondary parameters
#'
#' Checks that the model name is correct, that the number of model parameters is correct
#' and that every parameter is defined.
#'
#' @param secondary_models A list of secondary models returned by extract_secondary_pars
#'
#' @return NULL
#'
check_secondary_pars <- function(secondary_models) {

    for (each_model in secondary_models) {

        my_data <- secondary_model_data(each_model$model)

        ## Check that model name is correct

        if (is.null(my_data)) {
            stop("Unknown model: ", each_model$model)
        }

        each_model$model <- NULL

        ## Number of parameters

        if (length(each_model) != length(my_data$pars)) {
            warning("The number of model parameters (", length(each_model),
                    ") does not match the one in the model (", length(my_data$pars), ")")
        }

        ## Parameter names

        for (each_par in my_data$pars) {
            if (!(each_par %in% names(each_model))) {
                warning(paste("Parameter not defined:", each_par))
            }
        }

    }

    NULL

}

















