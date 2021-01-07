
#' Metainformation of primary growth models
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
primary_model_data <- function(model_name=NULL) {

    model_data <- list(modGompertz = list(identifier = "modGompertz",
                                          name = "modified Gompertz model",
                                          pars = c("logN0", "C", "mu", "lambda"),
                                          model = iso_repGompertz,
                                          ref = paste("Zwietering, M. H., Jongenburger, I., Rombouts,",
                                                      "F. M., and Riet, K. van t. (1990). Modeling of the Bacterial",
                                                      "Growth Curve. Applied and Environmental Microbiology, 56(6), 1875-1881.")
                                          ),
                       Baranyi = list(identifier = "Baranyi",
                                      name = "Isothermal Baranyi model",
                                      pars = c("logN0", "logNmax", "mu", "lambda"),
                                      model = iso_Baranyi,
                                      ref = paste("Baranyi, J., and Roberts, T. A. (1994). A dynamic approach",
                                      "to predicting bacterial growth in food. International Journal of Food",
                                      "Microbiology, 23(3-4), 277-294. https://doi.org/10.1016/0168-1605(94)90157-0")
                                      ),
                       Trilinear = list(identifier = "Trilinear",
                                        name = "Tri-linear growth model",
                                        pars = c("logN0", "logNmax", "mu", "lambda"),
                                        model = trilinear_model,
                                        ref = paste("Buchanan, R. L., Whiting, R. C., and Damert, W. C. (1997).",
                                                    "When is simple good enough: A comparison of the Gompertz,",
                                                    "Baranyi, and three-phase linear models for fitting bacterial",
                                                    "growth curves. Food Microbiology, 14(4), 313-326.",
                                                    "https://doi.org/10.1006/fmic.1997.0125")
                                        ),
                       Logistic = list(identifier = "Logistic",
                                       name = "Logistic growth model",
                                       pars = c("logN0", "C", "mu", "lambda"),
                                       model = logistic_model,
                                       ref = ""
                                       ),
                       Richards = list(identifier = "Richards",
                                       name = "Richards growth model",
                                       pars = c("logN0", "C", "mu", "lambda", "nu"),
                                       model = richards_model,
                                       ref = ""
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


#' Basic check of parameters for primary models
#'
#' Checks that: the model name is correct, the right number of model
#' parameters have been defined and that the parameters have the right names
#'
#' @param model_name Model identifier
#' @param pars A named list of model parameters
#'
#' @return If there is no error, the model function.
#'
check_primary_pars <- function(model_name, pars) {

    ## (Indirectly) check that model name is correct

    my_data <- primary_model_data(model_name)
    model_pars <- my_data$pars

    ## Check the number of parameters

    if (length(model_pars) != length(pars)) {

        warning(paste0("The length of the parameters (", length(pars),
                    ") does not match the one of the model (", length(model_pars),
                    ").")
                )
    }

    ## Check parameter names

    for(each_name in names(pars)) {

        if (!(each_name %in% model_pars)) {
            warning(paste("Not recognized parameter name:", each_name))
        }

    }

    ## Return

    my_data$model

}






















