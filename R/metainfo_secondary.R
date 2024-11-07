
#' Metainformation of secondary growth models
#' 
#' @description 
#' `r lifecycle::badge("stable")`
#' 
#' Provides different types of meta-data about the secondary growth models included
#' in biogrowth. This information is the basis of the automatic checks, and can also
#' help in the definition of models for [predict_growth()] and [fit_growth()].
#'
#' @param model_name The name of the model or `NULL` (default).
#'
#' @return
#' If model_name is `NULL`, returns a character string with the available models.
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
                                            ),
                       Aryani = list(identifier = "Aryani",
                                     name = "Aryani pH model",
                                     pars = c("xmin", "xhalf"),
                                     model = Aryani_model,
                                     ref = paste("Aryani, D.C., den Besten, H.M.W., Zwietering, M.H.,", 
                                                 "2016. Quantifying Variability in Growth and Thermal Inactivation", 
                                                 "Kinetics of Lactobacillus plantarum. Appl. Environ. Microbiol. 82,", 
                                                 "4896–4908. https://doi.org/10.1128/AEM.00277-16"
                                                 )
                                     ),
                       Rosso_aw = list(identifier = "Rosso_aw",
                            name = "Rosso aw model",
                            pars = c("xmin"),
                            model = Rossoaw_model,
                            ref = paste("Rosso, L., Robinson, T.P., 2001.",
                            "A cardinal model to describe the effect of water activity",
                            "on the growth of moulds. International Journal of Food Microbiology",
                            "63, 265–273."
                            )
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
#' Checks that the model names are correct, that no parameter is defined twice,
#' that every parameter is defined and that no unknown parameter has been defined.
#' Raises an error if any of these conditions is not met.
#'
#' @inheritParams fit_secondary_growth
#' @param primary_pars Character vector with the parameter names of the primary model.
#'
#' @return NULL
#'
check_secondary_pars <- function(starting_point, known_pars, sec_model_names,
                                 primary_pars = "mu_opt") {
    
    
    ## Check no parameter is defined twice
    
    if (any(names(starting_point) %in% names(known_pars))) {
        stop("Parameters cannot be defined as both fixed and to be fitted.")
    }
    
    par_names <- c(names(starting_point), names(known_pars))
    
    ## Check the primary_pars are defined
    
    missing_primary <- primary_pars[!primary_pars %in% par_names]
    
    if (length(missing_primary) > 0) {
        stop(paste("Parameter not defined:", missing_primary, "\n"))
        
    }
    
    ## Get rid of primary parameters
    
    my_regex <- paste(primary_pars, collapse="|")
    par_names <- par_names[!grepl(my_regex, par_names)]
    
    # if (! "mu_opt" %in% par_names) {
    #     stop("Parameter not defined: mu_opt")
    # }
    # 
    # par_names <- par_names[!grepl("mu_opt", par_names)]  # We do not do further checks with it
    
    ## Checks for secondary model
    
    for (each_factor in names(sec_model_names)) {
        
        ## (Indirectly) check the model exists
        
        model_data <- secondary_model_data(sec_model_names[[each_factor]])
        
        req_pars <- paste0(each_factor, "_", model_data$pars)
        
        ## Check every required parameter is defined
        
        missing_pars <- req_pars[!req_pars %in% par_names]
        
        if (length(missing_pars) > 0) {
            stop(paste("Parameter not defined:", missing_pars, "\n"))
        }
        
        ## Check there is no unrecognized parameter
        
        this_pars <- par_names[grepl(paste0(each_factor, "_"), par_names)]
        
        unknown_pars <- this_pars[!this_pars %in% req_pars]
        
        if (length(unknown_pars) > 0) {
            stop(paste("Unknown parameter: ", unknown_pars, "\n"))
        }
        
    }
    
    ## Check there is no parameter left
    
    my_regex <- paste(paste0(names(sec_model_names), "_"), collapse="|")
    wtpars <- par_names[!grepl(my_regex, par_names)]
    
    if (length(wtpars) > 0) {
        stop(paste("Unknown parameter: ", wtpars, "\n"))
    }
    

}

















