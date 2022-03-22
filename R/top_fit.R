


#' Fitting microbial growth
#' 
#' @description 
#' `r lifecycle::badge("stable")`
#' 
#' @export
#'
fit_growth <- function(fit_data,
                       model_keys,
                       start,
                       known,
                       environment = "constant",
                       algorithm = c("regression", "MCMC"),
                       approach = c("single", "multiple"),
                       env_conditions = NULL,
                       ..., 
                       check = TRUE,
                       logbase = c("natural", "10"),  # TODO
                       formula = logN ~ time
                       ) {
    
    
    ## This is just a top-level function. All the heavy lifting is still done in the superseded functions
    
    if (environment == "constant") {  # Fitting just a primary model
        
        ## Check the algorithm
        
        if (algorithm != "regression") {
            stop("Only regression is supported for constant environmental conditions, not ", algorithm)
        }
        
        ## Check the approach
        
        if (approach != "single") {
            stop("Only one experiment can be fitted using constant environmental conditions, not ", approach)
        }
        
        ## Check arguments that should be NULL
        
        if (check) {
            
            if(!is.null(env_conditions)) {
                warning("argument env_conditions is ignored for fitting under constant conditions")
            }
            
        }
        
        ## Extract the primary model
        
        my_model <- model_keys$primary
        
        ## Fit the model

        fit_isothermal_growth(fit_data, my_model, start, known,
                              ..., 
                              check = check,
                              formula = formula
                              )
        
    } else if (environment == "dynamic") {  # Fitting both primary and secondary models
        
    } else {
        
        
        stop("environment must be 'constant' or 'dynamic', received: ", environment)
        
    }
    
    
}