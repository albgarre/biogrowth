


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
                       algorithm = "regression",
                       approach = "single",
                       env_conditions = NULL,
                       niter = NULL,
                       ..., 
                       check = TRUE,
                       logbase = c("natural", "10"),  # TODO
                       formula = logN ~ time
                       ) {
    
    
    ## This is just a top-level function. All the heavy lifting is still done in the superseded functions
    
    # browser()
    
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
        
        if (check) {
            
            if(!is.null(niter)) {
                warning("argument niter is ignored for fitting under constant conditions")
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
        
        ## Check the primary model has not been defined
        
        if (!is.null(model_keys$primary)) {
            warning("model_keys$primary is ignored for dynamic fits")
            model_keys$primary <- NULL
        }

        if (algorithm == "regression" & approach == "single") {  # Dynamic fitting by regression
            
            ## Check whether niter was defined 
            
            if (check) {
                
                if(!is.null(niter)) {
                    warning("argument niter is ignored for fitting under constant conditions")
                }
                
            }
            
            ## Fit the model

            fit_dynamic_growth(fit_data, env_conditions,
                               start, known,
                               unlist(model_keys),
                               ...,
                               check = check,
                               formula = formula)
            
        } else if(algorithm == "MCMC" & approach == "single")  {  # Dynamic fitting by MCMC
            
            ## Check whether niter was defined 
            
            if (check) {
                
                if(!is.null(niter)) {
                    warning("argument niter is ignored for fitting under constant conditions")
                }
                
            }
            
            ## Fit the model
            
            fit_MCMC_growth(fit_data, env_conditions,
                            start, known,
                            unlist(model_keys),
                            niter,
                            ...,
                            check = check,
                            formula = formula)
            
        } else if(algorithm == "regression" & approach == "global")  {  # Global fitting by regression

            ## Check whether niter was defined 
            
            if (check) {
                
                if(!is.null(niter)) {
                    warning("argument niter is ignored for fitting under constant conditions")
                }
                
            }
            
            ## Put the data together
            
            my_data <- names(fit_data) %>%
                map(~ list(
                        data = fit_data[[.]],
                        conditions = env_conditions[[.]]
                    )
                )
            
            names(my_data) <- names(fit_data)
            
            ## Fit the model
            
            fit_multiple_growth(
                start,
                my_data,
                known,
                unlist(model_keys),
                ...,
                check = check,
                formula = formula
            )
            
        } else if(algorithm == "MCMC" & approach == "global")  {  # Global fitting by MCMC
            
            ## Put the data together
            
            my_data <- names(fit_data) %>%
                map(~ list(
                        data = fit_data[[.]],
                        conditions = env_conditions[[.]]
                    )
                )
            
            names(my_data) <- names(fit_data)
            
            ## Fit the model
            
            fit_multiple_growth_MCMC(
                start,
                my_data,
                known,
                unlist(model_keys),
                niter,
                ...,
                check = check,
                formula = formula
                )
            
        } else {
            
            if (! (algorithm %in% c("MCMC", "regression")) ) {
                stop("algorithm must be 'MCMC' or 'regression', got ", algorithm)
            }
            
            if (! (approach %in% c("single", "global")) ) {
                stop("approach must be 'single' or 'global', got ", approach)
            }
            
        }
        
        
    } else {
        
        
        stop("environment must be 'constant' or 'dynamic', received: ", environment)
        
    }
    
    
}