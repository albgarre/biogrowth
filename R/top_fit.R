


#' Fitting microbial growth
#' 
#' @description 
#' `r lifecycle::badge("stable")`
#' 
#' This function provides a top-level interface for fitting growth models to data describing
#' the variation of the population size through time, either under constant or dynamic
#' environment conditions. 
#' See below for details on the calculations.
#' 
#' @details 
#' Models can be fitted using two different algorithms: regression (Levenberg-Marquardt
#' algorithm) or an Adaptive Monte Carlo algorithm.
#' 
#' AA
#' 
#' @section Fitting under constant conditions:
#' AA
#' 
#' @section Fitting under dynamic conditions to a single experiment:
#' AA
#' 
#' @section Fitting undery dynamic conditions to multiple experiments (global fitting):
#' AA
#' 
#' @return Depending on the type of data, the function will return an instance 
#' of a different object:
#' * for environment="constant", an instance of [FitIsoGrowth],
#' * for environment="dynamic", approach="single" and algorithm="regression",
#' an instance of [FitDynamicGrowth],
#' * for environment="dynamic", approach="single" and algorithm="MCMC",
#' an instance of [FitDynamicGrowthMCMC],
#' * for environment="dynamic", approach="global" and algorithm="regression",
#' an instance of [FitMultipleDynamicGrowth],
#' * for environment="dynamic", approach="global" and algorithm="MCMC",
#' an instance of [FitMultipleDynamicGrowthMCMC].
#' 
#' Please check the help pages of each class for additional information.
#' 
#' @export
#' 
#' @examples 
#' 
#' ## Example 1 - Fitting a primary model --------------------------------------
#' 
#' ## A dummy dataset describing the variation of the population size 
#' 
#' my_data <- data.frame(time = c(0, 25, 50, 75, 100), 
#'                       logN = c(2, 2.5, 7, 8, 8))
#'                       
#' ## The primary model is defined as a list
#' 
#' models <- list(primary = "Baranyi")
#' 
#' ## Any model parameter can be fixed
#' known <- c(mu = .2)
#' 
#' ## The remaning parameters need initial guesses 
#' 
#' start <- c(logNmax = 8, lambda = 25, logN0 = 2)
#' 
#' primary_fit <- fit_growth(my_data, models, start, known,
#'                           environment = "constant",
#'                           )
#'                           
#' ## The instance of FitIsoGrowth includes several useful methods
#' 
#' print(primary_fit)
#' plot(primary_fit)
#' coef(primary_fit)
#' summary(primary_fit)
#' 
#' ## Example 2 - Fitting under dynamic conditions------------------------------
#' 
#' ## We will use the example data included in the package
#' 
#' data("example_dynamic_growth")
#' 
#' ## And the example environmental conditoins (temperature & aw)
#' 
#' data("example_env_conditions")
#' 
#' ## We need to assign a model equation (secondary model) to each environmental factor
#' 
#' sec_models <- list(temperature = "CPM", aw = "CPM")
#' 
#' ## Any model parameter (of the primary or secondary models) can be fixed
#' 
#' known_pars <- list(Nmax = 1e4,  # Primary model
#'                    N0 = 1e0, Q0 = 1e-3,  # Initial values of the primary model
#'                    mu_opt = 4, # mu_opt of the gamma model
#'                    temperature_n = 1,  # Secondary model for temperature
#'                    aw_xmax = 1, aw_xmin = .9, aw_n = 1  # Secondary model for water activity
#'                    )
#'                    
#' ## The rest, need initial guesses (you know, regression)
#' 
#' my_start <- list(temperature_xmin = 25, temperature_xopt = 35,
#'                  temperature_xmax = 40, aw_xopt = .95)
#'                  
#' ## We can now fit the model
#' 
#' \donttest{
#' dynamic_fit <- fit_growth(example_dynamic_growth, 
#'                           sec_models, 
#'                           my_start, known_pars,
#'                           environment = "dynamic",
#'                           env_conditions = example_env_conditions
#'                           ) 
#'                           
#' ## The instance of FitDynamicGrowth has several S3 methods
#' 
#' plot(dynamic_fit, add_factor = "temperature")
#' summary(dynamic_fit)
#' }
#' 
#' ## Example 3- Fitting under dynamic conditions using MCMC -------------------
#' 
#' ## We can reuse most of the arguments from the previous example
#' ## We just need to define the algorithm and the number of iterations
#' 
#' \donttest{
#' set.seed(12421)
#' MCMC_fit <- fit_growth(example_dynamic_growth, 
#'                        sec_models, 
#'                        my_start, known_pars,
#'                        environment = "dynamic",
#'                        env_conditions = example_env_conditions,
#'                        algorithm = "MCMC",
#'                        niter = 1000
#'                        ) 
#'                        
#' ## The instance of FitDynamicGrowthMCMC has several S3 methods
#' 
#' plot(MCMC_fit, add_factor = "aw")
#' summary(MCMC_fit)
#' }
#' 
#' ## Example 4 - Fitting a unique model to several dynamic experiments --------
#' 
#' ## We will use the data included in the package
#' 
#' data("multiple_counts")
#' data("multiple_conditions")
#' 
#' ## We need to assign a model equation for each environmental factor
#' 
#' sec_models <- list(temperature = "CPM", pH = "CPM")
#' 
#' ## Any model parameter (of the primary or secondary models) can be fixed
#' 
#' known_pars <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
#'                    temperature_n = 2, temperature_xmin = 20, 
#'                    temperature_xmax = 35,
#'                    pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5)
#'                    
#' ## The rest, need initial guesses
#' 
#' my_start <- list(mu_opt = .8, temperature_xopt = 30)
#' 
#' ## We can now fit the model
#' 
#' \donttest{
#' global_fit <- fit_growth(multiple_counts, 
#'                          sec_models, 
#'                          my_start, 
#'                          known_pars,
#'                          environment = "dynamic",
#'                          algorithm = "regression",
#'                          approach = "global",
#'                          env_conditions = multiple_conditions
#'                          ) 
#'                          
#' ## The instance of FitMultipleDynamicGrowth has nice S3 methods
#' 
#' plot(global_fit)
#' summary(global_fit)
#' print(global_fit)
#' }
#' 
#' ## Example 5 - MCMC fitting a unique model to several dynamic experiments ---
#' 
#' ## Again, we can re-use all the arguments from the previous example
#' ## We just need to define the right algorithm and the number of iterations
#' ## On top of that, we will also pass upper and lower bounds to modMCMC
#' 
#' \donttest{
#' set.seed(12421)
#' global_MCMC <- fit_growth(multiple_counts, 
#'                          sec_models, 
#'                          my_start, 
#'                          known_pars,
#'                          environment = "dynamic",
#'                          algorithm = "MCMC",
#'                          approach = "global",
#'                          env_conditions = multiple_conditions,
#'                          niter = 1000,
#'                          lower = c(.2, 29),  # lower limits of the model parameters
#'                          upper = c(.8, 34)  # upper limits of the model parameters
#'                          ) 
#'                          
#' ## The instance of FitMultipleDynamicGrowthMCMC has nice S3 methods
#' 
#' plot(global_MCMC)
#' summary(global_MCMC)
#' print(global_MCMC)
#' }
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
                    warning("argument niter is ignored for fitting using regression")
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