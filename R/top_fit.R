


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
#' @section Fitting under constant conditions:
#' When environment="constant", the functions fits a primary growth model to the
#' population size observed during an experiment. In this case, the data has to be
#' a tibble (or data.frame) with two columns: 
#' * time: the elapsed time
#' * logN: the logarithm of the observed population size
#' Nonetheless, the names of the columns can be modified with the formula argument.
#' 
#' The model equation is defined through the model_keys argument. It must include
#' an entry named "primary" assigned to a model. Valid model keys can be retrieved
#' calling [primary_model_data()].
#' 
#' The model is fitted by non-linear regression (using [modFit()]). This algorithm 
#' needs initial guesses for every model parameter. This are defined as a named numeric
#' vector. The names must be valid model keys, which can be retrieved using [primary_model_data()]
#' (see example below). Apart from that, any model parameter can be fixed using the
#' "known" argument. This is a named numeric vector, with the same convenctions as "start".
#' 
#' 
#' @section Fitting under dynamic conditions to a single experiment:
#' When environment="constant" and approach="single", a dynamic growth model combining
#' the Baranyi primary growth model with the gamma approach for the effect of the environmental
#' conditions on the growth rate is fitted to an experiment gathered under dynamic conditions.
#' In this case, the data is similar to fitting under constant conditions: a 
#' tibble (or data.frame) with two columns: 
#' * time: the elapsed time
#' * logN: the logarithm of the observed population size
#' Note that these default names can be changed using the formula argument.
#' 
#' The values of the experimental conditions during the experiment are defined using
#' the "env_conditions" argument. It is a tibble (or data.frame) with one column named ("time")
#' defining the elapsed time. Note that this default name can be modified using the
#' formula argument of the function. The tibble needs to have as many additional 
#' columns as environmental conditions included in the model, providing the values 
#' of the environmental conditions. 
#' 
#' The model equations are defined through the model_keys argument. It must be a named
#' list where the names match the column names of "env_conditions" and the values
#' are model keys. These can be retrieved using [secondary_model_data()].
#' 
#' The model can be fitted using regression ([modFit()]) or an adaptive Monte Carlo
#' algorithm ([modMCMC()]). Both algorithms require initial guesses for every model
#' parameter to fit. These are defined through the named numeric vector "start". Each
#' parameter must be named as *factor*+"_"+*parameter*, where *factor* is the name of the 
#' environmental factor defined in "model_keys". The *parameter* is a valid key
#' that can be retrieved from [secondary_model_data()]. For instance, parameter Xmin for
#' the factor temperature would be defined as "temperature_xmin". 
#' 
#' Note that the argument ... allows passing additional arguments to the fitting functions.
#' 
#' @section Fitting under dynamic conditions to multiple experiments (global fitting):
#' When environment="constant" and approach="global", fit_growth tries to find the vector of model
#' parameters that best describe the observations of several growth experiments.
#' 
#' The input requirements are very similar to the case when approach="single". The
#' models (equations, initial guesses, known parameters, algorithms...) are identical.
#' The only difference is that "fit_data" must be a list, where each element describes
#' the results of an experiment (using the same conventions as when approach="single").
#' In a similar fashion, "env_conditions" must be a list describing the values of the
#' environmental factors during each experiment. Although it is not mandatory, it
#' is recommended that the elements of both lists are named. Otherwise, the function
#' assigns automatically-generated names, and matches them by order.#' 
#' 
#' @param fit_data observed microbial growth. The format varies depending on the type
#' of model fit. See the relevant sections (and examples) below for details.
#' @param model_keys a named list assigning equations for the primary and secondary
#' models. See the relevant sections (and examples) below for details.
#' @param start a named numeric vector assigning initial guesses to the model parameters
#' to estimate from the data. See relevant section (and examples) below for details.
#' @param known named numeric vector of fixed model parameters, using the same conventions
#' as for "start".
#' @param environment type of environment. Either "constant" (default) or "dynamic" (see below for details 
#' on the calculations for each condition)
#' @param algorithm either "regression" (default; Levenberg-Marquard algorithm) or "MCMC"
#' (Adaptive Monte Carlo algorithm).
#' @param approach approach for model fitting. Either "single" (the model is fitted to
#' a unique experiment) or "global" (the model is fitted to several dynamic experiments).
#' @param env_conditions Tibble describing the variation of the environmental
#' conditions for dynamic experiments. See the relevant sections (and examples) below
#' for details. Ignored for environment="constant".
#' @param niter number of iterations of the MCMC algorithm. Ignored when algorithm!="MCMC".
#' @param ... Additional arguments for [modFit()].
#' @param check Whether to check the validity of the models. TRUE by default.
#' @param logbase aa # TODO  
#' @param formula An object of class "formula" defining the names of the x and y variables in 
#' the data. `logN ~ time` as a default.
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
#' ## A list of model keys can be gathered from 
#' 
#' primary_model_data()
#'                       
#' ## The primary model is defined as a list
#' 
#' models <- list(primary = "Baranyi")
#' 
#' ## The keys of the model parameters can also be gathered from primary_model_data
#' 
#' primary_model_data("Baranyi")$pars
#' 
#' ## Any model parameter can be fixed
#' 
#' known <- c(mu = .2)
#' 
#' ## The remaining parameters need initial guesses 
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
#' ## Valid keys for secondary models can be retrived from
#' 
#' secondary_model_data()
#' 
#' ## We need to assign a model equation (secondary model) to each environmental factor
#' 
#' sec_models <- list(temperature = "CPM", aw = "CPM")
#' 
#' ## The keys of the model parameters can be gathered from the same function
#' 
#' secondary_model_data("CPM")$pars
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
#' 
#' ## It can also make growth predictions including uncertainty
#' 
#' uncertain_growth <- predictMCMC(MCMC_fit, 
#'                                 seq(0, 10, length = 1000),  
#'                                 example_env_conditions, 
#'                                 niter = 1000)
#' 
#' ## The instance of MCMCgrowth includes several nice S3 methods
#' 
#' plot(uncertain_growth)
#' print(uncertain_growth)
#' 
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
#' 
#' ## It can also be used to make model predictions with parameter uncertainty
#' 
#' uncertain_prediction <- predictMCMC(global_MCMC,
#'                                     seq(0, 50, length = 1000), 
#'                                     multiple_conditions[[1]], 
#'                                     niter = 100
#'                                     )
#' 
#' ## The instance of MCMCgrowth includes several nice S3 methods
#' 
#' plot(uncertain_growth)
#' print(uncertain_growth)
#' 
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
                       logbase_mu = 10,  # TODO
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

        out <- fit_isothermal_growth(fit_data, my_model, start, known,
                              ..., 
                              check = check,
                              formula = formula,
                              logbase_mu = logbase_mu
                              )
        
        ## Overwrite the class
        
        class(out) <- c("GrowthFit", "list")
        
        ## Adapt the attributes for the new class

        out$environment <- "constant"
        out$algorithm <- "regression"
        out$start <- out$starting_point
        out$primary_model <- out$model
        out$fit_results <- out$fit
        out$sec_models <- NULL  # only for dynamic fits
        out$env_conditions <- NULL  # only for dynamic fits
        out$niter <- NULL  # only for MCMC fits
        
        out$model <- NULL  # superseded by primary_model
        out$starting_point <- NULL  # superseded by start
        out$fit <- NULL  # superseded by fit_results
        
        ## Return
        
        out
        
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

            out <- fit_dynamic_growth(fit_data, env_conditions,
                                      start, known,
                                      unlist(model_keys),
                                      ...,
                                      check = check,
                                      formula = formula,
                                      logbase_mu = logbase_mu
                                      )
            
            ## Overwrite the class
            
            class(out) <- c("GrowthFit", "list")
            
            ## Adapt the attributes for the new class

            out$environment <- "dynamic"
            out$algorithm <- "regression"
            out$start <- out$starting
            out$primary_model <- "Baranyi"
            out$niter <- NULL  # only for MCMC fits
            
            out$starting <- NULL  # superseded by start
            
            ## Return
            
            out
            
        } else if(algorithm == "MCMC" & approach == "single")  {  # Dynamic fitting by MCMC
            
            ## Fit the model
            
            out <- fit_MCMC_growth(fit_data, env_conditions,
                                   start, known,
                                   unlist(model_keys),
                                   niter,
                                   ...,
                                   check = check,
                                   formula = formula,
                                   logbase_mu = logbase_mu
                                   )
            
            ## Overwrite the class
            
            class(out) <- c("GrowthFit", "list")
            
            ## Adapt the attributes for the new class

            out$environment <- "dynamic"
            out$algorithm <- "MCMC"
            out$start <- out$starting
            out$primary_model <- "Baranyi"
            out$niter <- niter
            
            out$starting <- NULL  # superseded by start
            
            ## Return
            
            out
            
        } else if(algorithm == "regression" & approach == "global")  {  # Global fitting by regression

            ## Check whether niter was defined 
            
            if (check) {
                
                if(!is.null(niter)) {
                    warning("argument niter is ignored for fitting under constant conditions")
                }
                
            }
            
            ## Put the data together
            
            if (is.null(names(fit_data))) {  # In case the list were unnamed
                names(fit_data) <- paste0("exp_", 1:length(fit_data))
                names(env_conditions) <- paste0("exp_", 1:length(fit_data))
            }
            
            my_data <- names(fit_data) %>%
                map(~ list(
                        data = fit_data[[.]],
                        conditions = env_conditions[[.]]
                    )
                )
            
            names(my_data) <- names(fit_data)
            
            ## Fit the model
            
            out <- fit_multiple_growth(
                start,
                my_data,
                known,
                unlist(model_keys),
                ...,
                check = check,
                formula = formula,
                logbase_mu = logbase_mu
                )
            
            ## Overwrite the class
            
            class(out) <- c("GlobalGrowthFit", "list")
            
            ## Adapt the attributes for the new class
            
            out$algorithm <- "regression"
            out$start <- out$starting
            out$primary_model <- "Baranyi"
            out$niter <- NULL  # only for MCMC fits
            out$environment <- "dynamic"
            
            out$starting <- NULL  # superseded by start
            
            ## Return
            
            out
            
        } else if(algorithm == "MCMC" & approach == "global")  {  # Global fitting by MCMC
            
            ## Put the data together
            
            if (is.null(names(fit_data))) {  # In case the list were unnamed
                names(fit_data) <- paste0("exp_", 1:length(fit_data))
                names(env_conditions) <- paste0("exp_", 1:length(fit_data))
            }
            
            my_data <- names(fit_data) %>%
                map(~ list(
                        data = fit_data[[.]],
                        conditions = env_conditions[[.]]
                    )
                )
            
            names(my_data) <- names(fit_data)
            
            ## Fit the model
            
            out <- fit_multiple_growth_MCMC(
                start,
                my_data,
                known,
                unlist(model_keys),
                niter,
                ...,
                check = check,
                formula = formula,
                logbase_mu = logbase_mu
                )
            
            ## Overwrite the class
            
            class(out) <- c("GlobalGrowthFit", "list")
            
            ## Adapt the attributes for the new class
            
            out$algorithm <- "MCMC"
            out$start <- out$starting
            out$primary_model <- "Baranyi"
            out$niter <- niter
            out$environment <- "dynamic"
            
            out$starting <- NULL  # superseded by start
            
            ## Return
            
            out
            
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