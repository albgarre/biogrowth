

#' Model comparison and selection for growth models
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' This function is a constructor for [GrowthComparison] or [GlobalGrowthComparison],
#' a class that provides several functions for model comparison and model selection
#' for growth models fitted using [fit_growth()]. Please see the help pages for 
#' [GrowthComparison] or [GlobalGrowthComparison] for further details.
#' 
#' Although it is not necessary, we recommend passing the models as a named list,
#' as these names will later be kept in plots and tables.
#' 
#' @param models a (we recommend named) list of models fitted using [fit_growth()]. 
#' Every model should be of the same class. Otherwise, some functions may give unexpected results.
#' 
#' @importFrom FME modCost
#' 
#' @export
#' 
#' @examples 
#' 
#' ## Example 1 - Fitting under static environmental conditions ----------------
#' 
#' ## We will use the data on growth of Salmonella included in the package
#' 
#' data("growth_salmonella")
#' 
#' ## We will fit 3 different models to the data
#' 
#' fit1 <- fit_growth(growth_salmonella, 
#'                    list(primary = "Baranyi"),
#'                    start = c(lambda = 0, logNmax = 8, mu = .1, logN0 = 2),
#'                    known = c(),
#'                    environment = "constant",
#'                    )
#'                    
#' fit2 <- fit_growth(growth_salmonella,
#'                    list(primary = "Baranyi"),
#'                    start = c(logNmax = 8, mu = .1, logN0 = 2),
#'                    known = c(lambda = 0),
#'                    environment = "constant",
#'                    )
#'                    
#' fit3 <- fit_growth(growth_salmonella,
#'                    list(primary = "modGompertz"),
#'                    start = c(C = 8, mu = .1, logN0 = 2),
#'                    known = c(lambda = 0),
#'                    environment = "constant",
#'                    )
#'                    
#' ## We can now put them in a (preferably named) list
#' 
#' my_models <- list(`Baranyi` = fit1, 
#'                   `Baranyi no lag` = fit2, 
#'                   `Gompertz no lag` = fit3)
#'                   
#' ## And pass them to compare_growth_fits
#'                    
#' model_comparison <- compare_growth_fits(my_models)
#' 
#' ##  The instance of GrowthComparison has useful S3 methods
#' 
#' print(model_comparison)
#' plot(model_comparison)
#' plot(model_comparison, type = 2)
#' plot(model_comparison, type = 3)
#' 
#' ## The statistical indexes can be accessed through summary and coef
#' 
#' summary(model_comparison)
#' coef(model_comparison)
#' 
#' ## Example 2 - Fitting under dynamic environmental conditions ---------------
#' \donttest{
#' 
#' ## We will use one of the example datasets
#' 
#' data("example_dynamic_growth")
#' data("example_env_conditions")
#' 
#' ## First model fitted
#' 
#' sec_models <- list(temperature = "CPM", aw = "CPM")
#' 
#' known_pars <- list(Nmax = 1e4, 
#'                    N0 = 1e0, Q0 = 1e-3, 
#'                    mu_opt = 4,
#'                    temperature_n = 1,
#'                    aw_xmax = 1, aw_xmin = .9, aw_n = 1 
#'                    )
#'                    
#' my_start <- list(temperature_xmin = 25, temperature_xopt = 35,
#'                  temperature_xmax = 40, aw_xopt = .95)
#'                  
#' dynamic_fit <- fit_growth(example_dynamic_growth,
#'                           sec_models,
#'                           my_start, known_pars,
#'                           environment = "dynamic",
#'                           env_conditions = example_env_conditions
#'                           )
#'                           
#' ## Second model (different secondary model for temperature)
#' 
#' sec_models <- list(temperature = "Zwietering", aw = "CPM")
#' 
#' known_pars <- list(Nmax = 1e4, 
#'                    N0 = 1e0, Q0 = 1e-3,
#'                    mu_opt = 4,
#'                    temperature_n = 1,
#'                    aw_xmax = 1, aw_xmin = .9, aw_n = 1 
#'                    )
#'                    
#' my_start <- list(temperature_xmin = 25, temperature_xopt = 35,
#'                  aw_xopt = .95)
#'                  
#' dynamic_fit2 <- fit_growth(example_dynamic_growth,
#'                            sec_models,
#'                            my_start, known_pars,
#'                            environment = "dynamic",
#'                            env_conditions = example_env_conditions
#'                            )
#'                            
#' ## Once both models have been fitted, we can call the function               
#' 
#' dynamic_comparison <- compare_growth_fits(list(m1 = dynamic_fit, m2 = dynamic_fit2))
#' 
#' ## Which also returns an instance of GrowthComparison with the same S3 methods
#' 
#' print(dynamic_comparison)
#' plot(dynamic_comparison)
#' plot(dynamic_comparison, type = 2)
#' plot(dynamic_comparison, type = 3)
#' 
#' ## The statistical indexes can be accessed through summary and coef
#' 
#' summary(dynamic_comparison)
#' coef(dynamic_comparison)
#' }
#' 
#' ## Example 3 - Global fitting -----------------------------------------------
#' \donttest{
#' 
#' ## We use the example data
#' 
#' data("multiple_counts")
#' data("multiple_conditions")
#' 
#' ## We need to fit (at least) two models
#' 
#' sec_models <- list(temperature = "CPM", pH = "CPM")
#' 
#' known_pars <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
#'                    temperature_n = 2, temperature_xmin = 20, 
#'                    temperature_xmax = 35,
#'                    pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5)
#'                    
#' my_start <- list(mu_opt = .8, temperature_xopt = 30)
#' 
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
#' sec_models <- list(temperature = "CPM", pH = "CPM")
#' 
#' known_pars <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
#'                    temperature_n = 1, temperature_xmin = 20, 
#'                    temperature_xmax = 35,
#'                    pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5)
#'                    
#' my_start <- list(mu_opt = .8, temperature_xopt = 30)
#' 
#' global_fit2 <- fit_growth(multiple_counts, 
#'                           sec_models, 
#'                           my_start, 
#'                           known_pars,
#'                           environment = "dynamic",
#'                           algorithm = "regression",
#'                           approach = "global",
#'                           env_conditions = multiple_conditions
#'                           ) 
#'                           
#' ## We can now pass both models to the function as a (named) list
#' 
#' global_comparison <- compare_growth_fits(list(`n=2` = global_fit, 
#'                                               `n=1` = global_fit2)
#'                                               )
#'                                               
#' ## The residuals and model fits plots are divided by experiments
#' 
#' plot(global_comparison)
#' plot(global_comparison, type = 3)
#' 
#' ## The remaining S3 methods are the same as before
#' 
#' print(global_comparison)
#' plot(global_comparison, type = 2)
#' summary(global_comparison)
#' coef(global_comparison)
#' 
#' }
#' 
compare_growth_fits <- function(models) {
    
    # browser()
    
    ## Check for model types
    
    model_type <- unique(map_chr(models, ~ class(.)[1]))
    
    if (length(model_type) > 1) {
        warning("Every model should be of the same class. You are entering untested territory...")
    }
    
    ## Check if it is global or single
    
    if (is.GlobalGrowthFit(models[[1]])) {
        approach <- "global"
    } else {
        approach <- "single"
    }
        
    ## Calculate residuals 
    
    if (approach == "global") {
        
        residuals <- models %>% map(residuals)
        
    } else {
        
        d <- as.data.frame(models[[1]]$data)
        
        t <- seq(0, max(d$time, na.rm = TRUE), length = 1000)
        
        residuals <- models %>%
            map(
                ~ data.frame(time = t, 
                             logN = predict(., times = t)
                )
            ) %>%
            map(~ modCost(model = ., obs = d)
            )
        
    }
    
    ## Save the type of environment
    
    environment <- models[[1]]$environment
    
    ## Save the type of algorithm
    
    algorithm <- models[[1]]$algorithm

   ## Return
        
    out <- list(models = models,
                residuals = residuals,
                environment = environment,
                algorithm = algorithm,
                approach = approach)

    if (approach == "single") {
        
        class(out) <- c("GrowthComparison", class(out))
        
    } else {
        
        class(out) <- c("GlobalGrowthComparison", class(out))
        
    }
    
    return(out)

}

#' Model comparison and selection for secondary growth models
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' This function is a constructor for [SecondaryGrowthComparison]
#' a class that provides several functions for model comparison and model selection
#' for growth models fitted using [fit_secondary_growth()]. Please see the help pages for 
#' [SecondaryGrowthComparison] for further details.
#' 
#' Although it is not necessary, we recommend passing the models as a named list,
#' as these names will later be kept in plots and tables.
#' 
#' @param models a (we recommend named) list of models fitted using [fit_secondary_growth()]. 
#' 
#' @importFrom FME modCost
#' 
#' @export
#' 
#' @examples 
#' 
#' ## We first need to fit some models
#' 
#' data("example_cardinal")
#' 
#' sec_model_names <- c(temperature = "Zwietering", pH = "CPM")
#' 
#' known_pars <- list(mu_opt = 1.2, temperature_n = 1,
#'                    pH_n = 2, pH_xmax = 6.8, pH_xmin = 5.2)
#'                    
#' my_start <- list(temperature_xmin = 5, temperature_xopt = 35,
#'                  pH_xopt = 6.5)
#'                  
#' fit1 <- fit_secondary_growth(example_cardinal, my_start, known_pars, sec_model_names)
#' 
#' known_pars <- list(mu_opt = 1.2, temperature_n = 2,
#'                    pH_n = 2, pH_xmax = 6.8, pH_xmin = 5.2)
#'                    
#'  fit2 <- fit_secondary_growth(example_cardinal, my_start, known_pars, sec_model_names)
#'  
#'  ## We can now pass the models to the constructor
#' 
#'  comparison <- compare_secondary_fits(list(`n=1` = fit1, 
#'                                            `n=2` = fit2))
#'                                            
#'  ## The function includes several S3 methods for model selection and comparison
#'  
#'  print(comparison)
#'  
#'  plot(comparison)
#'  plot(comparison, type = 2)
#'  
#'  ## The numerical indexes can be accessed using coef and summary
#'  
#'  coef(comparison)
#'  summary(comparison)
#'
compare_secondary_fits <- function(models) {
    
    ## Check for model types
    
    model_type <- unique(map_chr(models, ~ class(.)[1]))
    
    if (length(model_type) > 1) {
        stop("Every model mustb be of the same class")
    }
    
    if (!is.FitSecondaryGrowth(models[[1]])) {
        
        stop("Only FitSecondaryGrowth is supported. Use compare_growth_fits for other model types.")
        
    }
    
    ## Return
    
    out <- list(models = models
                )
    
    class(out) <- c("SecondaryGrowthComparison")
    
    return(out)
    
}





