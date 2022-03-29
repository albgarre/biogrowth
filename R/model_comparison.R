

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
#' ## We fit 3 different models to an example dataset 
#' data("growth_salmonella")
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
#' model_comparison <- compare_growth_fits(list(`Baranyi` = fit1, 
#'                                              `Baranyi no lag` = fit2, 
#'                                              `Gompertz no lag` = fit3))
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
compare_growth_fits <- function(models) {
    
    ## Check for model types
    
    model_type <- unique(map_chr(models, ~ class(.)[1]))
    
    if (length(model_type) > 1) {
        warning("Every model should be of the same class. You are entering untested territories.")
    }
    
    ## Check if it is global or single
    
    if (is.FitMultipleDynamicGrowth(models[[1]]) | is.FitMultipleDynamicGrowthMCMC(models[[1]])) {
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
    
    if (is.FitIsoGrowth(models[[1]])) {
        environment <- "static"
    } else {
        environment <- "dynamic"
    }
    
    ## Save the type of algorithm
    
    if (is.FitDynamicGrowthMCMC(models[[1]]) | is.FitMultipleDynamicGrowthMCMC(models[[1]])) {
        algorithm <- "MCMC"
    } else {
        algorithm <- "regression"
    }
        
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





