

#' Comparison of growth fits
#' 
#' @importFrom FME modCost
#' 
#' @export
#' 
compare_growth_fits <- function(models) {
    
    ## Check for model types
    
    model_type <- unique(map_chr(models, ~ class(.)[1]))
    
    if (length(model_type) > 1) {
        stop("Every model must be of the same class")
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


