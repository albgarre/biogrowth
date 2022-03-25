

#' AA
#' 
#' @importFrom FME modCost
#' 
#' @export
#' 
compare_growth_fits <- function(models) {
    
    model_type <- unique(map_chr(models, ~ class(.)[1]))
    
    if (length(model_type) > 1) {
        stop("Every model must be of the same class")
    }
    
    if (is.FitIsoGrowth(models[[1]])) {

        d <- as.data.frame(models[[1]]$data)
        
        ## Calculate residuals 
        
        t <- seq(0, max(d$time, na.rm = TRUE), length = 1000)
        
        residuals <- models %>%
            map(
                ~ data.frame(time = t, 
                             logN = predict(., times = t)
                             )
                ) %>%
            map(~ modCost(model = ., obs = d)
            )
        
       ## Return
        
        out <- list(models = models,
                    residuals = residuals)

        class(out) <- c("ComparisonIsoGrowth", class(out))
        
        return(out)
        
    } else {
        stop("Model type not supported")
    }

}


