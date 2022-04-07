
#' DynamicGrowth class
#' 
#' @description 
#' `r lifecycle::badge("superseded")`
#' 
#' The class [DynamicGrowth] has been superseded by the top-level
#' class [GrowthPrediction], which provides a unified approach for growth modelling.
#' 
#' Still, it is still returned if the superseded [predict_dynamic_growth()] is called.
#' 
#' A subclass of list with items:
#' \itemize{
#' \item simulation: A tibble with the model prediction
#' \item gammas: A tibble with the value of each gamma factor for each
#' value of `times`.
#' \item env_conditions: A list of functions interpolating the environmental
#' conditions.
#' \item primary_pars: A list with the model parameters of the primary model.
#' \item sec_models: A nested list defining the secondary models.
#' }
#' 
#' @name DynamicGrowth
#'   
NULL

#' @describeIn DynamicGrowth print of the model
#' 
#' @param x An instance of `DynamicGrowth`.
#' @param ... ignored
#' 
#' @export
#' 
print.DynamicGrowth <- function(x, ...) {
    
    cat("Growth prediction under dynamic conditions\n\n")
    
    env <- names(x$env_conditions)
    cat(paste("Environmental factors included:", paste(env, collapse = ", "), "\n\n"))
    
    cat("Parameters of the primary model:\n")
    print(unlist(x$primary_pars))
    cat("\n")
    
    logbase <- x$logbase_mu
    
    if ( abs(logbase - exp(1)) < .1 ) {
        logbase <- "e"
    }
    cat(paste0("Parameter mu defined in log-", logbase, " scale\n\n"))
    
    for (i in 1:length(x$sec_models)) {
        cat(paste("Secondary model for ", names(x$sec_models)[i], ":\n", sep = ""))
        print(unlist(x$sec_models[[i]]))
        cat("\n")
    }
    
}

#' @describeIn DynamicGrowth predicted growth curve under dynamic conditions.
#'
#' @param x The object of class `DynamicGrowth` to plot.
#' @param y ignored
#' @param ... additional arguments passed to `plot`.
#' @param add_factor whether to plot also one environmental factor.
#' If `NULL` (default), no environmental factor is plotted. If set
#' to one character string that matches one entry of x$env_conditions,
#' that condition is plotted in the secondary axis
#' @param ylims A two dimensional vector with the limits of the primary y-axis.
#' @param label_y1 Label of the primary y-axis.
#' @param label_y2 Label of the secondary y-axis.
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: [geom_line()]
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: [geom_line()]
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: [geom_line()]
#' @param line_col2 Same as lin_col, but for the environmental factor.
#' @param line_size2 Same as line_size, but for the environmental factor.
#' @param line_type2 Same as lin_type, but for the environmental factor.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line scale_y_continuous sec_axis
#' @importFrom rlang .data
#' @importFrom cowplot theme_cowplot
#'
plot.DynamicGrowth <- function(x, y=NULL, ...,
                               add_factor = NULL,
                               ylims = NULL,
                               label_y1 = "logN",
                               label_y2 = add_factor,
                               line_col = "black",
                               line_size = 1,
                               line_type = "solid",
                               line_col2 = "black",
                               line_size2 = 1,
                               line_type2 = "dashed",
                               label_x = "time"
) {
    
    p <- ggplot(x$simulation) +
        geom_line(aes(x = .data$time, y = .data$logN),
                  col = line_col,
                  size = line_size,
                  linetype = line_type) +
        ylab(label_y1)
    
    if(!is.null(add_factor)) {
        
        min_time <- 0
        max_time <- max(x$simulation$time)
        
        min_count <- min(x$simulation$logN, na.rm = TRUE)
        max_count <- max(x$simulation$logN, na.rm = TRUE)
        
        tt <- seq(min_time, max_time, length = 1000)
        min_temp <- min(x$env_conditions[[add_factor]](tt))
        max_temp <- max(x$env_conditions[[add_factor]](tt))
        
        if (max_temp == min_temp) {  # Isothermal profile
            
            max_temp <- max_temp + 1
            min_temp <- min_temp - 1
            
        }
        
        slope <- (max_count - min_count)/(max_temp - min_temp)
        intercept <- max_count - slope*max_temp
        
        my_t <- seq(0, max_time, length = 1000)
        
        aa <- tibble(time = my_t,
                     y = x$env_conditions[[add_factor]](my_t)) %>%
            mutate(fake_y = .data$y*slope + intercept)
        
        my_line <- geom_line(aes(x = .data$time, y = .data$fake_y),
                             data = aa, linetype = line_type2,
                             colour = line_col2, size = line_size2)
        
        p <- p +
            my_line +
            scale_y_continuous(limits = ylims,
                               name = label_y1,
                               sec.axis = sec_axis(~(. - intercept)/slope,
                                                   name = label_y2))
        
        
    }
    
    p + theme_cowplot() + xlab(label_x)
    
}

#' @describeIn DynamicGrowth coefficients of the model
#' 
#' @param object an instance of [DynamicGrowth]
#' @param ... ignored
#' 
#' @export
#' 
coef.DynamicGrowth <- function(object, ...) {
    
    list(
        primary = object$primary_pars,
        secondary = object$sec_models
        )
    
}

