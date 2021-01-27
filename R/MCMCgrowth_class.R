
#' MCMCgrowth class
#' 
#' @description 
#' The \code{MCMCgrowth} class contains the results of a growth prediction
#' consider parameter variability based on a model
#' fitted using \code{\link{fit_MCMC_growth}} or 
#' \code{\link{fit_multiple_growth_MCMC}}.
#' 
#' It is a subclass of list with items:
#' \itemize{
#' \item sample: Parameter sample used for the calculations.
#' \item simulations: Individual growth curves calculated based on the parameter
#' sample.
#' \item quantiles: Tibble with the limits of the credible intervals
#'  (5%, 10%, 50%, 90% and 95%) for each time point.
#' \item model: Instance of \code{FitDynamicGrowthMCMC} used for predictions.
#' \itme env_conditions: A tibble with the environmental conditions of the simulation.
#' }
#' 
#' @name MCMCgrowth
#'   
NULL

#' @describeIn MCMCgrowth plot of predicted growth (prediction band).
#'
#' @param x The object of class \code{MCMCgrowth} to plot.
#' @param y ignored
#' @param ... ignored.
#' @param add_factor Includes the variation of one environmental factor in the plot.
#' It must be one of the column names in x$env_conditions.
#' @param alpha_80 transparency of the ribbon for the 80th posterior. .5 by default.
#' @param fill_80 fill colour of the ribbon for the 80th posterior. "grey" by default.
#' @param alpha_90 transparency of the ribbon for the 90th posterior. .5 by default.
#' @param fill_90 fill colour of the ribbon for the 90th posterior. "grey" by default.
#' @param label_y1 label of the primary y axis. "logN" by default.
#' @param label_y2 label of the secondary y axis. The name of the environmental factor
#' by default.
#' @param line_col colour of the line representing the median. "black" by default.
#' @param line_type linetype for the line representing the median. solid by default.
#' @param line_size size of the line representing the median. 1 by default.
#' @param line_type2 linetype for the line representing the environmental condition.
#' Dashed by default.
#' @param line_col2 colour of the line representing the environmental condition. "black"
#' by default.
#' @param line_size2 size of the line representing the environmental condition. 1 by default.
#' @param ylims limits of the primary y-axis. \code{NULL} by default (let ggplot choose).
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line geom_ribbon ylab
#' @importFrom rlang .data
#' @importFrom cowplot theme_cowplot
#'
plot.MCMCgrowth <- function(x, y=NULL, ...,
                            add_factor = NULL,
                            alpha_80 = .5,
                            fill_80 = "grey",
                            alpha_90 = .5,
                            fill_90 = "grey",
                            label_y1 = "logN",
                            label_y2 = add_factor,
                            line_col = "black",
                            line_type = 1,
                            line_size = 1,
                            line_type2 = 2,
                            line_col2 = "black",
                            line_size2 = 1,
                            ylims = NULL) {
    
    p <- ggplot(x$quantiles, aes(x = .data$time)) +
        geom_ribbon(aes(ymin = .data$q10, ymax = .data$q90), 
                    alpha = alpha_80, fill = fill_80) +
        geom_ribbon(aes(ymin = .data$q05, ymax = .data$q95), 
                    alpha = alpha_90, fill = fill_90) +
        geom_line(aes(y = .data$q50), colour = line_col,
                  linetype = line_type, size = line_size) +
        ylab("logN")
    
    if(!is.null(add_factor)) {
        
        my_env <- approx_env(x$env_conditions)[[add_factor]]
        
        min_time <- 0
        max_time <- max(x$simulations$time)
        
        min_count <- min(x$simulations$logN, na.rm = TRUE)
        max_count <- max(x$simulations$logN, na.rm = TRUE)
        
        tt <- seq(min_time, max_time, length = 1000)
        min_temp <- min(my_env(tt))
        max_temp <- max(my_env(tt))
        
        if (max_temp == min_temp) {  # Isothermal profile
            
            max_temp <- max_temp + 1
            min_temp <- min_temp - 1
            
        }
        
        slope <- (max_count - min_count)/(max_temp - min_temp)
        intercept <- max_count - slope*max_temp
        
        my_t <- seq(0, max_time, length = 1000)
        
        aa <- tibble(time = my_t,
                     y = my_env(my_t)) %>%
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
    
    p + theme_cowplot()
    
}

