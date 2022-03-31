
#' IsothermalGrowth class
#' 
#' @description 
#' The `IsothermalGrowth` class contains the results of a growth prediction
#' under isothermal conditions. Its constructor is [predict_isothermal_growth()].
#' 
#' It is a subclass of list with the items:
#' \itemize{
#' \item simulation: A tibble with the model simulation.
#' \item model: The name of the model used for the predictions.
#' \item pars: A list with the values of the model parameters.
#' }
#' 
#' @name IsothermalGrowth
#'   
NULL

#' @describeIn IsothermalGrowth print of the model
#' 
#' @param x An instance of `IsothermalGrowth`.
#' @param ... ignored
#' 
#' @export
#' 
print.IsothermalGrowth <- function(x, ...) {
    
    cat("Growth prediction based on primary models\n\n")
    
    cat(paste("Growth model:", x$model, "\n\n"))
    
    cat("Parameters of the primary model:\n")
    print(unlist(x$pars))
    
    logbase <- x$logbase_mu
    
    if ( abs(logbase - exp(1)) < .1 ) {
        logbase <- "e"
    }
    cat("\n")
    cat(paste0("Parameter mu defined in log-", logbase, " scale"))
    
}

#' @describeIn IsothermalGrowth plot of the predicted growth curve.
#'
#' @param x The object of class [IsothermalGrowth] to plot.
#' @param y ignored
#' @param ... ignored.
#' @param line_col Aesthetic parameter to change the colour of the line,
#' see: [geom_line()]
#' @param line_size Aesthetic parameter to change the thickness of the line,
#' see: [geom_line()]
#' @param line_type Aesthetic parameter to change the type of the line,
#' takes numbers (1-6) or strings ("solid") see: [geom_line()]
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line
#' @importFrom rlang .data
#' @importFrom cowplot theme_cowplot
#'
plot.IsothermalGrowth <- function(x, y=NULL, ...,
                                  line_col = "black",
                                  line_size = 1,
                                  line_type = "solid") {
    
    ggplot(x$simulation) +
        geom_line(aes(x = .data$time, y = .data$logN),
                  col = line_col,
                  size = line_size,
                  linetype = line_type) +
        theme_cowplot()
    
}

#' @describeIn IsothermalGrowth coefficients of the model
#' 
#' @param object an instance of [IsothermalGrowth]
#' @param ... ignored
#' 
#' @export
#' 
coef.IsothermalGrowth <- function(object, ...) {
    
    unlist(object$pars)
    
}


















