
#' IsothermalGrowth class
#' 
#' @description 
#' The \code{IsothermalGrowth} class contains the results of a growth prediction
#' under isothermal conditions. Its constructor is \code{\link{predict_isothermal_growth}}.
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
#' @param x An instance of \code{IsothermalGrowth}.
#' @param ... ignored
#' 
#' @export
#' 
print.IsothermalGrowth <- function(x, ...) {
    
    cat("Growth prediction based on primary models\n\n")
    
    cat(paste("Growth model:", x$model, "\n\n"))
    
    cat("Parameters of the primary model:\n")
    print(unlist(x$pars))
    
}

#' @describeIn IsothermalGrowth plot of the predicted growth curve.
#'
#' @param x The object of class \code{IsothermalGrowth} to plot.
#' @param y ignored
#' @param ... ignored.
#' @param line_col Aesthetic parameter to change the colour of the line,
#' see: \code{\link{geom_line}}
#' @param line_size Aesthetic parameter to change the thickness of the line,
#' see: \code{\link{geom_line}}
#' @param line_type Aesthetic parameter to change the type of the line,
#' takes numbers (1-6) or strings ("solid") see: \code{\link{geom_line}}
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



















