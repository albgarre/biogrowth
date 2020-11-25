
#' StochasticGrowth class
#' 
#' @description 
#' The \code{StochasticGrowth} class contains the results of a growth prediction
#' under isothermal conditions considering parameter unceratinty. Its constructor 
#' is \code{\link{predict_stochastic_growth}}.
#' 
#' It is a subclass of list with the items:
#' \itemize{
#' \item sample: parameter sample used for the calculations.
#' \item simulations: growth curves predicted for each parameter.
#' \item quantiles: limits of the credible intervals (5%, 10%, 50%, 90%, 95%) for
#' each time point.
#' \item model: Model used for the calculations.
#' \item mus: Mean parameter values used for the simulations.
#' \item sigma: Variance-covariance matrix used for the simulations.
#' }
#' 
#' @name StochasticGrowth
#'   
NULL

#' @describeIn StochasticGrowth Growth prediction (prediction band) considering
#' parameter uncertainty.
#'
#' @param x The object of class \code{StochasticGrowth} to plot.
#' @param y ignored
#' @param ... ignored.
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: \code{\link{geom_line}}
#' @param ribbon80_fill fill colour for the space between the 10th and 90th quantile, see: \code{\link{geom_ribbon}}
#' @param ribbon90_fill fill colour for the space between the 5th and 95th quantile, see: \code{\link{geom_ribbon}}
#' @param alpha80 transparency of the ribbon aesthetic for the space between the 10th and 90th quantile. Takes a value between 0 (fully transparant) and 1 (fully opaque)
#' @param alpha90 transparency of the ribbon aesthetic for the space between the 5th and 95th quantile. Takes a value between 0 (fully transparant) and 1 (fully opaque).
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line geom_ribbon ylab
#' @importFrom rlang .data
#' @importFrom cowplot theme_cowplot
#'
plot.StochasticGrowth <- function(x, y=NULL, ...,
                                  line_col = "black",
                                  line_size = 0.5,
                                  line_type = "solid",
                                  ribbon80_fill = "grey",
                                  ribbon90_fill = "grey",
                                  alpha80 = .5,
                                  alpha90 = .4) {
    
    ggplot(x$quantiles, aes(x = .data$time)) +
        geom_ribbon(aes(ymin = .data$q10, ymax = .data$q90),
                    fill = ribbon80_fill, alpha = alpha80) +
        geom_ribbon(aes(ymin = .data$q05, ymax = .data$q95),
                    fill = ribbon90_fill, alpha = alpha90) +
        geom_line(aes(y = .data$q50),
                  col = line_col,
                  size = line_size,
                  linetype = line_type) +
        ylab("logN") +
        theme_cowplot()
    
}


