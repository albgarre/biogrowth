
#' MCMCcoupled class
#' 
#' @description 
#' `r lifecycle::badge("stable")`
#' 
#' The `MCMCcoupled` class contains the results of a growth prediction
#' consider parameter variability based on a model
#' fitted using `fit_coupled_growth()`
#' 
#' It is a subclass of list with items:
#' \itemize{
#' \item sample: Parameter sample used for the calculations.
#' \item simulations: Individual growth curves calculated based on the parameter
#' sample.
#' \item quantiles: Tibble with the limits of the credible intervals
#'  (5%, 10%, 50%, 90% and 95%) for each time point.
#' }
#' 
#' @name MCMCcoupled
#'   
NULL

#' @describeIn MCMCcoupled plot of predicted growth (prediction band).
#'
#' @param x The object of class `MCMCcoupled` to plot.
#' @param y ignored
#' @param ... ignored.
#' @param add_factor Includes the variation of one environmental factor in the plot.
#' It must be one of the column names in x$env_conditions.
#' @param alpha_conf transparency of the ribbon for the confidence interval. .5 by default.
#' @param fill_conf fill colour of the ribbon for the confidence interval. "grey" by default.
#' @param alpha_pred transparency of the ribbon for the prediction interval. .5 by default.
#' @param fill_pred fill colour of the ribbon for the prediction interval. "grey" by default.
#' @param label_y label of the  y axis. "logN" by default.
#' @param linecol_conf colour of the line for the confidence interval. "black" by default.
#' @param linetype_conf linetype for the line of the confidence interval. solid by default.
#' @param linecol_pred colour of the line for the prediction interval. "grey45" by default.
#' @param linetype_pred linetype for the line of the prediction interval. solid by default.
#' @param line_size size of the line representing the median. 1 by default.
#' @param label_x label for the x-label ("time" by default).
#' @param line_col color of the line representing the median ("black" by default).
#' @param line_type type for the line representing the median (solid by default).
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line geom_ribbon ylab
#' @importFrom rlang .data
#' @importFrom cowplot theme_cowplot
#'
plot.MCMCcoupled <- function(x, y=NULL, ...,
                            add_factor = NULL,
                            alpha_conf = .5,
                            fill_conf = "grey",
                            linetype_conf = 2,
                            linecol_conf = "grey45",
                            linetype_pred = 1,
                            alpha_pred = .5,
                            fill_pred = "grey",
                            linecol_pred = "grey45",
                            line_col = "black",
                            line_type = 1,
                            line_size = .5,
                            label_y = "logN",
                            label_x = "time"
                            ) {
  
  p <- ggplot(x$quantiles, aes(x = .data$time)) +
    geom_ribbon(aes(ymin = .data$q10, ymax = .data$q90), 
                alpha = alpha_conf, 
                fill = fill_conf,
                linetype = linetype_conf,
                colour = linecol_conf) +
    geom_ribbon(aes(ymin = .data$q10_pred, ymax = .data$q90_pred), 
                alpha = alpha_pred, 
                fill = fill_pred,
                linetype = linetype_pred,
                colour = linecol_pred) +
    geom_line(aes(y = .data$med_logN), 
              colour = line_col,
              linetype = line_type, 
              size = line_size) +
    ylab(label_y) + xlab(label_x) +
    facet_wrap("temp", scales = "free")
  
  p + theme_cowplot()
  
}




