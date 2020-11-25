
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
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line geom_ribbon ylab
#' @importFrom rlang .data
#' @importFrom cowplot theme_cowplot
#'
plot.MCMCgrowth <- function(x, y=NULL, ...) {
    
    ggplot(x$quantiles, aes(x = .data$time)) +
        geom_ribbon(aes(ymin = .data$q10, ymax = .data$q90), alpha = .5) +
        geom_ribbon(aes(ymin = .data$q05, ymax = .data$q95), alpha = .5) +
        geom_line(aes(y = .data$q50)) +
        ylab("logN") +
        theme_cowplot()
    
}

