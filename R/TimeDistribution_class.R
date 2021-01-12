
#' TimeDistribution class
#' 
#' @description 
#' The \code{TimeDistribution} class contains an estimate of the probability
#' distribution of the time to reach a given microbial count. 
#' Its constructor is \code{\link{distribution_to_logcount}}.
#' 
#' It is a subclass of list with the items:
#' \itemize{
#' \item distribution Sample of the distribution of times to reach \code{log_count}.
#' \item summary Summary statistics of distribution (mean, sd, median, q10 and q90).
#' }
#' 
#' @name TimeDistribution
#'   
NULL

#' @describeIn TimeDistribution plot of the distribution of the time to reach a 
#' microbial count.
#'
#' @param x The object of class \code{TimeDistribution} to plot.
#' @param y ignored.
#' @param ... ignored.
#' @param bin_width A number that specifies the width of a bin in the histogram, 
#' see: \code{\link{geom_histogram}}
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_histogram aes geom_vline xlab
#' @importFrom cowplot theme_cowplot
#'
plot.TimeDistribution <- function(x, y=NULL, ...,
                                  bin_width = 1) {
    
    ggplot() +
        geom_histogram(aes(x$distribution), binwidth = bin_width) +
        geom_vline(xintercept = c(x$summary$med_time),
                   linetype = 2, colour = "red") +
        geom_vline(xintercept = c(x$summary$q10, x$summary$q90),
                   linetype = 2, colour = "darkgrey") +
        xlab("time") +
        theme_cowplot()
    
    
}
