
#' TimeDistribution class
#' 
#' @description 
#' The `TimeDistribution` class contains an estimate of the probability
#' distribution of the time to reach a given microbial count. 
#' Its constructor is [distribution_to_logcount()].
#' 
#' It is a subclass of list with the items:
#' \itemize{
#' \item distribution Sample of the distribution of times to reach `log_count`.
#' \item summary Summary statistics of distribution (mean, sd, median, q10 and q90).
#' }
#' 
#' @name TimeDistribution
#'   
NULL

#' @describeIn TimeDistribution print of the model
#' 
#' @param x An instance of `TimeDistribution`.
#' @param ... ignored
#' 
#' @export
#' 
print.TimeDistribution <- function(x, ...) {
    
    cat("Distribution of the time required to reach a target population size\n\n")
    
    print(x$summary)
    
}

#' @describeIn TimeDistribution summary of the model
#' 
#' @param object An instance of `TimeDistribution`.
#' @param ... ignored
#' 
#' @export
#' 
summary.TimeDistribution <- function(object, ...) {

    print(object$summary)
    
}




#' @describeIn TimeDistribution plot of the distribution of the time to reach a 
#' microbial count.
#'
#' @param x The object of class `TimeDistribution` to plot.
#' @param y ignored.
#' @param ... ignored.
#' @param bin_width A number that specifies the width of a bin in the histogram, 
#' see: [ggplot2::geom_histogram()]. `NULL` by default.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_histogram aes geom_vline xlab
#' @importFrom cowplot theme_cowplot
#'
plot.TimeDistribution <- function(x, y=NULL, ...,
                                  bin_width = NULL) {
    
    ggplot() +
        geom_histogram(aes(x$distribution), binwidth = bin_width) +
        geom_vline(xintercept = c(x$summary$med_time),
                   linetype = 2, colour = "red") +
        geom_vline(xintercept = c(x$summary$q10, x$summary$q90),
                   linetype = 2, colour = "darkgrey") +
        xlab("time") +
        theme_cowplot()
    
    
}
