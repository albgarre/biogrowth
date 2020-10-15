
#' Plot of IsothermalGrowth Object
#'
#' Plots the predicted log microbial count for an
#' instance of \code{IsothermalGrowth}.
#'
#' @param x The object of class \code{IsothermalGrowth} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: \code{\link{geom_line}}
#'
#' @return An instance of \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line
#' @importFrom rlang .data
#' @importFrom cowplot theme_cowplot
#'
plot.IsothermalGrowth <- function(x, y=NULL, ...,
                                  line_col = "black",
                                  line_size = 0.5,
                                  line_type = "solid") {

    ggplot(x$simulation) +
        geom_line(aes(x = .data$time, y = .data$logN), 
                  col = line_col, 
                  size = line_size,
                  linetype = line_type) +
        theme_cowplot()

}


#' Plot of DynamicGrowth Object
#'
#' Plots the predicted log microbial count for an
#' instance of \code{DynamicGrowth}.
#'
#' @param x The object of class \code{DynamicGrowth} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#' @param add_factor whether to plot also one environmental factor.
#' If \code{NULL} (default), no environmental factor is plotted. If set
#' to one character string that matches one entry of x$env_conditions,
#' that condition is plotted in the secondary axis
#' @param ylims A two dimensional vector with the limits of the primary y-axis.
#' @param label_y1 Label of the primary y-axis.
#' @param label_y2 Label of the secondary y-axis.
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: \code{\link{geom_line}}
#'
#' @return An instance of \code{ggplot}.
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
                               line_size = 0.5,
                               line_type = "solid") {

    p <- ggplot(x$simulation) +
        geom_line(aes(x = .data$time, y = .data$logN),
                  col = line_col, 
                  size = line_size,
                  linetype = line_type)

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

        my_line <- geom_line(aes(x = .data$time, y = .data$fake_y), data = aa, linetype = 2)

        p <- p +
            my_line +
            scale_y_continuous(limits = ylims,
                              name = label_y1,
                              sec.axis = sec_axis(~(. - intercept)/slope,
                                                  name = label_y2))


    }

    p + theme_cowplot()

}

#' Plot of MCMCgrowth Object
#'
#' Plots the predicted log microbial count for an
#' instance of \code{MCMCgrowth}.
#'
#' @param x The object of class \code{MCMCgrowth} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#'
#' @return An instance of \code{ggplot}.
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

#' Plot of StochasticGrowth Object
#'
#' Plots the predicted log microbial count for an
#' instance of \code{StochasticGrowth}.
#'
#' @param x The object of class \code{StochasticGrowth} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: \code{\link{geom_line}}
#' @param ribbon80_fill fill colour for the space between the 10th and 90th quantile, see: \code{\link{geom_ribbon}}
#' @param ribbon90_fill fill colour for the space between the 5th and 95th quantile, see: \code{\link{geom_ribbon}}
#' @param alpha80 transparency of the ribbon aesthetic for the space between the 10th and 90th quantile. Takes a value between 0 (fully transparant) and 1 (fully opaque) 
#' @param alpha90 transparency of the ribbon aesthetic for the space between the 5th and 95th quantile. Takes a value between 0 (fully transparant) and 1 (fully opaque) 
#'
#' @return An instance of \code{ggplot}.
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
        geom_ribbon(aes(ymin = .data$q10, ymax = .data$q90), fill = ribbon80_fill, alpha = alpha80) +
        geom_ribbon(aes(ymin = .data$q05, ymax = .data$q95), fill = ribbon90_fill, alpha = alpha90) +
        geom_line(aes(y = .data$q50),
                  col = line_col, 
                  size = line_size,
                  linetype = line_type) + 
        ylab("logN") +
        theme_cowplot()

}


#' Plot of FitDynamicGrowthMCMC Object
#'
#' Plots the predicted log microbial count for an
#' instance of \code{FitDynamicGrowthMCMC}.
#'
#' @param x The object of class \code{FitDynamicGrowthMCMC} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#' @param add_factor whether to plot also one environmental factor.
#' If \code{NULL} (default), no environmenta factor is plotted. If set
#' to one character string that matches one entry of x$env_conditions,
#' that condition is plotted in the secondary axis
#' @param ylims A two dimensional vector with the limits of the primary y-axis.
#' @param label_y1 Label of the primary y-axis.
#' @param label_y2 Label of the secondary y-axis.
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: \code{\link{geom_line}}
#' @param point_col Aesthetic parameter to change the colour of the point geom, see: \code{\link{geom_point}
#' @param point_size Aesthetic parameter to change the size of the point geom, see: \code{\link{geom_point} 
#' @param point_shape Aesthetic parameter to change the shape of the point geom, see: \code{\link{geom_point}
#'
#' @return An instance of \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom graphics plot
#' @importFrom rlang .data
#' @importFrom cowplot theme_cowplot
#'
plot.FitDynamicGrowthMCMC <- function(x, y=NULL, ...,
                                   add_factor = NULL,
                                   ylims = NULL,
                                   label_y1 = "logN",
                                   label_y2 = add_factor,
                                   line_col = "black",
                                   line_size = 0.5,
                                   line_type = 1,
                                   point_col = "black",
                                   point_size = 0.5,
                                   point_shape = 16) {
  
  p <- plot(x$best_prediction,
            add_factor = add_factor,
            ylims = ylims,
            label_y1 = label_y1,
            label_y2 = label_y2,
            line_col = line_col,
            line_size = line_size,
            line_type = line_type)
  
  p + geom_point(aes(x = .data$time, y = .data$logN), data = x$data,
                 inherit.aes = FALSE, col = point_col,  size = point_size, shape = point_shape) +
    theme_cowplot()
  
}


#' Plot of FitDynamicGrowth Object
#'
#' Plots the predicted log microbial count for an
#' instance of \code{FitDynamicGrowth}.
#'
#' @param x The object of class \code{FitDynamicGrowth} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#' @param add_factor whether to plot also one environmental factor.
#' If \code{NULL} (default), no environmenta factor is plotted. If set
#' to one character string that matches one entry of x$env_conditions,
#' that condition is plotted in the secondary axis
#' @param ylims A two dimensional vector with the limits of the primary y-axis.
#' @param label_y1 Label of the primary y-axis.
#' @param label_y2 Label of the secondary y-axis.
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: \code{\link{geom_line}}
#' @param point_col Aesthetic parameter to change the colour of the point geom, see: \code{\link{geom_point}
#' @param point_size Aesthetic parameter to change the size of the point geom, see: \code{\link{geom_point} 
#' @param point_shape Aesthetic parameter to change the shape of the point geom, see: \code{\link{geom_point}
#'
#' @return An instance of \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom graphics plot
#' @importFrom rlang .data
#' @importFrom cowplot theme_cowplot
#'
plot.FitDynamicGrowth <- function(x, y=NULL, ...,
                                      add_factor = NULL,
                                      ylims = NULL,
                                      label_y1 = "logN",
                                      label_y2 = add_factor, 
                                      line_col = "black",
                                      line_size = 0.5,
                                      line_type = 1,
                                      point_col = "black",
                                      point_size = 0.5,
                                      point_shape = 16) {

    p <- plot(x$best_prediction,
              add_factor = add_factor,
              ylims = ylims,
              label_y1 = label_y1,
              label_y2 = label_y2,
              line_col = line_col,
              line_size = line_size,
              line_type = line_type)

    p + geom_point(aes(x = .data$time, y = .data$logN), data = x$data,
                   inherit.aes = FALSE, col = point_col,  size = point_size, shape = point_shape) +
        theme_cowplot()

}

#' Plot of FitIsoGrowth Object
#'
#' Plots the predicted log microbial count for an
#' instance of \code{FitIsoGrowth}.
#'
#' @param x The object of class \code{FitIsoGrowth} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: \code{\link{geom_line}}
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: \code{\link{geom_line}}
#' @param point_col Aesthetic parameter to change the colour of the point geom, see: \code{\link{geom_point}
#' @param point_size Aesthetic parameter to change the size of the point geom, see: \code{\link{geom_point} 
#' @param point_shape Aesthetic parameter to change the shape of the point geom, see: \code{\link{geom_point}
#'
#' @return An instance of \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom rlang .data
#' @importFrom graphics plot
#' @importFrom cowplot theme_cowplot
#'
plot.FitIsoGrowth <- function(x, y=NULL, ...,
                              line_col = "black",
                              line_size = 0.5,
                              line_type = 1,
                              point_col = "black",
                              point_size = 0.5,
                              point_shape = 16) {

    p <- plot(x$best_prediction,  
              line_col = line_col,
              line_size = line_size,
              line_type = line_type)

    p + geom_point(aes(x = .data$time, y = .data$logN), data = x$data, col = point_col,  size = point_size, shape = point_shape) +
        theme_cowplot()


}

#' Plot of TimeDistribution Object
#'
#' Plots the predicted log microbial count for an
#' instance of \code{TimeDistribution}.
#'
#' @param x The object of class \code{TimeDistribution} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#' @param bin_width A number that specifies the width of a bin in the histogram, see: \code{\link{geom_histogram} 
#'
#' @return An instance of \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_histogram aes geom_vline xlab
#' @importFrom cowplot theme_cowplot
#'
plot.TimeDistribution <- function(x, y=NULL, ...,
                                  bin_width = 0.5) {

    ggplot() +
        geom_histogram(aes(x$distribution), binwidth = bin_width) +
        geom_vline(xintercept = c(x$summary$med_time),
                   linetype = 2, colour = "red") +
        geom_vline(xintercept = c(x$summary$q10, x$summary$q90),
                   linetype = 2, colour = "darkgrey") +
        xlab("time") +
        theme_cowplot()


}







