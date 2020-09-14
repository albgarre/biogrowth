
#' Plot of IsothermalGrowth Object
#'
#' Plots the predicted log microbial count for an
#' instance of \code{IsothermalGrowth}.
#'
#' @param x The object of class \code{IsothermalGrowth} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#'
#' @return An instance of \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line
#'
plot.IsothermalGrowth <- function(x, y=NULL, ...) {

    ggplot(x$simulation) +
        geom_line(aes(x = time, y = logN))

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
#' If \code{NULL} (default), no environmenta factor is plotted. If set
#' to one character string that matches one entry of x$env_conditions,
#' that condition is plotted in the secondary axis
#'
#' @return An instance of \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line
#'
plot.DynamicGrowth <- function(x, y=NULL, ...,
                               add_factor = NULL,
                               ylims = NULL,
                               label_y1 = "logN",
                               label_y2 = add_factor) {

    p <- ggplot(x$simulation) +
        geom_line(aes(x = time, y = logN))

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

        my_line <- tibble(time = seq(0, max_time, length = 1000),
               y = x$env_conditions[[add_factor]](time),
               fake_y = y*slope + intercept) %>%
            geom_line(aes(x = time, y = fake_y), data = ., linetype = 2)

        p <- p +
            my_line +
            scale_y_continuous(limits = ylims,
                                              name = label_y1,
                                              sec.axis = sec_axis(~(. + intercept)/slope,
                                                                  name = label_y2))


    }

    p

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
#'
plot.MCMCgrowth <- function(x, y=NULL, ...) {

    ggplot(x$quantiles, aes(x = time)) +
        geom_line(aes(y = q50)) +
        geom_ribbon(aes(ymin = q10, ymax = q90), alpha = .5) +
        geom_ribbon(aes(ymin = q05, ymax = q95), alpha = .5) +
        ylab("logN")

}

#' Plot of StochasticGrowth Object
#'
#' Plots the predicted log microbial count for an
#' instance of \code{StochasticGrowth}.
#'
#' @param x The object of class \code{StochasticGrowth} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#'
#' @return An instance of \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line geom_ribbon ylab
#'
plot.StochasticGrowth <- function(x, y=NULL, ...) {

    ggplot(x$quantiles, aes(x = time)) +
        geom_line(aes(y = q50)) +
        geom_ribbon(aes(ymin = q10, ymax = q90), alpha = .5) +
        geom_ribbon(aes(ymin = q05, ymax = q95), alpha = .5) +
        ylab("logN")

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
#'
#' @return An instance of \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point
#'
plot.FitDynamicGrowthMCMC <- function(x, y=NULL, ...,
                               add_factor = NULL,
                               ylims = NULL,
                               label_y1 = "logN",
                               label_y2 = add_factor) {

    p <- plot(x$best_prediction,
              add_factor = add_factor,
              ylims = ylims,
              label_y1 = label_y1,
              label_y2 = label_y2)

    p + geom_point(aes(x = time, y = logN), data = x$data,
                   inherit.aes = FALSE)

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
#'
#' @return An instance of \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point
#'
plot.FitDynamicGrowth <- function(x, y=NULL, ...,
                                      add_factor = NULL,
                                      ylims = NULL,
                                      label_y1 = "logN",
                                      label_y2 = add_factor) {

    p <- plot(x$best_prediction,
              add_factor = add_factor,
              ylims = ylims,
              label_y1 = label_y1,
              label_y2 = label_y2)

    p + geom_point(aes(x = time, y = logN), data = x$data,
                   inherit.aes = FALSE)

}

#' Plot of FitIsoGrowth Object
#'
#' Plots the predicted log microbial count for an
#' instance of \code{FitIsoGrowth}.
#'
#' @param x The object of class \code{FitIsoGrowth} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#'
#' @return An instance of \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point
#'
plot.FitIsoGrowth <- function(x, y=NULL, ...) {

    p <- plot(x$best_prediction)

    p + geom_point(aes(x = time, y = logN), data = x$data)


}

#' Plot of TimeDistribution Object
#'
#' Plots the predicted log microbial count for an
#' instance of \code{TimeDistribution}.
#'
#' @param x The object of class \code{TimeDistribution} to plot.
#' @param y ignored
#' @param ... additional arguments passed to \code{plot}.
#'
#' @return An instance of \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_histogram aes
#'
plot.TimeDistribution <- function(x, y=NULL, ...) {

    ggplot() +
        geom_histogram(aes(x$distribution)) +
        geom_vline(xintercept = c(x$summary$med_time),
                   linetype = 2, colour = "red") +
        geom_vline(xintercept = c(x$summary$q10, x$summary$q90),
                   linetype = 2, colour = "darkgrey") +
        xlab("time")


}







