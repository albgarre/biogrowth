
#' FitSerial class
#' 
#' @description 
#' The `FitSerial` class contains growth rates estimated using the 
#' function [fit_serial_dilution()].
#' 
#' It is a subclass of list with the items:
#' \itemize{
#' \item fit: object returned by [nls()].
#' \item mode: fitting approach.
#' \item data: data used for the model fitting.
#' }
#' 
#' @name FitSerial
#'   
NULL

#' @describeIn FitSerial print of the model
#' 
#' @param x An instance of `FitSerial`.
#' @param ... ignored
#' 
#' @export
#' 
print.FitSerial <- function(x, ...) {
  
  cat("Primary model estimated from OD data\n\n")
  
  cat(paste("Fitting approach:", x$mode, "\n\n"))
  
  cat("Parameter estimates:\n")
  print(coef(x))
  
}

#' @describeIn FitSerial vector of fitted model parameters.
#'
#' @param object an instance of [FitSerial].
#' @param ... ignored
#'
#' @importFrom stats coef
#'
#' @export
#'
coef.FitSerial <- function(object, ...) {
  
  coef(object$fit)
  
}

#' @describeIn FitSerial statistical summary of the fit.
#'
#' @param object Instance of [FitSerial]
#' @param ... ignored
#'
#' @export
#'
summary.FitSerial <- function(object, ...) {
  
  summary(object$fit)
  
}

#' @describeIn FitSerial vector of model predictions.
#' 
#' @param object an instance of [FitSerial]
#' @param ... ignored
#' @param newdata tibble (or data.frame) with the conditions for the prediction. 
#' If `NULL` (default), the fitting conditions. 
#'
#' @export
#' 
predict.FitSerial <- function(object, newdata = NULL, ...) {
  
  predict(object$fit, newdata = newdata)
  
}

#' @describeIn FitSerial vector of model residuals.
#'
#' @param object Instance of [FitSerial]
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitSerial <- function(object, ...) {
  
  residuals(object$fit)
  
}

#' @describeIn FitSerial variance-covariance matrix of the model
#'
#' @param object an instance of [FitSerial]
#' @param ... ignored
#'
#' @export
#'
vcov.FitSerial <- function(object, ...) {
  
  vcov(object$fit)
  
}

#' @describeIn FitSerial deviance of the model.
#'
#' @param object an instance of [FitSerial]
#' @param ... ignored
#' 
#' @importFrom stats deviance
#'
#' @export
#'
deviance.FitSerial <- function(object, ...) {
  
  deviance(object$fit)
  
}

#' @describeIn FitSerial vector of fitted values.
#' 
#' @param object an instance of [FitSerial]
#' @param ... ignored
#' 
#' @export
#' 
fitted.FitSerial <- function(object, ...) {
  
  predict(object)
  
}

#' @describeIn FitSerial loglikelihood of the model
#' 
#' @param object an instance of FitSerial
#' @param ... ignored
#' 
#' @export
#' 
logLik.FitSerial <- function(object, ...) {
  
  logLik(object$fit)
  
}

#' @describeIn FitSerial Akaike Information Criterion
#'
#' @param object an instance of FitSerial
#' @param ... ignored
#' @param k penalty for the parameters (k=2 by default)
#' 
#' @importFrom stats logLik
#'
#' @export
#'
AIC.FitSerial <- function(object, ..., k=2) {
  
  AIC(object$fit)
  
}

#' @describeIn FitSerial compares the fitted model against the data.
#'
#' @param x The object of class [FitSerial] to plot.
#' @param y ignored
#' @param ... ignored.
#' @param line_col colour of the line
#' @param line_size size of the line
#' @param line_type type of the line
#' @param point_col colour of the points
#' @param point_size size of the points
#' @param point_shape shape of the point
#' @param label_y label for the y-axis. By default, `NULL` (default value depending on the mode)
#' @param label_x label for the x-axis. By default, `NULL` (default value depending on the mode)
#' 
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point labs
#' @importFrom rlang .data
#' @importFrom graphics plot
#' @importFrom cowplot theme_cowplot 
#'
plot.FitSerial <- function(x, y=NULL, ...,
                                  line_col = "black",
                                  line_size = 1,
                                  line_type = 1,
                                  point_col = "black",
                                  point_size = 3,
                                  point_shape = 16,
                                  label_y = NULL,
                                  label_x = NULL) {
  
  ## Labels
  
  if (is.null(label_y)) label_y <- "Time to Detection (h)"
  if (is.null(label_x)) label_x <- "Number of dilutions"
  
  ## Make the plot
  
  x$data %>%
    filter(!is.na(.data$TTD)) %>%
    mutate(pred = predict(x)) %>%
    ggplot() +
    geom_point(aes(x = dil, y = TTD),
               colour = point_col, 
               size = point_size,
               shape = point_shape
               ) +
    geom_line(aes(x = dil, y = pred),
              colour = line_col,
              linewidth = line_size,
              linetype = line_type
              ) +
    theme_cowplot() +
    labs(x = label_x, y = label_y)
  
  
}


















