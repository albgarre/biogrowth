
#' FitCoupledGrowth class
#' 
#' @description 
#' The `FitCoupledGrowth` class contains a Baranyi model fitted to experimental data
#' considering the coupling between the primary and secondary models. 
#' Its constructor is [fit_coupled_growth()].
#' 
#' It is a subclass of list with the items:
#' \itemize{
#' \item fit: object returned by [FME::modFit()].
#' \item mode: fitting approach.
#' \item weight: type of weights for the two-steps approach.
#' \item logbase_mu: base of the logarithm used for the calculation of mu.
#' \item data: data used for the model fitting.
#' }
#' 
#' @name FitCoupledGrowth
#'   
NULL

#' @describeIn FitCoupledGrowth print of the model
#' 
#' @param x An instance of `FitCoupledGrowth`.
#' @param ... ignored
#' 
#' @export
#' 
print.FitCoupledGrowth <- function(x, ...) {
  
  cat("Baranyi model fitted from data considering coupling between secondary models\n\n")
  
  cat(paste("Fitting approach:", x$mode, "\n\n"))

  cat("Estimated parameters:\n")
  print(coef(x))
  
}

#' @describeIn FitCoupledGrowth vector of fitted model parameters.
#'
#' @param object an instance of [FitCoupledGrowth].
#' @param ... ignored
#'
#' @importFrom stats coef
#'
#' @export
#'
coef.FitCoupledGrowth <- function(object, ...) {
  
  coef(object$fit)
  
}

#' @describeIn FitCoupledGrowth statistical summary of the fit.
#'
#' @param object Instance of [FitCoupledGrowth]
#' @param ... ignored
#'
#' @export
#'
summary.FitCoupledGrowth <- function(object, ...) {
  
  summary(object$fit)
  
}

#' @describeIn FitCoupledGrowth vector of model predictions.
#' 
#' @param object an instance of [FitCoupledGrowth]
#' @param ... ignored
#' @param newdata tibble (or data.frame) with the conditions for the prediction. 
#' If `NULL` (default), the fitting conditions. For `two_steps` fits, it must have 
#' a column named `temp`. For `one_step`, it must have a column named `temp` and one
#' named `time`. 
#'
#' @export
#' 
predict.FitCoupledGrowth <- function(object, newdata = NULL, ...) {
  
  if (is.null(newdata)) {
    newdata <- object$data
  }

  if (object$mode == "two_steps") {  # Prediction of mu and lambda
    
    p <- c(coef(object), object$known)
    
    tibble(
      temp = newdata$temp,
      lambda = pred_lambda(p, newdata$temp),
      mu = (pred_sqmu(p, newdata$temp))^2
    )
    
  } else {  ## Prediction of logN
    
    p <- c(coef(object), object$known)
    
    pred <- pred_coupled_baranyi(p, newdata$temp, newdata$time)
    
    tibble(temp = newdata$temp,
           time = newdata$time,
           logN = pred
           )
  }
  
}

#' @describeIn FitCoupledGrowth vector of model residuals.
#'
#' @param object Instance of [FitCoupledGrowth]
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitCoupledGrowth <- function(object, ...) {
  
  residuals(object$fit)
  
}

#' @describeIn FitCoupledGrowth variance-covariance matrix of the model, estimated
#' as 1/(0.5*Hessian) for regression
#'
#' @param object an instance of [FitCoupledGrowth]
#' @param ... ignored
#'
#' @export
#'
vcov.FitCoupledGrowth <- function(object, ...) {
  
  covar  <- try(solve(0.5*object$fit$hessian), silent = TRUE)
  
  if (!is.numeric(covar)) {
    warning("Cannot estimate covariance; system is singular")
    
    param  <- object$par
    p      <- length(param)
    
    covar <- matrix(data = NA, nrow = p, ncol = p)
    
    return(covar)
    
  }
  
  ## Scale the variance
  
  rdf <- object$fit$df.residual
  resvar <- object$fit$ssr/rdf
  
  covar*resvar
}

#' @describeIn FitCoupledGrowth deviance of the model.
#'
#' @param object an instance of [FitCoupledGrowth]
#' @param ... ignored
#' 
#' @importFrom stats deviance
#'
#' @export
#'
deviance.FitCoupledGrowth <- function(object, ...) {
  
  deviance(object$fit)
  
}

#' @describeIn FitCoupledGrowth vector of fitted values.
#' 
#' @param object an instance of [FitCoupledGrowth]
#' @param ... ignored
#' 
#' @export
#' 
fitted.FitCoupledGrowth <- function(object, ...) {
  
  predict(object)
  
}

#' @describeIn FitCoupledGrowth loglikelihood of the model
#' 
#' @param object an instance of FitCoupledGrowth
#' @param ... ignored
#' 
#' @export
#' 
logLik.FitCoupledGrowth <- function(object, ...) {
  
  n <- nrow(object$data)
  sigma <- sqrt(object$fit$ssr/object$fit$df.residual)
  
  lL <- - n/2*log(2*pi) -n/2 * log(sigma^2) - 1/2/sigma^2*object$fit$ssr
  
  lL    
  
}

#' @describeIn FitCoupledGrowth Akaike Information Criterion
#'
#' @param object an instance of FitCoupledGrowth
#' @param ... ignored
#' @param k penalty for the parameters (k=2 by default)
#' 
#' @importFrom stats logLik
#'
#' @export
#'
AIC.FitCoupledGrowth <- function(object, ..., k=2) {
  
  ## Normal AIC
  
  p <- length(coef(object))
  
  lL <- logLik(object) 
  
  AIC <- 2*p - 2*lL
  
  ## Calculate the penalty
  
  n <- nrow(object$data)
  
  penalty <- (k*p^2 + k*p)/(n - p - 1)
  
  ## Return
  
  AIC + penalty
  
}

#' @describeIn FitCoupledGrowth compares the fitted model against the data.
#'
#' @param x The object of class [FitCoupledGrowth] to plot.
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
plot.FitCoupledGrowth <- function(x, y=NULL, ...,
                           line_col = "black",
                           line_size = 1,
                           line_type = 1,
                           point_col = "black",
                           point_size = 3,
                           point_shape = 16,
                           label_y = NULL,
                           label_x = NULL) {
  
  if (x$mode == "one_step") {  # plotting of logN
    
    ## Lines and points
    
    d <- x$data
    
    aa <- d %>%
      summarize(t = max(.data$time), .by = "temp") %>%
      mutate(i = row_number()) 
    
    p <- split(aa, aa$i) %>%
      map(
        ~ tibble(temp = .$temp,
                 time = seq(0, .$t, length = 100))
      ) %>%
      map_dfr(
        ~ predict(x, newdata = .)
      ) %>%
      ggplot() +
      geom_line(aes(x = .data$time, y = .data$logN),
                colour = line_col,
                linetype = line_type,
                linewidth = line_size
                ) +
      geom_point(aes(x = .data$time, y = .data$logN), 
                 data = d,
                 colour = point_col,
                 size = point_size,
                 shape = point_shape
                 ) +
      facet_wrap("temp", scales = "free")
    
    ## Aesthetics
    
    if (is.null(label_x)) label_x <- "Time (h)"
    if (is.null(label_y)) label_y <- "logN (log CFU/g)"
    
    p <- p + labs(x = label_x, y = label_y)
    
    ## Returns
    
    p + theme_cowplot()
    
  } else {  # plotting of mu and lambda
    
    ## Lines and points
    
    d <- x$data %>%
      pivot_longer(-c("temp"))
    
    p <- predict(x,
            newdata = tibble(temp = seq(min(d$temp, na.rm = TRUE),
                                        max(d$temp, na.rm = TRUE),
                                        length = 100))
            ) %>%
      pivot_longer(-c("temp")) %>%
      ggplot() +
      geom_line(aes(x = .data$temp, y = .data$value),
                colour = line_col,
                linetype = line_type,
                linewidth = line_size
                ) +
      geom_point(aes(x = .data$temp, y = .data$value), 
                 data = d,
                 colour = point_col,
                 size = point_size,
                 shape = point_shape
                 ) +
      facet_wrap("name", scales = "free")
    
    ## Aesthetics
    
    if (is.null(label_x)) label_x <- "Temperature (C)"
    if (is.null(label_y)) label_y <- "Value"
    
    p <- p + labs(x = label_x, y = label_y)
    
    ## Return 
    
    p + theme_cowplot()
    
  }
  
}


















