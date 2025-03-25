
#' Residuals of the coupled Baranyi model
#' 
#' @param p a numeric vector of model parameters. Must have entries `logN0`, `logNmax`, `logC0`, `b` and `Tmin`
#' @param this_data a tibble (or data.frame) with three columns: `logN` (microbial concentration; in logCFU/TIME), 
#' `temp` the temperature and `time` the storage time
#' @param known a numeric vector of known model parameters
#' 
#' @returns the vector of model residuals
#' 
cost_coupled_onestep <- function(p, this_data, known) {
  
  p <- c(p, known)
  
  pred <- pred_coupled_baranyi(p, this_data$temp, this_data$time)
  
  res <- pred - this_data$logN
  
  res
  
}

#' Predictions of the coupled Baranyi model
#' 
#' @param p a numeric vector of model parameters. Must have entries `logN0`, `logNmax`, `logC0`, `b` and `Tmin`
#' @param temp a numeric vector of temperature values
#' @param times a numeric vector of time points for the prediction
#' 
#' @returns a numeric vector of predicted logN (in log CFU/TIME)
#' 
pred_coupled_baranyi <- function(p, temp, times) {
  
  ## Apply the secondary models
  
  p <- as.list(p)
  
  lambda <- pred_lambda(p, temp)
  sq_mu <- pred_sqmu(p, temp)
  mu <- sq_mu^2
  
  # ## Apply the primary model

  inside <- exp(-mu * times) + exp(-mu*lambda) - exp(-mu*times - mu*lambda)
  A <- times + 1/mu*log(inside)

  logN <- p$logN0 + mu/log(10)*A - 1/log(10)*log(1 + (exp(mu*A) - 1)/(10^(p$logNmax - p$logN0))) 
  
  ## Correct for cases where mu == 0

  logN[mu == 0] <- p$logN0
  
  ## Return
  
  # tibble(time = times, temp = temp, mu = mu, lambda = lambda, logN = logN)
  logN
  
}

#' Prediction of the square root of mu for the coupled model
#' 
#' @param p numeric vector (or list) of model parameters. Must have entries `b` and `Tmin`
#' @param temp numeric vector of temperatures
#' 
#' @returns the values of the square root of mu (in ln CFU/TIME)
#' 
pred_sqmu <- function(p, temp) {
  
  p <- as.list(p)
  
  sq_mu <- p$b*(temp - p$Tmin)
  sq_mu[temp <= p$Tmin] <- 0
  
  sq_mu
  
}

#' Prediction of lambda for the coupled model
#' 
#' @param p numeric vector (or list) of model parameters. Must have entries `logC0`, `b` and `Tmin`
#' @param temp numeric vector of temperatures
#' 
#' @returns the values of lambda
#' 
pred_lambda <- function(p, temp) {
  p <- as.list(p)
  
  B <- log( 1 + 1/(10^p$logC0) )/p$b^2
  lambda <- B/(temp - p$Tmin)^2
  
  lambda[temp <= p$Tmin] <- 0
  
  lambda
  
}


#' Residuals for the square root of mu for the coupled model
#' 
#' @param p numeric vector (or list) of model parameters. Must have entries `b` and `Tmin`
#' @param my_d tibble (or data.frame) of data. It must have one column named `temp` (temperature) 
#' and one named `mu` (specific growth rate; in ln CFU/TIME).
#' 
#' @returns vector of residuals
#' 
residuals_sqmu <- function(p, my_d) {
  
  p <- as.list(p)

  pred_sqmu(p, my_d$temp) - my_d$sq_mu
  
  
}

#' Residuals for lambda for the coupled model
#' 
#' @param p numeric vector (or list) of model parameters. Must have entries `logC0`, `b` and `Tmin`
#' @param my_d tibble (or data.frame) of data. It must have one column named `temp` (temperature) 
#' and one named `lambda` (specific growth rate; in ln CFU/TIME).
#' 
#' @returns vector of residuals
#' 
residuals_lambda <- function(p, my_d) {
  
  p <- as.list(p)
  
  pred_lambda(p, my_d$temp) - my_d$lambda
}

#' Cost for the coupled model fitted in two-steps
#' 
#' @param p numeric vector (or list) of model parameters. Must have entries `logC0`, `b` and `Tmin`
#' @param this_data tibble (or data.frame) of data. It must have one column named `temp` (temperature),
#' one named `lambda` (specific growth rate; in ln CFU/TIME) and one named `mu` 
#' (specific growth rate; in ln CFU/TIME).
#' @param weight type of weights to apply. Either `NULL` (no weights; default), 
#' `sd` (standard deviation) or `mean` (mean value).
#' @param known vector of known model parameters
#' 
#' @returns vector of weighted residuals
#' 
cost_coupled_twosteps <- function(p, this_data,
                                  weight = NULL,
                                  known
                                  ) {
  
  p <- c(p, known)
  
  r1 <- residuals_sqmu(p, this_data)
  r2 <- residuals_lambda(p, this_data)
  
  if (is.null(weight)) {
    w1 <- 1
    w2 <- 1
  } else if (weight == "sd") {
    w1 <- sd(sqrt(this_data$mu), na.rm = TRUE)
    w2 <- sd(this_data$lambda, na.rm = TRUE)
  } else if (weight == "mean") {
    w1 <- mean(sqrt(this_data$mu), na.rm = TRUE)
    w2 <- mean(this_data$lambda, na.rm = TRUE)
  } else {
    stop("weight must be either NULL, sd or mean")
  }

  c(r1/w1, r2/w2)
  
}

#' Growth fitting considering link between mu and lambda for the Baranyi-Ratkowsky model
#' 
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' This function implements the methodology suggested by Garre et al. (2025; doi: 10.1016/j.ijfoodmicro.2025.111078)
#' for the Baranyi-Ratkowsky model. Rather than fitting independent models for mu and lambda,
#' this approach considers a link between both secondary models, reducing the number of 
#' unknown parameters from 3 to 4.
#' 
#' The function implements too modes of fitting: two-steps and one-step. Please see the respective sections
#' for further information.
#' 
#' @section Two-steps fitting:
#' In this mode, it is assumed that primary models have been already fitted to each experiment. Therefore,
#' the data is available as a table of values of mu and lambda estimated at each temperature. Hence,
#' `fit_data` must be a tibble (or data.frame) with three columns: `temp` (storage temperature),
#' `mu` (specific growth rate) and `lambda` (lag phase duration). By default, `mu` must be 
#' defined in the scale of natural logarithm, although this can be modified using the `logbase_mu` argument.
#' The package includes the dataset `example_coupled_twosteps` as an illustration of the type of data.
#' 
#' @section One-step fitting:
#' In this mode, secondary models are directly fitted to the observed (log) microbial counts. Hence,
#' `fit_data` must be a tibble (or data.frame) with three columns: `temp` (storage temperature),
#' `time` (the elapsed time) and `logN` (the log-microbial concentration). By default, `logN` must be 
#' defined in the scale of decimal logarithm, although this can be modified using the `logbase_logN` argument.
#' The package includes the dataset `example_coupled_onestep` as an illustration of the type of data.
#'
#' 
#' @param fit_data a tibble (or data.frame) with the data for the fit. The content must
#' be different depending on the fitting mode (see relevant sections within the help page).
#' @param start a numeric vector of initial guesses for the parameter estimates
#' @param known a numeric vector of known mode parameters. An empty vector by default (no knonw parameter)
#' @param mode the type of model fitting approach. Either `two_steps` (fitted from the
#' values of `mu` and `lambda`) or `one_step` (fitted from logN)
#' @param weight weights to apply for the `two_steps` fit. Either `NULL` (no weights), 
#' `sd` (standard deviation; default) or `mean` (mean value). 
#' @param ... ignored
#' @param logbase_mu Base for the definition of mu. By default, `exp(1)` (natural logarithm).
#' @param logbase_logN Base for the definition of logN. By default, 10 (decimal logarithm).
#' 
#' @export
#' 
#' @examples
#' 
#' ## Example 1: Two-steps fitting-------------------------------------------------
#' 
#' ## We can use the example dataset 
#' 
#' data(example_coupled_twosteps)
#' 
#' ## We need to define initial guesses for every parameter
#' 
#' guess <- c(logC0 = -1, b = .1, Tmin = 5)
#' 
#' ## We can now call the fitting function
#' 
#' my_fit <- fit_coupled_growth(example_coupled_twosteps, 
#'                              start = guess,
#'                              mode = "two_steps")
#' 
#' ## Common S3 methods are included
#' 
#' print(my_fit)
#' coef(my_fit)
#' summary(my_fit)
#' plot(my_fit)
#' 
#' ## Any model parameter can be fixed using the known argument
#' 
#' known <- c(b = .01)
#' 
#' ## Please note that the guess must be updated, as now parameter can appear both as a guess and known
#' 
#' guess <- c(logC0 = -1, Tmin =  0)
#' 
#' 
#' fixed_fit <- fit_coupled_growth(example_coupled_twosteps, 
#'                                 start = guess,
#'                                 known = known, 
#'                                 mode = "two_steps")
#' 
#' print(fixed_fit)
#' coef(fixed_fit)
#' summary(fixed_fit)
#' plot(fixed_fit)
#' 
#' ## Example 2: One-step fitting--------------------------------------------------
#' 
#' ## We can use an example dataset with the right format
#' 
#' data("example_coupled_onestep")
#' 
#' ## The function requires initial guesses for every model parameter
#' 
#' guess <- c(logN0 = 2, logNmax = 8, b = 0.04, logC0 = -4, Tmin = 5)
#' 
#' ## We can now call the fitting function
#' 
#' my_fit <- fit_coupled_growth(example_coupled_onestep,
#'                              start = guess,
#'                              mode = "one_step")
#' 
#' ## The package includes common S3 methods
#' 
#' print(my_fit)
#' coef(my_fit)
#' summary(my_fit)
#' plot(my_fit)
#' 
#' ## Any model parameter can be fixed before fitting
#' 
#' known <- c(logNmax = 7)
#' 
#' ## Guesses must be updated, so no parameter appears twice
#' 
#' guess <- c(logN0 = 2, b = 0.04, logC0 = -4, Tmin = 5)
#' 
#' ## We can now call the fitting function
#' 
#' my_fit <- fit_coupled_growth(example_coupled_onestep,
#'                              start = guess,
#'                              known = known,
#'                              mode = "one_step")
#' 
#' ## The package includes common S3 methods
#' 
#' print(my_fit)
#' coef(my_fit)
#' summary(my_fit)
#' plot(my_fit)
#' 
#' 
fit_coupled_growth <- function(fit_data, start, 
                               known = c(), 
                               mode = "two_steps",
                               weight = "sd",
                               ...,
                               logbase_mu = exp(1), logbase_logN = 10
                               ) {

  if (mode == "two_steps") {  # Fitting directly from mu and lambda
    
    ## Check the model parameters
    
    par_names <- c(names(start), names(known))
    
    if (length(par_names) != 3) {
      stop(paste("The initial guess and the known parameters must define exactly 3 parameters. Found: ", 
                 length(par_names)))
    }
    
    if ( !("b" %in% par_names) ) {
      stop("Parameter 'b' must be assigned an initial guess or fixed.")
    }
    
    if ( !("Tmin" %in% par_names) ) {
      stop("Parameter 'Tmin' must be assigned an initial guess or fixed.")
    }
    
    if ( !("logC0" %in% par_names) ) {
      stop("Parameter 'logC0' must be assigned an initial guess or fixed.")
    }
    
    ## Convert the mu to ln CFU
    
    fit_data$mu <- fit_data$mu*log(logbase_mu)
    
    ## Calculate the sq of mu (I dont wanna calculate it 1M times)

    fit_data$sq_mu <- sqrt(fit_data$mu)
    
    ## Fit the model
    
    my_fit <- modFit(cost_coupled_twosteps, 
                     start,
                     this_data = fit_data,
                     known = known,
                     weight = weight,
                     ...
           )
    
    ## Output
    
    out <- list(fit = my_fit,
                mode = "two_steps",
                weight = weight,
                logbase_mu = logbase_mu,
                data = select(fit_data, "temp", "mu", "lambda"),
                known = known
                )
    
    class(out) <- c("FitCoupledGrowth", class(out))
    
    out
    
  } else if (mode == "one_step") {  # Fitting from logN
    
    ## Check the model parameters
    
    par_names <- c(names(start), names(known))
    
    if (length(par_names) != 5) {
      stop(paste("The initial guess and the known parameters must define exactly 5 parameters. Found: ", 
                 length(par_names)))
    }
    
    if ( !("b" %in% par_names) ) {
      stop("Parameter 'b' must be assigned an initial guess or fixed.")
    }
    
    if ( !("Tmin" %in% par_names) ) {
      stop("Parameter 'Tmin' must be assigned an initial guess or fixed.")
    }
    
    if ( !("logC0" %in% par_names) ) {
      stop("Parameter 'logC0' must be assigned an initial guess or fixed.")
    }
    
    if ( !("logN0" %in% par_names) ) {
      stop("Parameter 'logN0' must be assigned an initial guess or fixed.")
    }
    
    if ( !("logNmax" %in% par_names) ) {
      stop("Parameter 'logNmax' must be assigned an initial guess or fixed.")
    }
    
    ## Convert logN to log CFU
    
    fit_data$logN <- fit_data$logN*log10(logbase_logN)
    
    # Fit the model
    
    my_fit <- modFit(cost_coupled_onestep, 
                     start,
                     this_data = fit_data,
                     known = known
    )
    
    ## Output
    
    out <- list(fit = my_fit,
                mode = "one_step",
                weight = weight,
                data = select(fit_data, "temp", "time", "logN"),
                known = known
    )
    
    class(out) <- c("FitCoupledGrowth", class(out))
    
    out
    
  } else {
    
    stop(paste0("Unknown mode: ", mode))
    
  }
  
}


















