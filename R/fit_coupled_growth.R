
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
#' @param my_d tibble (or data.frame) of data. It must have one column named `temp` (temperature),
#' one named `lambda` (specific growth rate; in ln CFU/TIME) and one named `mu` 
#' (specific growth rate; in ln CFU/TIME).
#' @param weight type of weights to apply. Either `NULL` (no weights; default), 
#' `sd` (standard deviation) or `mean` (mean value).
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

#' Growth fitting considering link between mu and lambda
#' 
#' @param fit_data a tibble (or data.frame) with the data for the fit. The content must
#' be different depending on the fitting mode (see Details).
#' @param start a numeric vector of initial guesses for the parameter estimates
#' @param known a numeric vector of known mode parameters
#' @param mode the type of model fitting approach. Either `two_steps` (fitted from the
#' values of `mu` and `lambda`) or `one_step` (fitted from logN)
#' @param weight weights to apply for the `two_steps` fit. Either `NULL` (no weights), 
#' `sd` (standard deviation; default) or `mean` (mean value). 
#' @param ... ignored
#' @param logbase_mu AA
#' @param logbase_logN AA
#' 
#' @export
#' 
fit_coupled_growth <- function(fit_data, start, known, 
                               mode = "two_steps",
                               weight = "sd",
                               ...,
                               logbase_mu = exp(1), logbase_logN = 10
                               ) {

  if (mode == "two_steps") {  # Fitting directly from mu and lambda
    
    ## Calculate the sq of mu (I dont wanna calculate it 1M times)
    
    fit_data <- mutate(fit_data, sq_mu = sqrt(mu))
    
    ## Fit the model
    
    my_fit <- modFit(cost_coupled_twosteps, 
                     start,
                     this_data = fit_data,
                     known = known,
                     weight = weight
           )
    
    ## Output
    
    out <- list(fit = my_fit,
                mode = "two_steps",
                weight = weight,
                logbase_mu = logbase_mu,
                data = select(fit_data, temp, mu, lambda),
                known = known
                )
    
    class(out) <- c("FitCoupledGrowth", class(out))
    
    out
    
  } else if (mode == "one_step") {  # Fitting from logN
    
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
                logbase_mu = logbase_mu,
                data = fit_data,
                known = known
    )
    
    class(out) <- c("FitCoupledGrowth", class(out))
    
    out
    
  } else {
    
    stop(paste0("Unknown mode: ", mode))
    
  }
  
}


















