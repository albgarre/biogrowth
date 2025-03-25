
#' Plot of the initial guess for the Baranyi-Ratkowsky model
#' 
#' Compares the prediction corresponding to a guess of the parameters of the Baranyi-Ratkowsky
#' model against experimental data
#' 
#' @param fit_data Tibble (or data.frame) of data for the fit. The shape of the data will depend on the
#' fitting `mode` (see [fit_coupled_growth()])
#' @param mode the type of model fitting approach. Either `two_steps` (fitted from the
#' values of `mu` and `lambda`) or `one_step` (fitted from logN)
#' @param guess Named vector with the initial guess of the model parameters
#' @param logbase_mu Base for the definition of mu. By default, `exp(1)` (natural logarithm).
#' @param logbase_logN Base for the definition of logN. By default, 10 (decimal logarithm).
#' 
#' @return A [ggplot2::ggplot()] comparing the model prediction against the data
#' 
#' @export
#'  
show_guess_coupled <- function(fit_data, 
                               guess, 
                               mode = "two_steps",
                               logbase_mu = exp(1), 
                               logbase_logN = 10
                               ) {
  
  if (mode == "two_steps") {
    
    
    ## Convert mu to ln base
    
    fit_data$mu <- fit_data$mu*log(logbase_mu)
    
    ## Make the predictions
    
    preds <- tibble(temp = seq(min(fit_data$temp, na.rm = TRUE),
                      max(fit_data$temp, na.rm = TRUE),
                      length = 101)) %>%
      mutate(lambda = pred_lambda(guess, .data$temp),
              mu = pred_sqmu(guess, .data$temp)^2
              )
    
    ## Make the plot
    
    p1 <- ggplot(preds) +
      geom_line(aes(x = temp, y = mu)) +
      geom_point(aes(x = temp, y = mu), data = fit_data) +
      theme_cowplot()
    
    p2 <- ggplot(preds) +
      geom_line(aes(x = temp, y = lambda)) +
      geom_point(aes(x = temp, y = lambda), data = fit_data) +
      theme_cowplot()
    
    plot_grid(p2, p1)
      
    
  } else if (mode == "one_step") {
    
    ## Convert logN to log CFU
    
    fit_data$logN <- fit_data$logN*log10(logbase_logN)
    
    ## Make the predictions
    
    preds <- split(fit_data, fit_data$temp) %>%
      map(
        ~ tibble(time = seq(0, 
                            max(.$time, na.rm = TRUE),
                            length = 101
                            ),
                 temp = .$temp[1]
                 )
        ) %>%
      map(.,
          ~ tibble(time = .$time,
                   temp = .$temp,
                   logN = pred_coupled_baranyi(guess, temp, time)
                   )
          ) %>%
      imap_dfr(., ~ mutate(.x, temp = as.numeric(.y)))
    
    ## Make the plot
    
    preds %>%
      ggplot() +
      geom_line(aes(x = time, y = logN)) +
      geom_point(aes(x = time, y = logN), data = fit_data) +
      facet_wrap("temp", scales = "free") +
      theme_cowplot()
    
    
    
  } else {
    
    stop(paste0("Unknown mode: ", mode))
    
  }
  
}



#' Initial guesses for fitting the Baranyi-Ratkowsky model
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' The function uses some heuristics to provide initial guesses for the parameters
#' of the Baranyi-Ratkowsky model selected that can be used with [fit_coupled_growth()].
#' 
#' @param fit_data Tibble (or data.frame) of data for the fit. The shape of the data will depend on the
#' fitting `mode` (see [fit_coupled_growth()])
#' @param mode the type of model fitting approach. Either `two_steps` (fitted from the
#' values of `mu` and `lambda`) or `one_step` (fitted from logN)
#' @param logbase_mu Base for the definition of mu. By default, `exp(1)` (natural logarithm).
#' @param logbase_logN Base for the definition of logN. By default, 10 (decimal logarithm).
#' 
#' @return A named numeric vector of initial guesses for the model parameters
#' 
#' @export
#' 
#' @examples
#' ## Example 1: Two-steps fitting-------------------------------------------------
#' 
#' data(example_coupled_twosteps)
#' 
#' guess <- make_guess_coupled(example_coupled_twosteps)
#' 
#' 
#' show_guess_coupled(example_coupled_twosteps, guess)
#' 
#' my_fit <- fit_coupled_growth(example_coupled_twosteps, 
#'                              start = guess,
#'                              mode = "two_steps")
#' 
#' print(my_fit)
#' coef(my_fit)
#' summary(my_fit)
#' plot(my_fit)
#' 
#' ## Example 2: One-step fitting--------------------------------------------------
#' 
#' data("example_coupled_onestep")
#' 
#' guess <- make_guess_coupled(example_coupled_onestep, mode = "one_step")
#' 
#' show_guess_coupled(example_coupled_onestep,
#'                    guess,
#'                    "one_step")
#' 
#' my_fit <- fit_coupled_growth(example_coupled_onestep,
#'                              start = guess,
#'                              mode = "one_step")
#' 
#' print(my_fit)
#' coef(my_fit)
#' summary(my_fit)
#' plot(my_fit)
#' 
#'  
make_guess_coupled <- function(fit_data, 
                               mode = "two_steps"
                               ) {
  
  if (mode == "two_steps") {
    
    ## Guess for b
    
    Tmax <- max(fit_data$temp, na.rm = TRUE)
    mumin <- min(fit_data$mu, na.rm = TRUE)
    mumax <- max(fit_data$mu, na.rm = TRUE)
    Tmin <- min(fit_data$temp, na.rm = TRUE)
    
    b <- ( sqrt(mumax) - sqrt(mumin) )/(Tmax - Tmin)
    
    ## Guess for Tmin

    Tmin <- mean(fit_data$temp - sqrt(fit_data$mu)/b, na.rm = TRUE)
    
    ## Guess for logC0
    
    C0 <- mean(fit_data$lambda * fit_data$mu, na.rm = TRUE)
    
    ## Return
    
    c(Tmin = Tmin, b = b, logC0 = log10(C0))

  } else if (mode == "one_step") {
    
    ## Guess for logN0
    
    logN0 <- min(fit_data$logN, na.rm = TRUE)
    
    ## Guess for logNmax
    
    logNmax <- max(fit_data$logN, na.rm = TRUE)
    
    ## Estimates for mu and lambda
    
    quickprimary <- split(fit_data, fit_data$temp) %>%
      map(
        ~ lm(logN ~ time, data = .) %>% coef()
      ) %>%
      map(
        ~ tibble(par = names(.), values = .)
      ) %>%
      map(
        ~ mutate(., par = ifelse(par == "time", "mu", "int"))
      ) %>%
      map(
        ~ pivot_wider(., names_from = "par", values_from = "values") %>%
          mutate(lambda = (1-mu)/(logN0 - int),
                 mu = mu*log(10)
                 )
      ) %>%
      imap_dfr(., ~ mutate(.x, temp = as.numeric(.y)))
    
    ## Guess for b
    
    Tmax <- max(quickprimary$temp, na.rm = TRUE)
    mumin <- min(quickprimary$mu, na.rm = TRUE)
    mumax <- max(quickprimary$mu, na.rm = TRUE)
    Tmin <- min(quickprimary$temp, na.rm = TRUE)
    
    b <- ( sqrt(mumax) - sqrt(mumin) )/(Tmax - Tmin)
    
    ## Guess for Tmin
    
    Tmin <- mean(quickprimary$temp - sqrt(quickprimary$mu)/b, na.rm = TRUE)
    
    ## Guess for logC0
    
    C0 <- mean(quickprimary$lambda * quickprimary$mu, na.rm = TRUE)
    
    ## Return
    
    c(Tmin = Tmin, b = b, logC0 = log10(C0), logN0 = logN0, logNmax = logNmax)
    
  } else {
    
    stop(paste0("Unknown mode: ", mode))
    
  }
  
}















