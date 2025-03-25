
#' Plot of the initial guess for the Baranyi-Ratkowsky model
#' 
#' Compares the prediction corresponding to a guess of the parameters of the Baranyi-Ratkowsky
#' model against experimental data
#' 
#' @param fit_data Tibble (or data.frame) of data for the fit. It must have two columns, one with
#' the elapsed time (`time` by default) and another one with the decimal logarithm
#' of the populatoin size (`logN` by default). Different column names can be
#' defined using the `formula` argument. 
#' @param model_name Character defining the primary growth model as per [primary_model_data()] 
#' @param guess Named vector with the initial guess of the model parameters
#' @param formula an object of class "formula" describing the x and y variables.
#' `logN ~ time` as a default.
#' @param logbase_mu Base of the logarithm the growth rate is referred to. 
#' By default, 10 (i.e. log10). See vignette about units for details. 
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



