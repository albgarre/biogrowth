
#' Generic for calculating predictions with uncertainty from fits
#' 
#' @param model An instance of FitCoupledGrowth
#' @param newdata a tibble (or data.frame) with two columns (time and temperature) 
#' for the prediction. By default, `NULL` (the fitting conditions)
#' @param niter number of MC simulations
#' @param includecorr whether to include parameter correlation (`TRUE` by default)
#' 
#' @export
#' 
predictMCMC_coupled <- function(model,
                                niter,
                                newdata = NULL,
                                includecorr = TRUE
) {
  UseMethod("predictMCMC_coupled", model)
}

#' @describeIn FitCoupledGrowth prediction including parameter uncertainty
#' 
#' @importFrom mvtnorm rmvnorm
#' 
#' @export
#' 
#' 
predictMCMC_coupled.FitCoupledGrowth <- function(model,
                                                 niter,
                                                 newdata = NULL,
                                                 includecorr = TRUE
) {
  
  if (model$mode != "one_step") stop("MCMC prediction only implemented for one-step models")
  
  ## Check the correlation
  
  if (includecorr) {
    v <- vcov(model)
  } else {
    v <- diag(summary(model)$par[,"Std. Error"]^2)
  }
  
  ## Get the parameter sample
  
  sample <- rmvnorm(n = niter, 
                    mean = coef(model), 
                    sigma = v) %>%
    as_tibble() %>%
    mutate(i = row_number())
  
  ## Add the fixed parameters
  
  if ( length(model$known) > 0 ) {
    
    for (i in 1:length(model$known)) {
      
      sample[[ names(model$known)[i] ]] <- model$known[i]
    }
    
  }
  
  ## Get the standard error of the residuals
  
  sd_res <- sqrt(model$fit$ssr/model$fit$df.residual)
  
  ## Make the simulations
  
  if (is.null(newdata)) {
    newdata <- select(model$data, -"logN")
  }
  
  sims <- split(sample, sample$i) %>%
    map(.,
        ~ mutate(newdata,
                 logN = pred_coupled_baranyi(as.list(.), temp, time)
        ) %>%
          mutate(eps = rnorm(nrow(.), mean = 0, sd = sd_res),
                 logN_pred = logN + eps)
    ) %>%
    imap_dfr(., ~ mutate(.x, i = .y))
  
  ## Summarize the simulations
  
  quants <- sims %>%
    summarize(med_logN = median(logN),
              q10 = quantile(logN, .1),
              q90 = quantile(logN, .9),
              q10_pred = quantile(logN_pred, .1),
              q90_pred = quantile(logN_pred, .9),
              .by = c("time", "temp"))
  
  ## Return
  
  out <- list(
    quantiles = quants,
    par_sample = sample,
    simulations = sims,
    sd_res = sd_res
  )
  
  class(out) <- c("MCMCcoupled", class(out))
  
  out
  
}




