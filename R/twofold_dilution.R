
#' Estimation of the Time to Detection of OD measurements
#' 
#' The function uses linear interpolation to identify the time at which different
#' wells reached a target optical density
#' 
#' @param OD_data a tibble (or data.frame) with the readings of the equipment. It 
#' must have a column named `time` with the time of the rading and as many additional 
#' columns as conditions
#' @param target_OD target OD for the calculation of the TTD
#' @param codified whether the columns are codified. If `FALSE` (default), the TTD estimated
#' for each condition is returned as such. If `TRUE`, it is assumed that each column
#' is codified as `condition_number-of-dilutions`. Therefore, the results are separated
#' to simplify the application of [fit_serial_dilution()]
#' 
#' @importFrom tidyr separate
#' 
#' @returns A tibble with two or three columns. If `codified = FALSE`, the tibble has two columns:
#' `condition` (the name of the well according to `OD_data`) and `TTD` (the estimated time to detection).
#' If the `target_OD` was not reached for some well, it assigns `NA`. If `codified = TRUE`, 
#' the code returns an additional column with the number of dilutions
#' 
#' @export
#' 
#' @examples
#' data("example_od")  # example dataset included int he package
#' 
#' get_TTDs(example_od, target_OD = 0.2)  # default behaviour, returns two columns
#' get_TTDs(example_od, target_OD = 0.2, codified = TRUE)  # extracts also the number of dilutions
#' 
#' 
get_TTDs <- function(OD_data, target_OD, codified = FALSE) {
  
  aa <- OD_data %>%
    pivot_longer(-c("time"), names_to = "cond", values_to = "od")
    
  TTDs <- split(aa, aa$cond) %>%
    imap_dfr(
             ~ tibble(condition = .y,
                      TTD = approx(x = .x$od, y = .x$time, xout = target_OD)$y
             )
    ) 
  
  if (codified) {
    TTDs <- TTDs %>%
      separate("condition", into = c("condition", "dil"), sep = "_") %>%
      mutate(dil = as.numeric(.data$dil))
  }
  
  TTDs

  
}

#' Serial-fold dilution method
#' 
#' Model fitting by the serial-fold dilution method
#' 
#' @importFrom stats nls
#' 
#' @param TTD_data a tibble (or data.frame) with the TTD observed for different dilutions.
#' It must have two columns: `TTD` (the TTD) and `dil` the number of serial dilutions.
#' @param mode one of "intercept" (serial dilution method with a generic intercept; default) 
#' or "lambda" (able to estimate also the value of the lag phase duration)
#' @param dil_factor dilution factor. By default, 2
#' @param logN_det log10 microbial concentration at the detection OD (only for mode = "lambda")
#' @param logN_dil0 log10 microbial concentration at wells where dilution = 0 (only for mode = "lambda")
#' @param max_dil maximum number of dilutions to include. By default, `NULL` (no limit)
#' @param start named numeric vector of initial guesses for the model parameters
#'
#' @export
#' 
#' @examples
#' ## We can use the example data set
#' 
#' data("example_od")
#' 
#' ## We first need to estimate the TTDs
#' 
#' library(tidyverse)
#' 
#' my_TTDs <- get_TTDs(example_od, target_OD = 0.2, codified = TRUE)
#' my_data <- filter(my_TTDs, condition == "S/6,5/35/R1")
#' 
#' ## Fitting using the "intercept" mode
#' 
#' guess <- c(a = 0, mu = .1)  # we need initial guesses for the model parameters
#' 
#' my_fit <- fit_serial_dilution(my_data, start = guess)
#' 
#' ## The class returned implements common S3 methods
#' 
#' my_fit
#' summary(my_fit)
#' plot(my_fit)
#' 
#' ## The fitting can define a maximum number of dilutions
#' 
#' my_fit <- fit_serial_dilution(my_data, start = guess, max_dil = 4)
#' plot(my_fit)
#' 
#' ## Fitting using the "lambda" mode
#' 
#' logNdet <- 7.5  # this mode requires the microbial concentration at the detection OD
#' logN_dil0 <- 4  # and the concentration at the well with dilution 0 
#' guess <- c(lambda = 0, mu = .1)  # the guess must be defined now on lambda instead of a
#' 
#' my_fit2 <- fit_serial_dilution(my_data, 
#'                                start = guess,
#'                                mode = "lambda", 
#'                                logN_det = logNdet,
#'                                logN_dil0 = logN_dil0)
#' 
#' ## The instance implements the same S3 methods as before
#' 
#' my_fit2
#' summary(my_fit2)
#' plot(my_fit2)
#' 
#' 
fit_serial_dilution <- function(TTD_data,
                                start,
                                dil_factor = 2,
                                mode = "intercept",
                                logN_det = NULL,
                                logN_dil0 = NULL,
                                max_dil = NULL) {
  
  if (!is.null(max_dil)) {
    TTD_data <- filter(TTD_data, .data$dil <= max_dil)
  }
  
  
  if (mode == "intercept") {
    
    ## Check the initial guess
    
    if (length(start) != 2) stop("The initial guess must have two elements named a and mu")
    
    if ( !("mu" %in% names(start)) ) stop("An initial guess of mu must be defined")
    if ( !("a" %in% names(start)) ) stop("An initial guess of a (the intercept) must be defined")
    
    ## Fit the model
    
    aa <- TTD_data %>%
      mutate(x = log10(dil_factor) * .data$dil) %>%
      filter(!is.na(.data$TTD), is.finite(.data$TTD))
    
    fit <- nls(TTD ~ a + x/mu, data = aa,
               start = start
               )
    
  } else if (mode == "lambda") {
    
    ## Check the initial guess
    
    if (length(start) != 2) stop("The initial guess must have two elements named lambda and mu")
    if ( !("mu" %in% names(start)) ) stop("An initial guess of mu must be defined")
    if ( !("lambda" %in% names(start)) ) stop("An initial guess of lambda must be defined")
    
    ## Check the additional pars are provided
    
    if (is.null(logN_det)) stop("logN_det must be defined for mode = 'lambda'")
    if (is.null(logN_dil0)) stop("logN_dil0 must be defined for mode = 'lambda'")
    
    ## Fit the model
    
    aa <- TTD_data %>%
      mutate(x = log10(dil_factor)*.data$dil) %>%
      filter(!is.na(.data$TTD), is.finite(.data$TTD))
    
    fit <- nls(TTD ~ (lambda + logN_det/mu - logN_dil0/mu) + x/mu, 
               data = aa,
               start = start
               )
    
  } else {
    stop(paste("Unknown mode:", mode))
  }
  
  ## Output
  
  out <- list(
    fit = fit,
    mode = mode,
    data = TTD_data
  )
  
  class(out) <- c("FitSerial", class(out))
  
  out
  
}





