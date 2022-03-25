
#' Initial guesses for fitting primary growth models
#' 
#' The function uses some heuristics to provide initial guesses for the parameters
#' of the growth model selected.
#' 
#' @param fit_data the experimental data. A tibble (or data.frame) with a column
#' named `time` with the elapsed time and one called `logN` with the logarithm
#' of the population size
#' @param primary_model a string defining the equation of the primary model, 
#' as defined in [primary_model_data()]
#' 
#' @return A named numeric vector of initial guesses for the model parameters
#' 
#' @importFrom tibble tribble
#' 
#' @export
#' 
#' @examples 
#' 
#' ## An example of experimental data
#' 
#' my_data <- data.frame(time = 0:9, 
#'                       logN = c(2, 2.1, 1.8, 2.5, 3.1, 3.4, 4, 4.5, 4.8, 4.7))
#'                       
#' ## We just need to pass the data and the model equation
#' 
#' make_guess_primary(my_data, "Logistic")
#' 
#' ## We can use this together with fit_growth()
#' 
#' fit_growth(my_data,
#'            list(primary = "Logistic"),
#'            make_guess_primary(my_data, "Logistic"),
#'            c()
#'            )
#' 
#' ## The parameters returned by the function are adapted to the model
#' 
#' make_guess_primary(my_data, "Baranyi")
#' 
#' 
make_guess_primary <- function(fit_data, primary_model,
                               logbase = c("log10", "natural")) {
    
    ## Check that we know the model
    
    if ( ! (primary_model %in% primary_model_data()) ) {
        stop("Unkonwn model: ", primary_model)
    }
    
    ## Guess for logN0
    
    index_t0 <- which(fit_data$time == min(fit_data$time, na.rm = TRUE))[1]
    logN0 <- fit_data$logN[index_t0]
    
    ## Guess for lambda
    
    lambda <- min(fit_data$time[which(fit_data$time > logN0 + .5)], na.rm = TRUE)
    
    ## Gues for logNmax
    
    logNmax <- max(fit_data$logN, na.rm = TRUE)
    
    ## Guess for mu
    
    tmax <- min(fit_data$time[which(fit_data$logN == logNmax)], na.rm = TRUE)
    
    mu <- (logNmax - logN0)/(tmax - 0)
    
    ## Guess for C
    
    C <- logNmax - logN0
    
    ## Guess for nu
    
    nu <- 1
    
    ## return
    
    out <- list(logN0 = logN0, mu = mu, lambda = lambda, 
                logNmax = logNmax, C = C, nu = nu)
    
    par_map <- tribble(
        ~ par,  ~modGompertz, ~Baranyi, ~Trilinear, ~Logistic, ~Richards,
        "logN0", TRUE, TRUE, TRUE, TRUE, TRUE,
        "mu", TRUE, TRUE, TRUE, TRUE, TRUE,
        "lambda", TRUE, TRUE, TRUE, TRUE, TRUE,
        "logNmax", FALSE, TRUE, TRUE, FALSE, FALSE,
        "C", TRUE, FALSE, FALSE, TRUE, TRUE,
        "nu", FALSE, FALSE, FALSE, FALSE, TRUE
        )
    
    my_pars <- par_map$par[par_map[[primary_model]]]
    
    unlist(out[my_pars])
    
}















