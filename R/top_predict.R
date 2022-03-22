
#' Prediction of microbial growth
#' 
#' AA
#' 
#' @export
#' 
predict_growth <- function(times,
                           primary_model,
                           environment = c("constant", "dynamic"),
                           secondary_model = NULL,
                           env_conditions = NULL,
                           type = c("deterministic", "interval"),
                           n_sims = NULL,
                           par_corr = NULL,
                           ...,
                           check = TRUE,
                           logbase = c("natural", "10"),
                           formula = . ~ time
                           ) {
    
    ##
    
    if (environment == "constant") {  # Predictions under constant environmental conditions
        
        
        if (type == "deterministic") {  # Deterministic prediction
            
            ## Adapt the model parameters
            
            my_model <- primary_model$model
            my_pars <- primary_model
            my_pars$model <- NULL
            
            if (check) {
                
                if (is.null(my_model)) {
                    stop("primary model must include a 'model' entry")
                }
                
            }
            
            predict_isothermal_growth(my_model, times, my_pars, check = check)
            
            
        } else if (type == "interval") {  # Prediction with parameter uncertainty
            
        } else {  # Wrong input
            stop("type must be either 'deterministic' or 'interval', received: ", type)
        }
        
    } else if(environment == "dynamic") {  # Prediction under dynamic environmental conditions
        
    } else {  # Wrong input
        stop("environment must be 'constant' or 'dynamic', received: ", environment)
    }
    
    ##
    
}


















