
#' GrowthPrediction class
#' 
#' @description 
#' `r lifecycle::badge("stable")`
#' 
#' The `GrowthPrediction` class contains the results of a growth prediction
#' Its constructor is [predict_growth()].
#' 
#' It is a subclass of list with the items:
#' 
#' - simulation: a tibble with the model simulation
#' - primary model: a list describing the primary model as in [predict_growth()]
#' - environment: a character describing the type of environmental conditions 
#' as in [predict_growth()]
#' - env_conditions: a named list with the functions used to approximate the (dynamic)
#' environmental conditions. `NULL` if `environment="constant"`.
#' - sec_models: a named list describing the secondary models as in [predict_growth()].
#' `NULL` if `environment="constant"`.
#' - gammas: a tibble describing the variation of the gamma factors through the experiment.
#' - logbase_mu: the log-base for the definition of parameter mu (see the relevant vignette)
#' - logbase_logN: the log-base for the definition of the logarithm of the population size
#' 
#' @name GrowthPrediction
#'   
NULL

#' @describeIn GrowthPrediction print of the model
#' 
#' @param x An instance of `GrowthPrediction`.
#' @param ... ignored
#' 
#' @export
#' 
print.GrowthPrediction <- function(x, ...) {
    
    
    if (x$environment == "constant") {
        
        cat("Growth prediction based on primary models\n\n")
        
        cat(paste("Growth model:", x$primary_model$model, "\n\n"))
        
        cat("Parameters of the primary model:\n")
        print(coef(x))
        
        logbase <- x$logbase_mu
        
        if ( abs(logbase - exp(1)) < .1 ) {
            logbase <- "e"
        }
        cat("\n")
        cat(paste0("Parameter mu defined in log-", logbase, " scale"))
        
        logbase <- x$logbase_logN
        
        if ( abs(logbase - exp(1)) < .1 ) {
            logbase <- "e"
        }
        cat("\n")
        cat(paste0("Population size defined in log-", logbase, " scale\n"))
        
    } else if (x$environment == "dynamic") {
        
        cat("Growth prediction under dynamic environmental conditions\n\n")
        
        env <- names(x$env_conditions)
        cat(paste("Environmental factors included:", paste(env, collapse = ", "), "\n\n"))
        
        cat("Parameters of the Baranyi primary model:\n")
        print(unlist(x$primary_model))
        cat("\n")
        
        logbase <- x$logbase_mu
        
        if ( abs(logbase - exp(1)) < .1 ) {
            logbase <- "e"
        }
        cat(paste0("Parameter mu defined in log-", logbase, " scale\n"))
        
        logbase <- x$logbase_logN
        
        if ( abs(logbase - exp(1)) < .1 ) {
            logbase <- "e"
        }
        cat("\n")
        cat(paste0("Population size defined in log-", logbase, " scale\n\n"))
        
        for (i in 1:length(x$sec_models)) {
            cat(paste("Secondary model for ", names(x$sec_models)[i], ":\n", sep = ""))
            print(unlist(x$sec_models[[i]]))
            cat("\n")
        }
        
    }

}

#' @describeIn GrowthPrediction summary of the model
#' 
#' @param object An instance of `GrowthPrediction`.
#' @param ... ignored
#' 
#' @export
#' 
summary.GrowthPrediction <- function(object, ...) {
    
    
    if (object$environment == "constant") {
        
        cat("Growth prediction based on primary models\n\n")
        
        cat(paste("Growth model:", object$primary_model$model, "\n\n"))
        
        cat("Parameters of the primary model:\n")
        print(coef(object))
        
        logbase <- object$logbase_mu
        
        if ( abs(logbase - exp(1)) < .1 ) {
            logbase <- "e"
        }
        cat("\n")
        cat(paste0("Parameter mu defined in log-", logbase, " scale"))
        
        logbase <- object$logbase_logN
        
        if ( abs(logbase - exp(1)) < .1 ) {
            logbase <- "e"
        }
        cat("\n")
        cat(paste0("Population size defined in log-", logbase, " scale\n"))
        
        cat("\n")
        cat(paste0("Maximum elapsed time: ", max(object$simulation$time, na.rm=TRUE), "\n"))
        cat(paste0("Maximum population size: ", max(object$simulation$logN, na.rm=TRUE), "\n"))
        
    } else if (object$environment == "dynamic") {
        
        cat("Growth prediction under dynamic environmental conditions\n\n")
        
        env <- names(object$env_conditions)
        cat(paste("Environmental factors included:", paste(env, collapse = ", "), "\n\n"))
        
        cat("Parameters of the Baranyi primary model:\n")
        print(unlist(object$primary_model))
        cat("\n")
        
        logbase <- object$logbase_mu
        
        if ( abs(logbase - exp(1)) < .1 ) {
            logbase <- "e"
        }
        cat(paste0("Parameter mu defined in log-", logbase, " scale\n"))
        
        logbase <- object$logbase_logN
        
        if ( abs(logbase - exp(1)) < .1 ) {
            logbase <- "e"
        }
        cat("\n")
        cat(paste0("Population size defined in log-", logbase, " scale\n\n"))
        
        for (i in 1:length(object$sec_models)) {
            cat(paste("Secondary model for ", names(object$sec_models)[i], ":\n", sep = ""))
            print(unlist(object$sec_models[[i]]))
            cat("\n")
        }
        
        cat("\n")
        cat(paste0("Maximum elapsed time: ", max(object$simulation$time, na.rm=TRUE), "\n"))
        cat(paste0("Maximum population size: ", max(object$simulation$logN, na.rm=TRUE), "\n"))
        
    }
    
}

#' @describeIn GrowthPrediction predicted growth curve.
#'
#' @param x The object of class `GrowthPrediction` to plot.
#' @param y ignored
#' @param ... ignored
#' @param add_factor whether to plot also one environmental factor.
#' If `NULL` (default), no environmental factor is plotted. If set
#' to one character string that matches one entry of x$env_conditions,
#' that condition is plotted in the secondary axis. Ignored for `environment="constant"`.
#' @param ylims A two dimensional vector with the limits of the primary y-axis.
#' @param label_y1 Label of the primary y-axis.
#' @param label_y2 Label of the secondary y-axis.
#' @param line_col Aesthetic parameter to change the colour of the line geom in the plot, see: [geom_line()]
#' @param line_size Aesthetic parameter to change the thickness of the line geom in the plot, see: [geom_line()]
#' @param line_type Aesthetic parameter to change the type of the line geom in the plot, takes numbers (1-6) or strings ("solid") see: [geom_line()]
#' @param line_col2 Same as lin_col, but for the environmental factor.
#' @param line_size2 Same as line_size, but for the environmental factor.
#' @param line_type2 Same as lin_type, but for the environmental factor.
#' @param label_x Label of the x-axis.
#'
#' @export
#'
plot.GrowthPrediction <- function(x, y=NULL, ...,
                               add_factor = NULL,
                               ylims = NULL,
                               label_y1 = NULL,
                               label_y2 = add_factor,
                               line_col = "black",
                               line_size = 1,
                               line_type = "solid",
                               line_col2 = "black",
                               line_size2 = 1,
                               line_type2 = "dashed",
                               label_x = "time"
) {
    
    ## Get the label for the y-axis
    
    logbase <- x$logbase_logN
    
    if ( abs(logbase - exp(1)) < .1 ) {
        logbase <- "e"
    }
    
    if (is.null(label_y1)) {
        label_y1 <- paste0("logN (in log-", logbase, ")")
    } else {
        label_y1 <- label_y1
    }
    
    switch(x$environment,
           constant = plot.IsothermalGrowth(x,
                                            line_col = line_col,
                                            line_size = line_size,
                                            line_type = line_type,
                                            ylims = ylims,
                                            label_y = label_y1,
                                            label_x = label_x
                                            ),
           dynamic = plot.DynamicGrowth(x,
                                        add_factor = add_factor,
                                        ylims = ylims,
                                        label_y1 = label_y1,
                                        label_y2 = label_y2,
                                        line_col = line_col,
                                        line_size = line_size,
                                        line_type = line_type,
                                        line_col2 = line_col2,
                                        line_size2 = line_size2,
                                        line_type2 = line_type2,
                                        label_x = label_x
                                        )
    )
    
    
}

#' @describeIn GrowthPrediction coefficients of the model
#' 
#' @param object an instance of [GrowthPrediction]
#' @param ... ignored
#' 
#' @export
#' 
coef.GrowthPrediction <- function(object, ...) {
    
    if (object$environment == "constant") {
        
        out <- object$primary_model
        out$model <- NULL
        unlist(out)
        
    } else if (object$environment == "dynamic") {
        
        list(
            primary = object$primary_model,
            secondary = object$sec_models
        )
        
    }

}
















