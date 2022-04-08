
#' GlobalGrowthComparison class
#' 
#' @description 
#' The `GlobalGrowthComparison` class contains several functions for model comparison
#' and model selection of growth models. It should not be instanced directly. Instead,
#' it should be constructed using [compare_growth_fits()]. It is similar to 
#' [GrowthComparison], although with specific tools to deal with several experiments.
#' 
#' It includes two type of tools for model selection and comparison: statistical indexes
#' and visual analyses. Please check the sections below for details.
#' 
#' Note that all these tools use the names defined in [compare_growth_fits()], so 
#' we recommend passing a named list to that function.
#' 
#' @section Statistical indexes:
#' `GlobalGrowthComparison` implements two S3 methods to obtain numerical values to facilitate
#' model comparison and selection. 
#' - the `coef` method returns a tibble with the values of the parameter estimates
#' and their corresponding standard errors for each model. 
#' - the `summary` returns a tibble with the AIC, number of degrees of freedom,
#' mean error and root mean squared error for each model. 
#' 
#' @section Visual analyses:
#' The S3 plot method can generate three types of plots:
#' - when `type = 1`, the plot compares the fitted growth curves against the
#' experimental data used to fit the model. 
#' - when `type = 2`, the plot compares the parameter estimates using error bars, where
#' the limits of the error bars are the expected value +/- one standard error. In case
#' one model does not has some model parameter (i.e. either because it is not defined
#' or because it was fixed), the parameter is not included in the plot.
#' - when `type=3`, the plot shows the tendency of the residuals for each model. This
#' plot can be used to detect deviations from independence.
#' 
#' These plots are divided by facets for each experiment.
#' 
#' @name GlobalGrowthComparison
#'   
NULL

#' @describeIn GlobalGrowthComparison table of parameter estimates
#' 
#' @param object an instance of GlobalGrowthComparison
#' @param ... ignored
#' 
#' @importFrom tidyr pivot_longer pivot_wider
#' 
#' @export
#' 
coef.GlobalGrowthComparison <- function(object, ...) {
    
    if (object$algorithm == "regression") {
        
        object$models %>%
            map(~ summary(.)$par) %>%
            map(~ as_tibble(., rownames = "parameter")) %>%
            imap_dfr(~ mutate(.x, model = as.character(.y))
            ) %>%
            select("model", "parameter", estimate = "Estimate", std.err = "Std. Error")
        
    } else if (object$algorithm == "MCMC") {
        
        object %>% summary() %>%
            as_tibble(rownames = "index") %>%
            pivot_longer(-"index") %>%
            pivot_wider(values_from = "value", names_from = "index")

        object$models %>%
            map(~ summary(.)) %>%
            map(~ as_tibble(., rownames = "index")) %>%
            map(~ pivot_longer(., -"index")) %>%
            map(~ pivot_wider(., values_from = "value", names_from = "index")) %>%
            imap_dfr(~ mutate(.x, model = .y)) %>%
            select("model", parameter = "name", estimate = "mean", std.err = "sd")
        
    } else {
        stop("Something very bad happened here here")
    }
    
}

#' @describeIn GlobalGrowthComparison summary table for the comparison
#' 
#' @param object an instance of GlobalGrowthComparison
#' @param ... ignored
#' 
#' @importFrom dplyr arrange left_join
#' @importFrom stats AIC
#' 
#' @export
#' 
summary.GlobalGrowthComparison <- function(object, ...) {
    
    index_table <- object$models %>%
        imap_dfr(~ tibble(model = .y,
                          AIC = AIC(.x),
                          df = .x$fit$df.residual))
    
    res_table <- object$residuals %>%
        map(~ mutate(., res2 = res^2)) %>%
        map(~ summarize(.,
                        ME = mean(res),
                        RMSE = sqrt(mean(res2))
        )) %>%
        imap_dfr(~ mutate(.x, model = .y))
    
    left_join(index_table, res_table, by = "model") %>%
        arrange(AIC)
    
}

#' @describeIn GlobalGrowthComparison print of the model comparison
#' 
#' @param x an instance of GlobalGrowthComparison
#' @param ... ignored
#' 
#' @export
#' 
print.GlobalGrowthComparison <- function(x, ...) {
    
    cat("Comparison between models fitted using a global approach\n\n")
    
    index_table <- summary(x)
    
    cat("Statistical indexes arranged by AIC:\n\n")
    print(index_table)
    
}

#' @describeIn GlobalGrowthComparison illustrations comparing the fitted models
#' 
#' @param x an instance of GlobalGrowthComparison
#' @param y ignored
#' @param ... ignored
#' @param type if type==1, the plot compares the model predictions. If type ==2,
#' the plot compares the parameter estimates. If type==3, the plot shows the residuals
#' @param add_trend should a trend line of the residuals be added for type==3? `TRUE`
#' by default
#' 
#' @importFrom tibble tibble
#' @importFrom purrr imap_dfr
#' @importFrom ggplot2 ggplot geom_line geom_point aes_string geom_errorbar facet_wrap geom_hline
#' @importFrom dplyr select
#' 
#' @export
#' 
plot.GlobalGrowthComparison <- function(x, y, ...,
                                  type = 1,
                                  add_trend = TRUE) {
    
    if (type == 1) {  # Plot of the predictions
        
        p <- x$models %>%
            map(get_all_predictions) %>%
            imap_dfr(~ mutate(.x, model = as.character(.y))) %>%
            ggplot() +
            geom_line(aes_string(x = "time", y = "logN", colour = "model")) +
            facet_wrap("experiment", scales = "free")
        
        d <- x$models[[1]]$data %>%
            map(~ .$data) %>%
            imap_dfr(~ mutate(.x, experiment = .y))
        
        my_points <- geom_point(aes_string(x = "time", y = "logN"), 
                                data = d, inherit.aes = FALSE)
        
        # my_points <- x$models[[1]]$data %>%
        #     map(~ .$data) %>%
        #     imap_dfr(~ mutate(.x, experiment = .y)) %>%
        #     geom_point(aes_string(x = "time", y = "logN"), data = ., inherit.aes = FALSE)
        
        p + my_points
    
    } else if (type == 2) {  # Plot of the parameter estimates
        
        coef(x) %>%
            ggplot(aes_string(x = "model", y = "estimate")) +
            geom_point() +
            geom_errorbar(aes_string(ymin = "estimate - std.err", 
                                     ymax = "estimate + std.err")) +
            facet_wrap("parameter", scales = "free_y")
        
    } else if (type == 3) {  # Plot of the residuals
        
        p <- x$residuals %>%
            imap_dfr(~ mutate(.x, model = as.character(.y))) %>%
            ggplot(aes_string(x = "time", y = "res", colour = "model")) +
            geom_point() +
            facet_wrap("exp")

        if (add_trend) {
            p <- p + geom_smooth(se = FALSE, method = "loess")
        }
        
        p + geom_hline(yintercept = 0, linetype = 2)
        
    } else {
        stop("type must be 1, 2 or 3, got ", type )
    }
}



#' A helper for making the plots
#' 
#' @param model An instance of FitMultipleDynamicGrowth
#' 
get_all_predictions <- function(model) {
    
    model$data %>%
        map(~ tibble(time = seq(0, max(.$data$time, na.rm = TRUE), length = 100),
                     logN = predict(model, 
                                    times = seq(0, max(.$data$time, na.rm = TRUE), length = 100),
                                    env_conditions = .x$conditions
                                    )
                     )
            ) %>%
        imap_dfr(~ mutate(.x, experiment = .y))
    
}
































