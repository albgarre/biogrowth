
#' SecondaryGrowthComparison class
#' 
#' @description 
#' The `SecondaryGrowthComparison` class contains several functions for model comparison
#' and model selection of growth models. It should not be instanced directly. Instead,
#' it should be constructed using [compare_secondary_fits()].
#' 
#' It includes two type of tools for model selection and comparison: statistical indexes
#' and visual analyses. Please check the sections below for details.
#' 
#' Note that all these tools use the names defined in [compare_secondary_fits()], so 
#' we recommend passing a named list to that function.
#' 
#' @section Statistical indexes:
#' `SecondaryGrowthComparison` implements two S3 methods to obtain numerical values to facilitate
#' model comparison and selection. 
#' - the `coef` method returns a tibble with the values of the parameter estimates
#' and their corresponding standard errors for each model. 
#' - the `summary` returns a tibble with the AIC, number of degrees of freedom,
#' mean error and root mean squared error for each model. 
#' 
#' @section Visual analyses:
#' The S3 plot method can generate three types of plots:
#' - when `type = 1`, the plot compares the observations against the model predictions
#' for each model. The plot includes a linear model fitted to the residuals. In the case 
#' of a perfect fit, the line would have slope=1 and intercept=0 (shown as a black, dashed line).
#' - when `type = 2`, the plot compares the parameter estimates using error bars, where
#' the limits of the error bars are the expected value +/- one standard error. In case
#' one model does not has some model parameter (i.e. either because it is not defined
#' or because it was fixed), the parameter is not included in the plot.
#' 
#' @name SecondaryGrowthComparison
#'   
NULL

#' @describeIn SecondaryGrowthComparison table of parameter estimates
#' 
#' @param object an instance of SecondaryGrowthComparison
#' @param ... ignored
#' 
#' @export
#' 
coef.SecondaryGrowthComparison <- function(object, ...) {
    
    object$models %>%
        map(~ summary(.)$par) %>%
        map(~ as_tibble(., rownames = "parameter")) %>%
        imap_dfr(~ mutate(.x, model = as.character(.y))
        ) %>%
        select("model", "parameter", estimate = "Estimate", std.err = "Std. Error")

    
}

#' @describeIn SecondaryGrowthComparison summary table for the comparison
#' 
#' @param object an instance of SecondaryGrowthComparison
#' @param ... ignored
#' 
#' @importFrom dplyr arrange left_join
#' @importFrom stats AIC
#' 
#' @export
#' 
summary.SecondaryGrowthComparison <- function(object, ...) {
    
    index_table <- object$models %>%
        imap_dfr(~ tibble(model = .y,
                          AIC = AIC(.x),
                          df = .x$fit_results$df.residual))
    
    res_table <- object$models %>%
        map(~ tibble(res = residuals(.))) %>%
        map(~ mutate(., res2 = res^2)) %>%
        map(~ summarize(.,
                        ME = mean(res),
                        RMSE = sqrt(mean(res2))
        )) %>%
        imap_dfr(~ mutate(.x, model = .y))
    
    left_join(index_table, res_table, by = "model") %>%
        arrange(AIC)
    
}

#' @describeIn SecondaryGrowthComparison print of the model comparison
#' 
#' @param x an instance of SecondaryGrowthComparison
#' @param ... ignored
#' 
#' @export
#' 
print.SecondaryGrowthComparison <- function(x, ...) {
    
    cat("Comparison between secondary growth models fitted to a dataset of growth rates\n\n")
    
    index_table <- summary(x)
    
    cat("Statistical indexes arranged by AIC:\n\n")
    print(index_table)
    
}

#' @describeIn SecondaryGrowthComparison illustrations comparing the fitted models
#' 
#' @param x an instance of SecondaryGrowthComparison
#' @param y ignored
#' @param ... ignored
#' @param type if type==1, the plot compares the model predictions. If type ==2,
#' the plot compares the parameter estimates.
#' 
#' @importFrom tibble tibble
#' @importFrom purrr imap_dfr
#' @importFrom ggplot2 ggplot geom_line geom_point aes_string geom_errorbar facet_wrap geom_hline
#' @importFrom dplyr select
#' 
#' @export
#' 
plot.SecondaryGrowthComparison <- function(x, y, ...,
                                  type = 1,
                                  add_trend = TRUE) {
    
    if (type == 1) {  # Plot of the predictions
        
        x$models %>%
            map(~plot(.)) %>%
            map(~.$data) %>%
            imap_dfr(~ mutate(.x, model = .y)) %>%
            ggplot(aes_string(x = "observed", y = "predicted", colour = "model")) +
            geom_point() +
            geom_smooth(method = "lm", se=FALSE) +
            geom_abline(slope = 1, intercept = 0, linetype = 2)
        
    } else if (type == 2) {  # Plot of the parameter estimates
        
        coef(x) %>%
            ggplot(aes_string(x = "model", y = "estimate")) +
            geom_point() +
            geom_errorbar(aes_string(ymin = "estimate - std.err", 
                                     ymax = "estimate + std.err")) +
            facet_wrap("parameter", scales = "free_y")
        
    } else {
        stop("type must be 1 or 2, got ", type )
    }
}
