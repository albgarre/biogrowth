
#' ComparisonIsoGrowth class
#' 
#' @description 
#' The `ComparisonIsoGrowth` class contains AA
#' 
#' @name ComparisonIsoGrowth
#'   
NULL

#' @describeIn ComparisonIsoGrowth illustrations comparing the fitted models
#' 
#' @param x an instance of ComparisonIsoGrowth
#' @param y ignored
#' @param ... ignored
#' @param type if type==1, the plot compares the model predictions. If type ==2,
#' the plot compares the parameter estimates. If type==3, the plot shows the residuals
#' 
#' @importFrom tibble tibble
#' @importFrom purrr imap_dfr
#' @importFrom ggplot2 ggplot geom_line geom_point aes geom_errorbar facet_wrap geom_hline
#' @importFrom dplyr select
#' 
#' @export
#' 
plot.ComparisonIsoGrowth <- function(x, y, ...,
                                     type = 1,
                                     add_trend = TRUE) {
    
    if (type == 1) {  # Plot of the predictions
        
        d <- x$models[[1]]$data
        
        x$models %>%
            map(~ tibble(time = seq(0, max(d$time, na.rm = TRUE), length =1000),
                         logN = predict(., times = time)
                )
            ) %>%
            imap_dfr(~ mutate(.x, model = as.character(.y))
            ) %>%
            ggplot() +
            geom_line(aes(x = time, y = logN, colour = model)) +
            geom_point(aes(x = time, y = logN), data = d)
        
    } else if (type == 2) {  # Plot of the parameter estimates
        
        coef(x) %>%
            ggplot(aes(x = model, y = estimate)) +
            geom_point() +
            geom_errorbar(aes(ymin = estimate - std.err, ymax = estimate + std.err)) +
            facet_wrap("parameter", scales = "free_y")
        
    } else if (type == 3) {  # Plot of the residuals
        
        p <- x$residuals %>%
            map(~ .$residuals) %>%
            imap_dfr(~ mutate(.x, model = as.character(.y))) %>%
            ggplot(aes(x = x, y = res, colour = model)) +
            geom_point()
        
        if (add_trend) {
            p <- p + geom_smooth(se = FALSE)
        }
        
        p + geom_hline(yintercept = 0, linetype = 2)
        
    } else {
        stop("type must be 1, 2 or 3, got ", type )
    }
}

#' @describeIn ComparisonIsoGrowth table of parameter estimates
#' 
#' @param object an instance of ComparisonIsoGrowth
#' @param ... ignored
#' 
#' @export
#' 
coef.ComparisonIsoGrowth <- function(object, ...) {
    
    object$models %>%
        map(~ summary(.)$par) %>%
        map(~ as_tibble(., rownames = "parameter")) %>%
        imap_dfr(~ mutate(.x, model = as.character(.y))
        ) %>%
        select("model", "parameter", estimate = "Estimate", std.err = "Std. Error")
    
}

#' @describeIn ComparisonIsoGrowth print of the model comparison
#' 
#' @param x an instance of ComparisonIsoGrowth
#' @param ... ignored
#' 
#' @export
#' 
print.ComparisonIsoGrowth <- function(x, ..) {
    
    cat("Comparison between models fitted to data under isothermal conditions\n\n")
    
    index_table <- summary(x)
    
    cat("Statistical indexes arranged by AIC:\n\n")
    print(index_table)
    
}


#' @describeIn ComparisonIsoGrowth summary table for the comparison
#' 
#' @param object an instance of ComparisonIsoGrowth
#' @param ... ignored
#' 
#' @importFrom dplyr arrange left_join
#' 
#' @export
#' 
summary.ComparisonIsoGrowth <- function(object, ...) {
    
    index_table <- object$models %>%
        imap_dfr(~ tibble(model = .y,
                          AIC = AIC(.x),
                          df = .x$fit$df.residual))
    
    res_table <- object$residuals %>%
        map(~ .$residuals) %>%
        map(~ mutate(., res2 = res^2)) %>%
        map(~ summarize(.,
                        ME = mean(res),
                        RMSE = sqrt(mean(res2))
                        )) %>%
        imap_dfr(~ mutate(.x, model = .y))
    
    left_join(index_table, res_table, by = "model") %>%
        arrange(AIC)
    
}




