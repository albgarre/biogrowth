
#' FitMultipleGrowthMCMC class
#' 
#' @description 
#' The \code{FitMultipleGrowthMCMC} class contains a model fitted to a set of dynamic
#' experiments using an MCMC algorithm. Its constructor is 
#' \code{\link{fit_multiple_growth_MCMC}}.
#' 
#' It is a subclass of list with the items:
#'      \itemize{
#'          \item fit_results: the object returned by \code{modFit}.
#'          \item best_prediction: a list with the models predictions for each condition.
#'          \item data: a list with the data used for the fit.
#'          \item starting: starting values for model fitting
#'          \item known: parameter values set as known.
#'          \item sec_models: a named vector with the secondary model
#'          for each environmental factor.
#'          }
#' 
#' @name FitMultipleGrowthMCMC
#'   
NULL

#' @describeIn FitMultipleGrowthMCMC comparison between the model fitted and the
#' data.
#'
#' @inheritParams plot.FitMultipleDynamicGrowth
#' @param x an instance of FitMultipleGrowthMCMC.
#'
#' @export
#'
plot.FitMultipleGrowthMCMC <- function(x, y=NULL, ...,
                                       add_factor = NULL,
                                       ylims = NULL,
                                       label_x = "time",
                                       label_y1 = "logN",
                                       label_y2 = add_factor,
                                       line_col = "black",
                                       line_size = 1,
                                       line_type = "solid",
                                       line_col2 = "black",
                                       line_size2 = 1,
                                       line_type2 = "dashed",
                                       point_size = 3,
                                       point_shape = 16,
                                       subplot_labels = "AUTO"
) {
    
    plot.FitMultipleDynamicGrowth(x,
                                  add_factor = add_factor,
                                  label_x = label_x,
                                  ylims = ylims,
                                  label_y1 = label_y1,
                                  label_y2 = label_y2,
                                  line_col = line_col,
                                  line_size = line_size,
                                  line_type = line_type,
                                  line_col2 = line_col2,
                                  line_size2 = line_size2,
                                  line_type2 = line_type2,
                                  point_size = point_size,
                                  point_shape = point_shape,
                                  subplot_labels = subplot_labels
    )
    
}
