
## "is." methods

#' Test of FitDynamicGrowth object
#'
#' Tests if an object is of class \code{FitDynamicGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{FitDynamicGrowth}
#'
#' @export
#'
is.FitDynamicGrowth <- function(x) inherits(x, "FitDynamicGrowth")

#' Test of FitDynamicGrowthMCMC object
#'
#' Tests if an object is of class \code{FitDynamicGrowthMCMC}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{FitDynamicGrowthMCMC}
#'
#' @export
#'
is.FitDynamicGrowthMCMC <- function(x) inherits(x, "FitDynamicGrowthMCMC")

#' Test of FitIsoGrowth object
#'
#' Tests if an object is of class \code{FitIsoGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{FitIsoGrowth}
#'
#' @export
#'
is.FitIsoGrowth <- function(x) inherits(x, "FitIsoGrowth")

#' Test of FitSecondaryGrowth object
#'
#' Tests if an object is of class \code{FitSecondaryGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{FitSecondaryGrowth}
#'
#' @export
#'
is.FitSecondaryGrowth <- function(x) inherits(x, "FitSecondaryGrowth")

#' Test of DynamicGrowth object
#'
#' Tests if an object is of class \code{DynamicGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{DynamicGrowth}
#'
#' @export
#'
is.DynamicGrowth <- function(x) inherits(x, "DynamicGrowth")

#' Test of IsothermalGrowth object
#'
#' Tests if an object is of class \code{IsothermalGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{IsothermalGrowth}
#'
#' @export
#'
is.IsothermalGrowth <- function(x) inherits(x, "IsothermalGrowth")

#' Test of MCMCgrowth object
#'
#' Tests if an object is of class \code{MCMCgrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{MCMCgrowth}
#'
#' @export
#'
is.MCMCgrowth <- function(x) inherits(x, "MCMCgrowth")

#' Test of StochasticGrowth object
#'
#' Tests if an object is of class \code{StochasticGrowth}.
#'
#' @param x object to be checked.
#'
#' @return A boolean specifying whether \code{x} is of class
#'         \code{StochasticGrowth}
#'
#' @export
#'
is.StochasticGrowth <- function(x) inherits(x, "StochasticGrowth")

#------------------------------------------------------------------------------

## "summary" methods

#' Summary of a FitDynamicGrowth object
#'
#' @param object Instance of FitDynamicGrowth
#' @param ... ignored
#'
#' @export
#'
summary.FitDynamicGrowth <- function(object, ...) {

    summary(object$fit_results)

}

#' Summary of a FitDynamicGrowthMCMC object
#'
#' @param object Instance of FitDynamicGrowthMCMC
#' @param ... ignored
#'
#' @export
#'
summary.FitDynamicGrowthMCMC <- function(object, ...) {

    summary(object$fit_results)

}

#' Summary of a FitIsoGrowth object
#'
#' @param object Instance of FitIsoGrowth
#' @param ... ignored
#'
#' @export
#'
summary.FitIsoGrowth <- function(object, ...) {

    summary(object$fit)

}

#' Summary of a FitSecondaryGrowth object
#'
#' @param object Instance of FitSecondaryGrowth
#' @param ... ignored
#'
#' @export
#'
summary.FitSecondaryGrowth <- function(object, ...) {

    summary(object$fit_results)

}

#' Summary of a FitSecondaryGrowth object
#'
#' @param object Instance of FitSecondaryGrowth
#' @param ... ignored
#'
#' @export
#'
summary.FitSecondaryGrowth <- function(object, ...) {

    summary(object$fit_results)

}

#' Summary of a FitMultipleDynamicGrowth object
#'
#' @param object Instance of FitMultipleDynamicGrowth
#' @param ... ignored
#'
#' @export
#'
summary.FitMultipleDynamicGrowth <- function(object, ...) {

    summary(object$fit_results)

}

#' Summary of a FitMultipleGrowthMCMC object
#'
#' @param object instance of FitMultipleGrowthMCMC.
#' @param ... ignored.
#'
#' @export
#'
summary.FitMultipleGrowthMCMC <- function(object, ...) {

    summary(object$fit_results)

}

#---------------------------------------------------------

#' Residuals of a FitSecondaryGrowth object
#'
#' @param object Instance of FitSecondaryGrowth
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitSecondaryGrowth <- function(object, ...) {
    residuals(object$fit_results)
}

#' Residuals of FitIsoGrowth object
#'
#' @param object Instance of FitIsoGrowth
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitIsoGrowth <- function(object, ...) {
    residuals(object$fit)
}

#' Residuals of FitDynamicGrowth
#'
#' @param object Instance of FitDynamicGrowth
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitDynamicGrowth <- function(object, ...) {
    residuals(object$fit_results)
}


#' Residuals of FitDynamicGrowthMCMC
#'
#' @param object Instance of FitDynamicGrowthMCMC.
#' @param ... ignored.
#'
#' @importFrom dplyr select
#' @importFrom FME modCost
#'
#' @export
#'
residuals.FitDynamicGrowthMCMC <- function(object, ...) {

    simulations <- object$best_prediction$simulation %>%
        select("time", "logN") %>%
        as.data.frame()

    my_cost <- modCost(model = simulations,
                obs = as.data.frame(object$data))

    my_cost$residuals$res

}

#' Residuals of FitMultipleDynamicGrowth
#'
#' @param object Instance of FitMultipleDynamicGrowth
#' @param ... ignored
#'
#' @importFrom stats residuals
#'
#' @export
#'
residuals.FitMultipleDynamicGrowth <- function(object, ...) {
    residuals(object$fit_results)
}

#' Residuals of FitMultipleGrowthMCMC
#'
#' @param object Instance of FitMultipleGrowthMCMC
#' @param ... ignored
#'
#' @importFrom dplyr bind_rows select
#' @importFrom FME modCost
#'
#' @export
#'
#'
residuals.FitMultipleGrowthMCMC <- function(object, ...) {

    out <- lapply(1:length(object$data), function(i) {

        simulations <- object$best_prediction[[i]]$simulation %>%
            select("time", "logN") %>%
            as.data.frame()

        my_cost <- modCost(model = simulations,
                           obs = as.data.frame(object$data[[i]]$data))

        tibble(residual = my_cost$residuals$res,
               experiment = i)

    })

    bind_rows(out)

}















