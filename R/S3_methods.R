
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





