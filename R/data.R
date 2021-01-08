
#' Growth of Salmonella spp in broth
#' 
#' An example dataset to illustrate \code{\link{fit_isothermal_growth}}. It
#' describes the growth of Salmonella spp. in broth. It was retrieved from 
#' ComBase (ID: B092_10).
#' 
#' @format A tibble with 21 rows and 2 columns:
#' \describe{
#'     \item{time}{elapsed time in hours.}
#'     \item{logN}{observed microbial count (log CFU/g).}
#' }
#' 
"growth_salmonella"

#' Growth rates obtained for several growth experiments
#'
#' An example dataset illustrating the requirements of the
#' \code{\link{fit_secondary_growth}} function.
#'
#' @format A data frame with 64 rows and 3 variables:
#' \describe{
#'   \item{temperature}{storage temperature}
#'   \item{pH}{pH of the media}
#'   \item{mu}{specific growth rate}
#' }
"example_cardinal"

#' Environmental conditions during a dynamic experiment
#'
#' An example dataset illustrating the requirements of the
#' \code{\link{fit_dynamic_growth}} function.
#'
#' @format A data frame with 3 rows and 3 variables:
#' \describe{
#'   \item{time}{elapsed time}
#'   \item{temperature}{storage temperature}
#'   \item{aw}{water activity}
#' }
"example_env_conditions"

#' Microbial growth under dynamic conditions
#'
#' An example dataset illustrating the requirements of the
#' \code{\link{fit_dynamic_growth}} function.
#'
#' @format A data frame with 30 rows and 2 variables:
#' \describe{
#'   \item{time}{elapsed time}
#'   \item{logN}{log microbial count}
#' }
"example_dynamic_growth"

#' A set of growth experiments under dynamic conditions
#'
#' An example dataset illustrating the requirements of
#' \code{\link{fit_multiple_growth}} and
#' \code{\link{fit_multiple_growth_MCMC}}.
#'
#' @format A nested list with two elements. Each element
#' corresponds to one experiment and is described by a list
#' with two data frames:
#' \describe{
#'     \item{data}{a tibble describing the microbial counts. It has 2 columns:
#'     time (elapsed time) and logN (logarithm of the microbial count)}.
#'     \item{conditions}{a tibble describing the environmental conditions. It has
#'     3 columns: time (elapsed time), temperature (storage temperature) and pH (pH of the media).}
#' }
"multiple_experiments"
