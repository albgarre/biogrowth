
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
#' @source \url{http://www.diamondse.info/}
"example_dynamic_growth"
