
#' Growth of Salmonella spp in broth
#' 
#' An example dataset to illustrate [fit_isothermal_growth()]. It
#' describes the growth of Salmonella spp. in broth. It was retrieved from 
#' ComBase (ID: B092_10).
#' 
#' @format A tibble with 21 rows and 2 columns:
#' \describe{
#'     \item{time}{elapsed time in hours.}
#'     \item{logN}{observed population size (log CFU/g).}
#' }
#' 
"growth_salmonella"

#' Growth rates obtained for several growth experiments
#'
#' An example dataset illustrating the requirements of the
#' [fit_secondary_growth()] function.
#'
#' @format A data frame with 64 rows and 3 variables:
#' \describe{
#'   \item{temperature}{storage temperature (ºC)}
#'   \item{pH}{pH of the media}
#'   \item{mu}{specific growth rate (log10 CFU/h)}
#' }
"example_cardinal"

#' Environmental conditions during a dynamic experiment
#'
#' An example dataset illustrating the requirements of the
#' [fit_dynamic_growth()] function.
#'
#' @format A data frame with 3 rows and 3 variables:
#' \describe{
#'   \item{time}{elapsed time (h)}
#'   \item{temperature}{storage temperature (ºC)}
#'   \item{aw}{water activity}
#' }
"example_env_conditions"

#' Microbial growth under dynamic conditions
#'
#' An example dataset illustrating the requirements of the
#' [fit_dynamic_growth()] function.
#'
#' @format A data frame with 30 rows and 2 variables:
#' \describe{
#'   \item{time}{elapsed time (h)}
#'   \item{logN}{log population size (log10 CFU)}
#' }
"example_dynamic_growth"

#' A set of growth experiments under dynamic conditions
#'
#' An example dataset illustrating the requirements of
#' [fit_multiple_growth()] and
#' [fit_multiple_growth_MCMC()].
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

#' Number of tractors in the Arab World according to the World Bank
#' 
#' A dataset showing the increase in tractors in the Arab World. It was retrieved
#' from https://data.worldbank.org/indicator/AG.AGR.TRAC.NO?end=2009&start=1961&view=chart.
#' 
#' @format A tibble with 40 rows (each corresponding to one year) and 7 columns:
#' \describe{
#'     \item{year}{Year for the recording}
#'     \item{tractors}{Number of tractors}
#' }
"arabian_tractors"

#' Example of dynamic growth
#' 
#' A dataset to demonstrate the use of fit_dynamic_growth. The values of the 
#' environmental conditions are described in conditions_pH_temperature.
#' 
#' @format A tibble with 20 rows and 2 columns:
#' \describe{
#'     \item{time}{elapsed time}
#'     \item{logN}{decimal logarithm of the population size}
#' }
#' 
"growth_pH_temperature"

#' Conditions during a dynamic growth experiment
#' 
#' A dataset to demonstrate the use of fit_dynamic_growth. The observations 
#' environmental conditions are described in conditions_pH_temperature.
#' 
#' @format A tibble with 4 rows and 3 columns:
#' \describe{
#'     \item{time}{elapsed time}
#'     \item{temperature}{temperature}
#'     \item{pH}{pH}
#' }
"conditions_pH_temperature"

