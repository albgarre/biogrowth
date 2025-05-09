% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_isothermal_growth.R
\name{iso_Baranyi_noStat}
\alias{iso_Baranyi_noStat}
\title{Isothermal Baranyi model without stationary phase}
\usage{
iso_Baranyi_noStat(times, logN0, mu, lambda)
}
\arguments{
\item{times}{Numeric vector of storage times}

\item{logN0}{Initial log microbial count}

\item{mu}{Maximum specific growth rate (in ln CFU/\link{t})}

\item{lambda}{Lag phase duration}
}
\value{
Numeric vector with the predicted microbial count.
}
\description{
Baranyi growth model as defined by Baranyi and Roberts (1994). We use the solution
calculated by Poschet et al. (2005, doi: https://doi.org/10.1016/j.ijfoodmicro.2004.10.008)
after log-transformation according to MONTE CARLO ANALYSIS FOR MICROBIAL GROWTH CURVES,
by Oksuz and Buzrul.
}
