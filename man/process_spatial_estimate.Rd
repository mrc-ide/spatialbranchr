% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_fit.R
\name{process_spatial_estimate}
\alias{process_spatial_estimate}
\title{Process the spatially explicit branching process model fitted to incidence}
\usage{
process_spatial_estimate(fit, x, window, samples = 1000L)
}
\arguments{
\item{fit}{stanfit object returned by `spatial_estimate`}

\item{x}{A matrix of past incidence (integer).
The matrix has 1 column for each location; that is, each column is
interpreted as the incidence in a location.}

\item{window}{integer indicating the length of the window over
which Rt is to be estimated. Window length is assumed to be the same
for all locations. Default value is 7 i.e. Rt is estimated over
a weekly window. Currently only non-overlapping windows
are supported.}

\item{samples}{a non-negative integer giving the desired number of
samples from the posterior distribution of each parameter.}
}
\value{
a list with the 3 elements: (1) a three-dimensional matrix
where the first dimension is time,
the second dimension is the number of spatial units, and the thirs dimesnion
is the desired number of samples from the posterior distributions of Rt; (2)
}
\description{
This function processes the fitted object returned by `spatial_estimate`.
The processing consists of translating the vector of effective reproduction number
parameters to a space-time matrix that is easier to interpret.
}
\author{
Sangeeta Bhatia
}
