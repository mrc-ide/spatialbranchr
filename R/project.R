##' Project future incidence
##'
##' Simulate future incidence based on past incidence in multiple
##' locations, a matrix of (possibly time-varying) reproduction
##' numbers for each location, a matrix of serial intervals,
##' a matrix of relative movement between locations.
##'
##'
##' @export
##' @param x A matrix of past incidence (integer).
##' The matrix has 1 column for each location; that is, each column is
##' interpreted as the incidence in a location.
##' @param R A n_days X n_locations matrix of reproduction numbers;
##' where n_days is either 1 or
##' the number of days over which we want to project,
##' and n_locations is the number of locations (i.e. the number of
##' columns in \code{x}). If \code{R} has only 1 row, the values will
##' be recycled.
##' @param si A matrix of discretised serial intervals. They will
##' usually be the same for each location.
##' @param pmovement A n_locations X n_locations matrix of the
##' probability of movement between locations. Entry in row i, column j
##' is the probability that a case in i will move to j during their
##' infectious period.
##' @param n_sim The number of epicurves to simulate. Defaults to 100.
##' @param n_days projection horizon. Defaults to 7.
##' @param model Currently only "poisson" is supported
##' @return
##' @author Sangeeta Bhatia, Anne Cori, Pierre Nouvellet
spatial_project <- function(x, R, si, pmovement, n_sim, n_days,
                            model = "poisson") {

}
