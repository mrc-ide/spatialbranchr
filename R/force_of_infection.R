##' Force of infection under a spatially explicit brancing process model
##'
##' This function computes the force of infection at time t + 1 at
##' locations 1, 2,....n
##' given incidence (by onset) at these locations, the serial interval
##' and the probabilities of movement of individuals during their
##' infectious period.
##'
##'
##'
##' @param x A matrix of past incidence (integer). The matrix has 1
##' column for each location; that is, each column is interpreted as
##' the incidence in a location.
##' @param r_t A matrix of reproduction numbers at times 1 through t
##' in locations 1 through n.
##' @param ws flipped serial interval
##' @param pmovement A n_locations X n_locations matrix of the
##' probability of movement between locations. Entry in row i, column j
##' is the probability that a case in i will move to j during their
##' infectious period.
##' @return
##' @author Sangeeta Bhatia, Anne Cori, Pierre Nouvellet
##' @export
force_of_infection <- function(x, r_t, ws, pmovement) {
  out <- (ws %*% x) * r_t
  out %*% pmovement
}
