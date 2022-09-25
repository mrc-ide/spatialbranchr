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
##' @param si A matrix of discretised serial interval, assumed to be the same
##' same for all locations. These can be generated easily using the package EpiEstim.
##' See vignettes for examples.
##' @param pmovement A n_locations X n_locations matrix of the
##' probability of movement between locations. Entry in row i, column j
##' is the probability that a case in i will move to j during their
##' infectious period.
##' @param n_sim The number of epicurves to simulate. Defaults to 100.
##' @param n_days projection horizon. Defaults to 7.
##' @param model Currently only "poisson" is supported
##' @return an array conatining the projected incidence.
##' The dimensions of the returned array are
##' n_days X n_locations X n_sim
##' @author Sangeeta Bhatia, Anne Cori, Pierre Nouvellet
##' @importFrom stats rpois
##' @importFrom utils tail
spatial_project <- function(x, R, si, pmovement, n_sim, n_days,
                            model = "poisson") {
  model <- match.arg(model)
  ## TODO Checks
  ## Check dimensions of x
  ## Check that x has only integers.
  ## Check dimensions of R.
  ## Check that R is non-negative
  ## Check dimensions of si
  ## Check that si is non-negative and that si[0] = 0 as in EpiEstim
  ## Check dimensions of pmovement.
  ## Check that pmovement is non-negative
  ## Check that n_sim is an integer
  ## Check that n_days is an integer
  if (ncol(R) != ncol(x)) {
    stop("R should be a either a 1 X N or T X N matrix.")
  }
  if (nrow(R) != nrow(x)) {
    if (nrow(R) != 1) {
      stop("R should be a either a 1 X N or T X N matrix.")
    }
  }

  n_loc <- ncol(x)
  out <- vector(mode = "list", length = n_sim)
  start <- nrow(x) + 1
  end <- nrow(x) + n_days

  if (nrow(R) == 1) {
    R <- matrix(R,
      nrow = end - start + 1,
      ncol = n_loc, byrow = TRUE
    )
  }
  if (length(si) < end) {
    ws <- rev(c(si, rep(0, end - length(si))))
  } else {
    ws <- rev(si)
  }

  for (sim in seq_len(n_sim)) {
    out_local <- rbind(x, matrix(0, nrow = n_days, ncol = n_loc))
    for (i in start:end) {
      i_t <- out_local[1:i, ]
      w_t <- utils::tail(ws, i)
      r_t <- R[i - start + 1, ]
      mu <- force_of_infection(i_t, r_t, w_t, pmovement)
      out_local[i - nrow(x), ] <- rpois(n_loc, mu)
    }
    out[[sim]] <- out_local
  }
  ## Set-up as an array
  out_array <- array(
    0, dim = c(nrow(out_local), ncol(out_local), n_sim)
  )
  for (sim in seq_len(n_sim)) {
    out_array[, , sim] <- out[[sim]]
  }
  out_array
}
