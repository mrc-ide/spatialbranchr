##' Process the spatially explicit branching process model fitted to incidence
##'
##' This function processes the fitted object returned by `spatial_estimate`.
##' The processing consists of translating the vector of effective reproduction number
##' parameters to a space-time matrix that is easier to interpret.
##'
##' @param fit stanfit object returned by `spatial_estimate`
##' @inheritParams spatial_project
##' @return a list with the 3 elements: (1) a three-dimensional matrix
##' where the first dimension is time,
##' the second dimension is the number of spatial units, and the thirs dimesnion
##' is the desired number of samples from the posterior distributions of Rt; (2)
##'
##' @author Sangeeta Bhatia
##' @export
process_spatial_estimate <- function(fit, x, window, samples = 1000L) {
  list_of_draws <- extract(fit)
  ## Can't get more samples than we have from the fit.

  if (samples > nrow(list_of_draws[["R"]])) {
    warning(
      "Cannot generate more than ", nrow(list_of_draws[["R"]]),
      "samples. Resetting samples to ", nrow(list_of_draws[["R"]])
    )
    samples <- nrow(list_of_draws[["R"]])
  }
  T <- nrow(x)
  N <- ncol(x)
  ## Prepare index matrix for R
  rindex <- make_rindex(window, N, T)
  out <- array(NA, dim = c(T, N, samples))
  for (row in 1:nrow(rindex)) {
    for (col in 1:ncol(rindex)) {
      ## row-col cell of the matrix gives the index of the Rt for the
      ## Rt at this time (row) for this location (cell). We get the
      ## desired number of samples for this Rt from the extracted
      ## draws and fill out the array.
      index <- rindex[row, col]
      out[row, col, ] <- list_of_draws[["R"]][index]
    }
  }

  if (! is.null(list_of_draws[["gamma"]])) {
    gamma <- list_of_draws[["gamma"]]
  } else {
    gamma <- NULL
  }

  if (! is.null(list_of_draws[["pstay"]])) {
    pstay <- list_of_draws[["pstay"]]
  } else {
    pstay <- NULL
  }

  list(Rt = out, gamma = gamma, pstay = pstay)

}
