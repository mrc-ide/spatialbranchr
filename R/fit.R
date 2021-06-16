##' Prepare index matrix for Rt
##' @details
##' This is a utility function that prepared the
##' index matrix for Rt estimation in Stan.
##' Say we have two locations and a time series
##' of cases over 14 days. For each location,
##' we want to estimate Rt over a 7 day window
##' so that in all the model has 4 parameters for
##' Rt (two for each location). The matrix looks
##' like:
##' #' \tabular{rr}{
##'   \strong{1} \tab \strong{2} \cr
##'   1 \tab 3\cr
##'   1 \tab 3\cr
##'   1 \tab 3\cr
##'   1 \tab 3\cr
##'   1 \tab 3\cr
##'   1 \tab 3\cr
##'   1 \tab 3\cr
##'   2 \tab 4\cr
##'   2 \tab 4\cr
##'   2 \tab 4\cr
##'   2 \tab 4\cr
##'   2 \tab 4\cr
##'   2 \tab 4\cr
##'   2 \tab 4
##' }
##' Given a vector R return a nrow * ncol matrix where
##' R[1:ncol] will be inserted in rows 1 to change_at[1].
##' Similarly R[ncol + 1: (2 * ncol)] will be inserted from
##' change_at[1] + 1 to row_indices[2] and so on.
##'
##' @title
##' @param R
##' @param ncol
##' @param nrow
##' @param change_at
##' @return
##' @examples
##' make_rindex(c(1, 2, 3), 3, 4, c(4))
##' make_rindex(c(1, 2, 3), 1, 9, c(5, 7))
##' @author Sangeeta Bhatia, Anne Cori, Pierre Nouvellet
make_rindex <- function(rindex, ncol, nrow, change_at) {
  split_at <- seq(from = 1, to = length(rindex), by = ncol)
  split_R  <- unname(split(rindex, cumsum(seq(rindex) %in%  split_at)))
  num_rows <- diff(c(1, change_at, nrow + 1))

  out <- mapply(rep, x = split_R, times = num_rows)
  matrix(
    unlist(out), byrow = T, ncol = ncol, nrow = nrow
  )
}
##' Estimate effective reproduction number using a spatially explicit
##' branching process model.
##'
##' Given past incidence in N locations, this function (a) jointly
##' estimates the reproduction number and the probability of movement,
##' or (b) if pmovement is not NULL, estimates only the reproduction
##' number
##' @inheritParams spatial_project
##'
##' @param x
##' @param si
##' @param pmovement
##' @param tstart A matrix of positive integers giving the starting
##' index of the windows over which reproduction number is estimated.
##'
##' @param tend A matrix of positive integers giving the indices of the
##' endpoints of the windows over which reproduction number is estimated.
##'
##' @param priors
##' @param ...
##' @return
##' @author Sangeeta Bhatia
spatial_estimater <- function(x, si, pmovement = NULL, tstart,
                              tend, model = "poisson",
                              priors, ...) {
  ## TODO implement checks on data
  ## Prepare data for Stan
  standata <- list(
    T = nrow(x), N = ncol(x),
    I = x, SI = si
  )
  if (is.null(pmovement)) {

  } else {

  }

}
