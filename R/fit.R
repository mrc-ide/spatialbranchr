##' Prepare index matrix for Rt
##' @details
##' This is a utility function that prepares the
##' index matrix for Rt estimation in Stan.
##' Say we have two locations and a time series
##' of cases over 14 days. For each location,
##' we want to estimate Rt over a 7 day window
##' so that in all, the model has 4 parameters for
##' Rt (two for each location). The matrix looks
##' like:
##'  \tabular{rr}{
##'   1 \tab 3\cr
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
##' The columns index the locations and the rows index the time points.
##' Given a vector rindex return a nrow * ncol matrix where
##' rindex[1:ncol] will be inserted in rows 1 to change_at[1].
##' Similarly rindex[ncol + 1: (2 * ncol)] will be inserted from
##' change_at[1] + 1 to row_indices[2] and so on.
##'
##'
##' @param window
##' @param nloc integer. Number of locations.
##'
##' @param T integer. Number of time steps
##' @return matrix of R index
##' @export
##' @examples
##' make_rindex(c(1, 2, 3), 3, 4, c(4))
##' make_rindex(c(1, 2, 3), 1, 9, c(5, 7))
##' @author Sangeeta Bhatia, Anne Cori, Pierre Nouvellet
make_rindex <- function(window, nloc, T) {
  change_at <- window + 1
  ## Number of parameters if T is divisible by window length
  ## (T/window) * nloc
  if (T %% window == 0) {
    npars <- (T / window) * nloc
  } else {
    ## Need one more
    npars <- (floor(T / window) + 1) * nloc
  }
  rindex <- seq(from  = 1, to = npars)
  split_at <- seq(from = 1, to = length(rindex), by = nloc)
  split_R  <- unname(split(rindex, cumsum(seq(rindex) %in%  split_at)))
  out <- mapply(rep, x = split_R, times = window)
  matrix(
    unlist(out), byrow = TRUE, ncol = nloc, nrow = T
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
##' @param window integer indicating the length of the window over
##' which Rt is to be estimated. Window length is assumed to be the same
##' for all locations. Default value is 7 i.e. Rt is estimated over
##' a weekly window.
##'
##' @param moving TRUE/FALSE indicating whether estimation should happen
##' over moving windows (TRUE) as in EpiEstim, or non-overlapping
##' windows (FALSE). Defaults to FALSE. Setting this to true will increase
##' the number of parameters in the model.
##'
##'
##' @param priors
##' @param ...
##' @return
##' @author Sangeeta Bhatia
spatial_estimater <- function(x, si, pmovement = NULL, window = 7L,
                              moving = FALSE, model = "poisson",
                              priors, ...) {
  ## TODO implement checks on data
  ## Prepare data for Stan
  standata <- list(T = nrow(x), N = ncol(x), I = x, SI = si)
  ## Prepare index matrix for R
  rindex <- make_rindex()

  if (is.null(pmovement)) {

  } else {

  }

}
