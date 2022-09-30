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
##'
##'
##'
##'
##'
##'
##' @inheritParams spatial_estimate
##' @param nloc integer. Number of locations.
##'
##' @param T integer. Number of time steps
##' @return matrix of R index
##' @export
##' @examples
##' make_rindex(7, 2, 14)
##' make_rindex(4, 2, 14)
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

##' Default priors for parameters of a spatially explicit branching process
##' model
##'
##' The parameters are the effective reproduction number and the exponent on the
##' distance in a gravity model, when both are being jointly estimated.
##' Effective reproduction number is assumed to be Gamma distributed
##' and therefore, its mean and standard deviation need to be specified.
##' These are automataically translated into the shape and scale paramaters.
##' When estimating only the effective reproduction number, the
##' `gamma` component of the list is silently ignored.
##'
##' @return a list of default parameters for the priors.
##' Values can then be manually be edited as in the examples below.
##' @examples
##' priors <- spatial_priors()
##' priors$gamma <- 2
##' @author Sangeeta Bhatia
##' @export
spatial_priors <- function() {
  list(
    prior_mean = 1, prior_std = 1, gamma = 1.5
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
##' @param window integer indicating the length of the window over
##' which Rt is to be estimated. Window length is assumed to be the same
##' for all locations. Default value is 7 i.e. Rt is estimated over
##' a weekly window. Currently only non-overlapping windows
##' are supported.
##' @param pmovement either NULL or a N X N matrix where N is the
##' number of locations. If pmovement is NULL, then this function
##' jointly estimates the parameters of gravity and
##' branching process models. The gravity model describes the
##' population movement and the brancing process model describes the
##' underlying transmission process.
##' @param population a vector of length N giving the populations
##' of each location. Silently ignored if pmovement is not NULL
##' @param distance N X N matrix of the distances between the
##' N locations. Silently ignored if pmovement is not NULL
##' @param alpha gravity model paramater; exponent on source population
##' @param beta gravity model paramater; exponent on destination population
##' @param K gravity model paramater
##' @param priors a list specifying priors for the model. To use the default priors,
##' use the function `spatial_priors`
##' @param ... Additional parameters to be passed to stan
##' @return Fitted stan model, a  stanfit object
##' @author Sangeeta Bhatia
##'
##' @examples
##' # Fake incidence matrix
##' x <- rpois(14, 2)
##' y <- rpois(14, 3)
##' incid <- matrix(c(x, y), nrow = 14, ncol = 2)
##' # Fake serial interval
##' si <- c(0, 0.01, 0.02, 0.07, 0.12, 0.14, 0.15,
##'         0.13, 0.11, 0.08, 0.06, 0.04, 0.03, 0.02, 0.02)
##' pops <- c(1e6, 1e6)
##' dist <- matrix(c(0, 1e3, 1e3, 0), nrow = 2)
##' priors <- spatial_priors()
##' spatial_estimate(incid, si, 7L, NULL, pops, dist, 1, 1, 1, "poisson", priors)
##' @export
spatial_estimate <- function(x, si, window = 7L,
                              ## Data needed for population movement
                              pmovement = NULL,
                              population,
                              distance,
                              alpha, beta, K,
                              ## Transmission Model
                              model = "poisson",
                              priors, ...) {
  ## TODO implement checks on data
  T <- nrow(x)
  N <- ncol(x)
  ## Prepare index matrix for R
  rindex <- make_rindex(window, N, T)
  ## Prepare data for Stan
  standata <- list(
    T = T, N = N, I = x, SI = si, rindex = rindex,
    num_Rjt = max(rindex),
    prior_mean = priors$prior_mean,
    prior_std = priors$prior_std
  )

  if (is.null(pmovement)) {
    standata$population <- population
    standata$dist_mat <- distance
    standata$alpha <- alpha
    standata$beta <- beta
    standata$K <- K
    out <- rstan::sampling(stanmodels$estimate_both, data = standata, ...)
  } else {
    standata$pmovement <- pmovement
    out <- rstan::sampling(stanmodels$estimate_rt, data = standata, ...)
  }
  out
}
