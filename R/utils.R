##' Convert the projections to a data.frame
##'
##' This function convers the output of
##' \code{spatial_project} function into a wide
##' data.frame.
##'
##'
##' @param x output from \code{spatial_project}
##' @return data.frame with columns
##' sim, day, and 1 column with each location.
##' @author Sangeeta Bhatia, Anne Cori, Pierre Nouvellet
##' @export
##'
as_dataframe <- function(x) {
  ## x is the output from spatial_project
  ## It is 3 dimensional array with dimension 1
  ## indexing days, dimension 2 indexing locations
  ## and dimension 3 indexing simulations
  xdf <- data.frame(x[, , 1])
  xdf$sim <- 1
  xdf$day <- seq_len(nrow(xdf))
  nsims <- dim(x)[3]
  for (index in seq(2, nsims)) {
    out <- data.frame(x[, , index])
    out$sim <- index
    out$day <- seq_len(nrow(out))
    xdf <- rbind(xdf, out)
  }
  xdf
}
