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

##' Standard deviation with optional args
##'
##' This function computes the standard deviation of the values in x.
##' It calls \code{stats::sd} except that it also accepts optional
##' arguments and therefore can be used in conjunction with other functions
##' in \code{summarise_projections}.
##' @param vec a numeric vector
##' @param ... optional arguments
##' @return numeric. standard deviation
##' @author Sangeeta Bhatia, Anne Cori, Pierre Nouvellet
##' @export
sd2 <- function(vec, ...) {
  ## sd pnly has one argument, na.rm. It does not have an optional arg
  if (! 'na.rm' %in% names(list(...))) return(sd(vec))
  else sd(vec,  list(...)[["na.rm"]])
}
##' Extract summary statistics for projections
##'
##'
##' @title Summarise projections across simulations
##' @param x output from \code{spatial_project} function
##' @param funs a list of summary functions to be applied.
##' It is the user's responsibility to ensure that the functions
##' are valid and can be applied to a vector. Defaults to
##' quantile, min, max, mean and sd.
##' @param ... Additional arguments for funs, e.g. na.rm, probs
##' @return A list of the same length as that of the supplied functions.
##' Each element of the list is the result of applying the corresponding function
##' across simulations for each day and each location.
##' @details If you want to use multiple functions and  pass optional
##' arguments other than na.rm, then use \code{sd2} rather than \code{sd}
##' to estimate standard deviation. This is because \code{sd} does not
##' accept optional arguments.
##' @author Sangeeta Bhatia, Anne Cori, Pierre Nouvellet
##' @export
summarise_projections <- function(x,
                                  funs = c(`quantile` = quantile,
                                           `min` = min, `max` = max,
                                           `mean` = mean, `sd` = sd2
                                           ), ...) {
  out <- vector(mode = "list", length = length(funs))
  for (index in seq_along(funs)) {
    f <- funs[[index]]
    out[[index]] <-   apply(x, c(1, 2), function(y) f(y, ...))
  }
  names(out) <- names(funs)
  out
}
