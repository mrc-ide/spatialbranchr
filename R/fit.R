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

}
