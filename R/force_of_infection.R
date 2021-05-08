force_of_infection <- function(x, r_t, ws, pmovement) {
  out <- (ws %*% incid) * r_t
  out <- out %*% pij
  out
}
