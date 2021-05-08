force_of_infection <- function(x, r_t, ws, pmovement) {
  out <- (ws %*% x) * r_t
  out <- out %*% pmovement
  out
}
