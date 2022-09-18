## https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
matequal <- function(x, y) {
  is.matrix(x) &&
    is.matrix(y) &&
    all(dim(x) == dim(y)) &&
    all(x == y)
}

test_that("rindex matrix works", {
  window <- 2
  T <- 4
  nloc <- 2
  out <- make_rindex(window, nloc, T)
  right <- matrix(
    c(1, 1, 3, 3, 2, 2, 4, 4), ncol = 2
  )
  expect_true(matequal(out, right))
})


test_that(
  "rindex matrix works when window does not divide T", {
  window <- 3
  T <- 8
  nloc <- 2
  out <- make_rindex(window, nloc, T)
  right <- matrix(
    c(1, 2, 1, 2, 1, 2, 3, 4, 3, 4, 3, 4, 5, 6, 5, 6),
    ncol = nloc, byrow = TRUE
  )
  expect_true(matequal(out, right))
})
