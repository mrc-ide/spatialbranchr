## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(EpiEstim)
library(ggplot2)
library(MASS)
library(spatialbranchr)
library(tidyr)

## -----------------------------------------------------------------------------
locations <- c("A", "B", "C")
## Movement matrix, all off-diagonal elemnets are 0.
pmat <- matrix(0, nrow = 3, ncol = 3)
diag(pmat) <- 1
## Initial Rt in the three locations
r_init <- matrix(rep(2, 3), nrow = 1, ncol = 3)
## Initial incidence, start with a large seed so that the epidemic can
## take off.
incid_init <- matrix(rep(20, 3), nrow = 1, ncol = 3)

si <- EpiEstim::discr_si(k = 0:30, mu = 6, sigma = 4)

no_movement <- spatial_project(
  incid_init, r_init, si, pmat, n_sim = 1000, n_days = 30,
  model = "poisson"
)

no_movement_df <- as_dataframe(no_movement)
tall <- gather(no_movement_df, location, incid, -sim, -day)


## -----------------------------------------------------------------------------

ggplot(tall) +
  geom_line(aes(day, incid, group = sim), alpha = 0.3) +
  facet_wrap(~location, nrow = 3)


## -----------------------------------------------------------------------------
puni <- matrix(
  c(0.8, 0.1, 0.1, 0.2, 0.3, 0.5, 0.3, 0.3, 0.4), nrow = 3, ncol = 3
)
uni_movement <- spatial_project(
  incid_init, r_init, si, puni, n_sim = 1000, n_days = 30,
  model = "poisson"
)

uni_movement <- as_dataframe(uni_movement)
tall_uni <- gather(uni_movement, location, incid, -sim, -day)


## -----------------------------------------------------------------------------
ggplot(tall) +
  geom_line(aes(day, incid, group = sim), alpha = 0.3) +
  facet_wrap(~location, nrow = 3)


