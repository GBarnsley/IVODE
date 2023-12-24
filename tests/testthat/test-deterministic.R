test_that("multiplication works", {
  type <- "deterministic"
  t <- c(0, 365)
  age_group_sizes <- c(365/12, 365*(11/12), 14*365, 45*365)
  n_age <- length(age_group_sizes) + 1
  death_rates <- 1/c(100, 1000, 100, 10, 1)
  tt_death_rates <- NULL
  birth_rates <- c(0, 0, 0, 2.5, 0)
  tt_birth_rates <- NULL
  force_of_infection <- c(0, 0, 0, 0, 0)
  tt_force_of_infection <- NULL
  vaccine_efficacy <- 0.5
  vaccination_coverage <- c(0, 0, 0, 0, 0)
  tt_vaccination_coverage <- NULL
  duration_of_immunity <- 365*5
  duration_of_maternal_immunity <- 364/2
  M_0 <- rep(0, 2)
  S_0 <- rep(1000, n_age)
  R_0 <- rep(0, n_age)

})
