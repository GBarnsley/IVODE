test_that("static point estimate projection", {
  type <- "static_model"
  t_projection_starts <- 365 * 10
  t_projection_ends <- t_projection_starts + 365
  age_group_sizes <- c(365/12, 365*(5/12), 365*(6/12), 14*365, 45*365)
  n_age <- length(age_group_sizes) + 1
  death_rates <- 1/500
  tt_death_rates <- NULL
  birth_rates <- 1/500
  tt_birth_rates <- NULL
  duration_of_maternal_immunity <- 364 / 2
  vaccine_names <- c("PCV", "Measles")
  force_of_infection <- list(PCV = 1/100, Measles = 0)
  tt_force_of_infection <- list(PCV = NULL, Measles = NULL)
  vaccine_efficacy <- list(PCV = 0.5, Measles = 0.7)
  vaccine_efficacy_disease <- list(PCV = 0.6, Measles = 0.95)
  vaccinations <- list(PCV = c(0, 0.5, 0, 0, 0, 0), Measles = c(0, 0.75, 0, 0, 0, 0))
  tt_vaccinations <- list(PCV = NULL, Measles = NULL)
  duration_of_pre_infectious <- NULL
  duration_of_infectious <- NULL
  duration_of_immunity <- list(PCV = 5*365, Measles = 10*365)
  M_0 <- list(PCV = NULL, Measles = NULL)
  S_0 <- list(PCV = rep(10000, n_age), Measles = rep(9000, n_age))
  I_0 <- NULL
  R_0 <- list(PCV = rep(1000, n_age), Measles = rep(2000, n_age))

  additional_parameters <- list(
    PCV = list(
    prop_death = rep(1, n_age)
  ), Measles = list(
    prop_death = rep(1, n_age)
  ))

  output <- project_point_estimate(
    type = type,
    t_projection_starts = t_projection_starts,
    t_projection_ends = t_projection_ends,
    age_group_sizes = age_group_sizes,
    death_rates = death_rates,
    tt_death_rates = tt_death_rates,
    birth_rates = birth_rates,
    tt_birth_rates = tt_birth_rates,
    duration_of_maternal_immunity = duration_of_maternal_immunity,
    additional_parameters = additional_parameters,
    vaccine_names = vaccine_names,
    force_of_infection = force_of_infection,
    tt_force_of_infection = tt_force_of_infection,
    vaccine_efficacy = vaccine_efficacy,
    vaccine_efficacy_disease = vaccine_efficacy_disease,
    vaccinations = vaccinations,
    tt_vaccinations = tt_vaccinations,
    duration_of_pre_infectious = duration_of_pre_infectious,
    duration_of_infectious = duration_of_infectious,
    duration_of_immunity = duration_of_immunity,
    S_0 = S_0,
    I_0 = I_0,
    R_0 = R_0,
    M_0 = M_0
  )
  expect_type(output, "list")
  expect_true(is.data.frame(output$demographics))
  expect_true(is.data.frame(output$projections))
  #in projections the population should be the same as the initial population
})

test_that("dynamic point estimate projection", {
  type <- "dynamic_model"
  t_projection_starts <- 365 * 10
  t_projection_ends <- t_projection_starts + 365
  age_group_sizes <- c(365/12, 365*(5/12), 365*(6/12), 14*365, 45*365)
  n_age <- length(age_group_sizes) + 1
  death_rates <- 1/500
  tt_death_rates <- NULL
  birth_rates <- 1/500
  tt_birth_rates <- NULL
  duration_of_maternal_immunity <- 364 / 2
  vaccine_names <- c("PCV", "Measles")
  force_of_infection <- list(PCV = 10, Measles = 20)
  tt_force_of_infection <- list(PCV = NULL, Measles = NULL)
  vaccine_efficacy <- list(PCV = 0.5, Measles = 0.7)
  vaccine_efficacy_disease <- list(PCV = 0.6, Measles = 0.95)
  vaccinations <- list(PCV = c(0, 0.5, 0, 0, 0, 0), Measles = c(0, 0.75, 0, 0, 0, 0))
  tt_vaccinations <- list(PCV = NULL, Measles = NULL)
  duration_of_pre_infectious <- list(PCV = 1/1, Measles = 1/10)
  duration_of_infectious <- list(PCV = 1/20, Measles = 1/5)
  duration_of_immunity <- list(PCV = 5*365, Measles = 10*365)
  M_0 <- list(PCV = NULL, Measles = NULL)
  S_0 <- list(PCV = rep(10000, n_age), Measles = rep(9000, n_age))
  I_0 <- list(PCV = rep(1, n_age), Measles = rep(1, n_age))
  R_0 <- list(PCV = rep(1000, n_age), Measles = rep(2000, n_age))

  additional_parameters <- list(
  PCV = list(
    prop_death = rep(1, n_age)
  ), Measles = list(
    prop_death = rep(1, n_age)
  ))

  output <- project_point_estimate(
    type = type,
    t_projection_starts = t_projection_starts,
    t_projection_ends = t_projection_ends,
    age_group_sizes = age_group_sizes,
    death_rates = death_rates,
    tt_death_rates = tt_death_rates,
    birth_rates = birth_rates,
    tt_birth_rates = tt_birth_rates,
    duration_of_maternal_immunity = duration_of_maternal_immunity,
    additional_parameters = additional_parameters,
    vaccine_names = vaccine_names,
    force_of_infection = force_of_infection,
    tt_force_of_infection = tt_force_of_infection,
    vaccine_efficacy = vaccine_efficacy,
    vaccine_efficacy_disease = vaccine_efficacy_disease,
    vaccinations = vaccinations,
    tt_vaccinations = tt_vaccinations,
    duration_of_pre_infectious = duration_of_pre_infectious,
    duration_of_infectious = duration_of_infectious,
    duration_of_immunity = duration_of_immunity,
    S_0 = S_0,
    I_0 = I_0,
    R_0 = R_0,
    M_0 = M_0
  )
  expect_type(output, "list")
  expect_true(is.data.frame(output$demographics))
  expect_true(is.data.frame(output$projections))
  #in projections the population should be the same as the initial population
})
