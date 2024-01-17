test_that("simple deterministic", {
  type <- "deterministic"
  t <- seq(0, 365)
  age_group_sizes <- c(365/12, 365*(11/12), 14*365, 45*365)
  n_age <- length(age_group_sizes) + 1
  death_rates <- 1/c(100, 1000, 100, 10, 1)
  tt_death_rates <- NULL
  birth_rates <- c(0, 0, 0, 2.5, 0)
  tt_birth_rates <- NULL
  force_of_infection <- c(0, 0, 0, 0, 0)
  tt_force_of_infection <- NULL
  vaccine_efficacy <- 0.5
  vaccine_efficacy_disease <- 0.75
  vaccinations <- c(0, 100, 0, 0, 0)
  tt_vaccinations <- NULL
  duration_of_immunity <- 365 * 5
  duration_of_maternal_immunity <- 364 / 2
  M_0 <- rep(0, 2)
  S_0 <- rep(1000, n_age)
  R_0 <- rep(0, n_age)

  res <- simulate(
    type,
    t,
    age_group_sizes,
    death_rates,
    tt_death_rates,
    birth_rates,
    tt_birth_rates,
    force_of_infection,
    tt_force_of_infection,
    vaccine_efficacy,
    vaccine_efficacy_disease,
    vaccinations,
    tt_vaccinations,
    duration_of_immunity,
    duration_of_maternal_immunity,
    S_0,
    R_0,
    M_0
  )
  # no nan
  expect_false(any(is.nan(res@output)))
  # all postive (with lower bound of 0.1)
  expect_true(all(res@output >= -0.1))
  #number of vaccinated people should roughly be doses * time * efficacy (no waning)
  duration_of_immunity <- 365 * 100000000
  death_rates <- rep(0, n_age)
  res <- simulate(
    type,
    t,
    age_group_sizes,
    death_rates,
    tt_death_rates,
    birth_rates,
    tt_birth_rates,
    force_of_infection,
    tt_force_of_infection,
    vaccine_efficacy,
    vaccine_efficacy_disease,
    vaccinations,
    tt_vaccinations,
    duration_of_immunity,
    duration_of_maternal_immunity,
    S_0,
    R_0,
    M_0
  )
  vaccine_immune <- format_output(res, "Immune(Vaccine)", reduce_age = TRUE) %>%
    dplyr::pull(value) %>%
    max()
  expect_true(abs(sum(vaccinations) * max(t) * vaccine_efficacy - vaccine_immune)/vaccine_immune < 0.01)
})

test_that("gaza deterministic", {
  type <- "deterministic_gz"
  t <- seq(0, 365)
  age_group_sizes <- c(365/12, (5/12)*365, 365*(6/12), 14*365, 45*365)
  n_age <- length(age_group_sizes) + 1
  death_rates <- 1/10000
  tt_death_rates <- NULL
  birth_rates <- 1/1000
  tt_birth_rates <- NULL
  force_of_infection <- 0
  tt_force_of_infection <- NULL
  vaccine_efficacy <- 0.5
  vaccine_efficacy_disease <- 0.75
  coverage <- 0.50
  vaccinations <- c(0, 0, coverage, 0, 0, 0)
  tt_vaccinations <- NULL
  duration_of_immunity <- 365 * 5
  duration_of_maternal_immunity <- 364 / 2
  M_0 <- NULL
  S_0 <- rep(50000, n_age)
  R_0 <- rep(0, n_age)
  additional_parameters <- list(
    prop_death = rep(1, n_age)
  )

  res <- simulate(
    type,
    t,
    age_group_sizes,
    death_rates,
    tt_death_rates,
    birth_rates,
    tt_birth_rates,
    force_of_infection,
    tt_force_of_infection,
    vaccine_efficacy,
    vaccine_efficacy_disease,
    vaccinations,
    tt_vaccinations,
    duration_of_immunity,
    duration_of_maternal_immunity,
    S_0,
    R_0,
    M_0,
    additional_parameters
  )
  # no nan
  expect_false(any(is.nan(res@output)))
  # all postive (with lower bound of 0.1)
  expect_true(all(res@output >= -0.1))
  #number of vaccinated people should roughly be doses * time * efficacy (no waning)
  t <- c(0, 365 * 100)
  duration_of_immunity <- Inf
  age_group_sizes <- c(365/12, 365*(5/12), 365 * (11/12), 365, 14*365, 45*365)
  n_age <- length(age_group_sizes) + 1
  M_0 <- NULL
  S_0 <- c(rep(5000, 1), rep(10, n_age - 1))
  R_0 <- rep(0, n_age)
  additional_parameters <- list(
    prop_death = rep(1, n_age)
  )
  #not sure this makes sense
  vaccinations <- c(0, coverage, 0, 0, 0, 0, 0)
  res <- simulate(
    type,
    t,
    age_group_sizes,
    death_rates,
    tt_death_rates,
    birth_rates,
    tt_birth_rates,
    force_of_infection,
    tt_force_of_infection,
    vaccine_efficacy,
    vaccine_efficacy_disease,
    vaccinations,
    tt_vaccinations,
    duration_of_immunity,
    duration_of_maternal_immunity,
    S_0,
    R_0,
    M_0,
    additional_parameters
  )
  vaccine_immune <- format_output(res, "Immune", reduce_age = FALSE) %>%
    dplyr::filter(age_group == 4 & t == max(t)) %>%
    dplyr::pull(value) %>%
    sum()
  vaccine_immune_disease <- format_output(res, "Immune(Disease)", reduce_age = FALSE) %>%
    dplyr::filter(age_group == 4 & t == max(t)) %>%
    dplyr::pull(value) %>%
    sum()
  pop <- format_output(res, "Population", reduce_age = FALSE) %>%
    dplyr::filter(age_group == 4 & t == max(t)) %>%
    dplyr::pull(value) %>%
    sum()
  
  infection_coverage <- coverage * vaccine_efficacy
  infection_immune <- vaccine_immune/pop
  disease_coverage <- coverage * vaccine_efficacy_disease
  disease_immune <- vaccine_immune_disease/pop

  err_func <- function(x, y) {
    abs(x - y)/x
  }

  expect_true(err_func(infection_coverage, infection_immune) < 0.05)
  expect_true(err_func(disease_coverage, disease_immune) < 0.05)
})
