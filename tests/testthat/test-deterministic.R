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
  age_group_sizes <- c(365/12, 365*(11/12), 14*365, 45*365)
  n_age <- length(age_group_sizes) + 1
  death_rates <- 1/10000
  tt_death_rates <- NULL
  birth_rates <- 1/1000
  tt_birth_rates <- NULL
  force_of_infection <- 0
  tt_force_of_infection <- NULL
  vaccine_efficacy <- 0.5
  coverage <- 0.50
  vaccinations <- c(0, -(log(1-coverage)/min(c(max(t), age_group_sizes[2]))), 0, 0, 0)
  tt_vaccinations <- NULL
  duration_of_immunity <- 365 * 5
  duration_of_maternal_immunity <- 364 / 2
  M_0 <- rep(0, 2)
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
  t <- c(0, 365 * 20)
  duration_of_immunity <- Inf
  #not sure this makes sense
  vaccinations <- -log(1 - coverage) / c(age_group_sizes, 1/death_rates)
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
    vaccinations,
    tt_vaccinations,
    duration_of_immunity,
    duration_of_maternal_immunity,
    S_0,
    R_0,
    M_0,
    additional_parameters
  )
  vaccine_immune <- format_output(res, "Immune(Vaccine)", reduce_age = FALSE) %>%
    dplyr::filter(age_group > 1 & t == max(t)) %>%
    dplyr::pull(value) %>%
    sum()
  pop <- format_output(res, "Population", reduce_age = FALSE) %>%
    dplyr::filter(age_group > 1 & t == max(t)) %>%
    dplyr::pull(value) %>%
    sum()
  expect_true(abs((vaccine_efficacy * coverage - (vaccine_immune/pop))/(vaccine_efficacy * coverage)) < 0.05)
})
