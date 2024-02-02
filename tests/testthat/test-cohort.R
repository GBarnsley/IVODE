test_that("cohort model", {
  type <- "cohort_static_model"
  t <- seq(0, 365*20, 365)
  cohort_size <- 365/3
  n_age <- 240
  age_group_sizes <- rep(cohort_size, n_age - 1)
  death_rates <- 1/10000
  tt_death_rates <- NULL
  birth_rates <- 1/1000
  tt_birth_rates <- NULL
  force_of_infection <- 1/10
  tt_force_of_infection <- NULL
  vaccine_efficacy <- 0.5
  vaccine_efficacy_disease <- 0.75
  coverage <- 0.50
  vaccinations <- rep(0, n_age)
  vaccinations[2] <- coverage
  tt_vaccinations <- NULL
  duration_of_immunity <- 365 * 5
  duration_of_maternal_immunity <- 364 / 2
  duration_of_pre_infectious <- NULL
  duration_of_infectious <- NULL
  M_0 <- NULL
  I_0 <- NULL
  S_0 <- rep(50000*6/n_age, n_age)
  R_0 <- rep(0, n_age)
  additional_parameters <- list(
    prop_death = rep(1, n_age),
    cohort_size = cohort_size
  )

  res <- simulate(
    type = type,
    t = t,
    age_group_sizes = age_group_sizes,
    death_rates = death_rates,
    tt_death_rates = tt_death_rates,
    birth_rates = birth_rates,
    tt_birth_rates = tt_birth_rates,
    force_of_infection = force_of_infection,
    tt_force_of_infection = tt_force_of_infection,
    vaccine_efficacy = vaccine_efficacy,
    vaccine_efficacy_disease = vaccine_efficacy_disease,
    vaccinations = vaccinations,
    tt_vaccinations = tt_vaccinations,
    duration_of_immunity = duration_of_immunity,
    duration_of_maternal_immunity = duration_of_maternal_immunity,
    duration_of_pre_infectious = duration_of_pre_infectious,
    duration_of_infectious = duration_of_infectious,
    S_0 = S_0,
    I_0 = I_0,
    R_0 = R_0,
    M_0 = M_0,
    additional_parameters = additional_parameters
  )
  test_cohort <- format_output(res, "Population", reduce_age = TRUE)
  
  # no nan
  expect_false(any(is.nan(res@output)))
  # all postive (with lower bound of 0.1)
  expect_true(all(res@output >= -0.1))
  #number of vaccinated people should roughly be doses * time * efficacy (no waning)
  t <- c(0, 365 * 10)#c(0, 365 * 100)
  force_of_infection <- 0
  duration_of_immunity <- Inf

  res <- simulate(
    type = type,
    t = t,
    age_group_sizes = age_group_sizes,
    death_rates = death_rates,
    tt_death_rates = tt_death_rates,
    birth_rates = birth_rates,
    tt_birth_rates = tt_birth_rates,
    force_of_infection = force_of_infection,
    tt_force_of_infection = tt_force_of_infection,
    vaccine_efficacy = vaccine_efficacy,
    vaccine_efficacy_disease = vaccine_efficacy_disease,
    vaccinations = vaccinations,
    tt_vaccinations = tt_vaccinations,
    duration_of_immunity = duration_of_immunity,
    duration_of_maternal_immunity = duration_of_maternal_immunity,
    duration_of_pre_infectious = duration_of_pre_infectious,
    duration_of_infectious = duration_of_infectious,
    S_0 = S_0,
    I_0 = I_0,
    R_0 = R_0,
    M_0 = M_0,
    additional_parameters = additional_parameters
  )


  vaccine_immune <- format_output(res, "Immune", reduce_age = FALSE) %>%
    dplyr::filter(age_group == 3 & t == max(t)) %>%
    dplyr::pull(value) %>%
    sum()
  vaccine_immune_disease <- format_output(res, "Immune(Disease)", reduce_age = FALSE) %>%
    dplyr::filter(age_group == 3 & t == max(t)) %>%
    dplyr::pull(value) %>%
    sum()
  pop <- format_output(res, "Population", reduce_age = FALSE) %>%
    dplyr::filter(age_group == 3 & t == max(t)) %>%
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