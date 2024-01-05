#' Convert vaccine parameters into a list of parameters for each vaccine
#' @noRd
collate_parameters <- function(
    vaccine_names,
    force_of_infection,
    tt_force_of_infection,
    vaccine_efficacy,
    vaccinations,
    tt_vaccinations,
    duration_of_immunity,
    S_0,
    R_0,
    M_0,
    type,
    t,
    age_group_sizes,
    death_rates,
    tt_death_rates,
    birth_rates,
    tt_birth_rates,
    duration_of_maternal_immunity
) {
    #check vaccine parameters are in the right format
    check_list_format(force_of_infection, vaccine_names)
    check_list_format(tt_force_of_infection, vaccine_names)
    check_list_format(vaccine_efficacy, vaccine_names)
    check_list_format(vaccinations, vaccine_names)
    check_list_format(tt_vaccinations, vaccine_names)
    check_list_format(duration_of_immunity, vaccine_names)
    check_list_format(S_0, vaccine_names)
    check_list_format(R_0, vaccine_names)
    check_list_format(M_0, vaccine_names)
    #pivot the lists
    names(vaccine_names) <- vaccine_names
    purrr::map(
        vaccine_names, ~list(
            force_of_infection = force_of_infection[[.x]],
            tt_force_of_infection = tt_force_of_infection[[.x]],
            vaccine_efficacy = vaccine_efficacy[[.x]],
            vaccinations = vaccinations[[.x]],
            tt_vaccinations = tt_vaccinations[[.x]],
            duration_of_immunity = duration_of_immunity[[.x]],
            S_0 = S_0[[.x]],
            R_0 = R_0[[.x]],
            M_0 = M_0[[.x]],
            type = type,
            t = t,
            age_group_sizes = age_group_sizes,
            death_rates = death_rates,
            tt_death_rates = tt_death_rates,
            birth_rates = birth_rates,
            tt_birth_rates = tt_birth_rates,
            duration_of_maternal_immunity = duration_of_maternal_immunity
        )
    )
}
#' Convience function to drop the first row of a projection
#' @noRd
drop_first_row_output <- function(object){
    object@output <- object@output[-1, ]
    object
}

#' Function to project immunity using point estimates of the parameters
#' 
#' Runs multiple models for each vaccine type (determined by the first dimension of relevant parameters
#' @param type Type of model to run
#' @param t_projection_starts Time to start projections
#' @param t_projection_ends Time to end projections
#' @param age_group_sizes Vector of age group sizes
#' @param death_rates Vector of death rates
#' @param tt_death_rates Vector of time varying death rates
#' @param birth_rates Vector of birth rates
#' @param tt_birth_rates Vector of time varying birth rates
#' @param duration_of_maternal_immunity Duration of maternal immunity
#' @param additional_parameters List of additional parameters,default NULL
#' @param vaccine_names Vector of vaccine names
#' @param force_of_infection list of named vectors of force of infection
#' @param tt_force_of_infection list of named vectors of time varying force of infection
#' @param vaccine_efficacy list of named vectors of vaccine efficacy
#' @param vaccinations list of named vectors of vaccine dose/rate parameters
#' @param tt_vaccinations list of named vectors of time varying vaccine doses
#' @param duration_of_immunity list of named vectors of duration of immunity
#' @param S_0 list of named vectors of initial susceptibles
#' @param R_0 list of named vectors of initial recovered
#' @param M_0 list of named vectors of initial maternal immunity
#' 
#' @export
project_point_estimate <- function(
    type,
    t_projection_starts,
    t_projection_ends,
    age_group_sizes,
    death_rates,
    tt_death_rates = NULL,
    birth_rates,
    tt_birth_rates = NULL,
    duration_of_maternal_immunity,
    additional_parameters = NULL,
    vaccine_names,
    force_of_infection,
    tt_force_of_infection = NULL,
    vaccine_efficacy,
    vaccinations,
    tt_vaccinations = NULL,
    duration_of_immunity,
    S_0,
    R_0,
    M_0
) {
    n_age <- length(age_group_sizes) + 1
    
    ##first we check the demographic data holds up (returns a plot for comparison)
    #dummy variables
    total_population <- S_0[[1]] + R_0[[1]]
    total_population[1:2] <- total_population[1:2] + M_0[[1]]
    demographics <- simulate(
        type = type,
        t = seq(0, t_projection_starts),
        age_group_sizes = age_group_sizes,
        death_rates = death_rates,
        tt_death_rates = tt_death_rates,
        birth_rates = birth_rates,
        tt_birth_rates = tt_birth_rates,
        force_of_infection = rep(0, n_age),
        tt_force_of_infection = NULL,
        vaccine_efficacy = 0,
        vaccinations = rep(0, n_age),
        tt_vaccinations = NULL,
        duration_of_immunity = 100,
        duration_of_maternal_immunity = duration_of_maternal_immunity,
        S_0 = total_population,
        R_0 = rep(0, n_age),
        M_0 = rep(0, 2)
    ) %>%
        format_output(
            "Population"
        )
    message("Once projections are completed please check fit quality via $demographics")

    #collate parameters
    pars_list <- collate_parameters(
        vaccine_names = vaccine_names,
        force_of_infection,
        tt_force_of_infection = tt_force_of_infection,
        vaccine_efficacy = vaccine_efficacy,
        vaccinations = vaccinations,
        tt_vaccinations = tt_vaccinations,
        duration_of_immunity = duration_of_immunity,
        duration_of_maternal_immunity = duration_of_maternal_immunity,
        type = type,
        t = c(0, seq(t_projection_starts, t_projection_ends)),
        age_group_sizes = age_group_sizes,
        death_rates = death_rates,
        tt_death_rates = tt_death_rates,
        birth_rates = birth_rates,
        tt_birth_rates = tt_birth_rates,
        S_0 = S_0,
        R_0 = R_0,
        M_0 = M_0
    )

    #now simulate for each vaccine
    projections <- purrr::map_dfr(pars_list, ~do.call(simulate, .x) %>%
        drop_first_row_output() %>%
        format_output(
            c("Immune", "Population"), reduce_age = TRUE
        ), .id = "vaccine_type"
    )

    #population shouldn't change with vaccine type of we'll check thats the same
    check_pop <- projections %>%
        dplyr::filter(.data$compartment == "Population") %>%
        dplyr::arrange(.data$t) %>%
        dplyr::group_by(.data$t) %>%
        dplyr::mutate(value = as.integer(round(.data$value/max(.data$value), digits = 4) * 10^4)) %>%
        dplyr::select(!c("compartment", "vaccine_type")) %>%
        unique() %>%
        dplyr::filter(dplyr::n() > 1)
    
    if (nrow(check_pop) > 0) {
        stop("Warning: Population is not constant across vaccine types, please check previous message this could be due to large numbers")
    } else {
        projections <- projections %>%
            tidyr::pivot_wider(names_from = "compartment", values_from = "value")
    }

    list(
        projections = projections,
        demographics = demographics
    )
}
