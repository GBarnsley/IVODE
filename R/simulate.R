#' Run model
#' 
#' @param type Type of odin model to run
#' @param t Time-series of times to collect data from
#' @param age_group_sizes Sizes of each age group
#' @param death_rates Death rates for each age group
#' @param tt_death_rates Times at which death rates change
#' @param birth_rates Birth rates for each age group
#' @param tt_birth_rates Times at which birth rates change
#' @param force_of_infection Force of infection for each age group
#' @param tt_force_of_infection Times at which force of infection changes
#' @param vaccine_efficacy Efficacy of vaccine
#' @param vaccine_efficacy_disease Efficacy of vaccine against disease, should be the total efficacy not adjusted for protection against infection
#' @param vaccinations Doses or rates of vaccine given each day
#' @param tt_vaccinations Times at which vaccination rate changes
#' @param duration_of_immunity Duration of protection (in days) from vaccines and natural immunity
#' @param duration_of_maternal_immunity Duration of maternal immunity
#' @param S_0 Initial susceptible population
#' @param R_0 Initial recovered population
#' @param M_0 Initial maternal immunity population
#' @param additional_parameters List of additional parameters
#' @return output array of results
#' @export
simulate <- function(
    type,
    t,
    age_group_sizes,
    death_rates,
    tt_death_rates = NULL,
    birth_rates,
    tt_birth_rates = NULL,
    force_of_infection,
    tt_force_of_infection = NULL,
    vaccine_efficacy,
    vaccine_efficacy_disease,
    vaccinations,
    tt_vaccinations = NULL,
    duration_of_immunity,
    duration_of_maternal_immunity,
    S_0,
    R_0,
    M_0,
    additional_parameters = NULL
    ) {
    
    type_class <- call_type(type)

    #check formats of the parameters
    check_format_t(t)
    check_format_age_group_sizes(age_group_sizes)

    n_age <- length(age_group_sizes) + 1

    #check_format_age_group_par(death_rates, tt_death_rates, n_age) #need to OOP this
    check_format_tt(tt_death_rates, t)

    #check_format_age_group_par(birth_rates, tt_birth_rates, n_age) #need to OOP this
    check_format_tt(tt_birth_rates, t)

    #check_format_age_group_par(force_of_infection, tt_force_of_infection, n_age) #oop this
    check_format_tt(tt_force_of_infection, t)

    check_format_percentage(vaccine_efficacy)

    check_format_percentage(vaccine_efficacy_disease)

    check_efficacy_consistency(vaccine_efficacy, vaccine_efficacy_disease)

    #check_format_age_group_par(vaccinations, tt_vaccinations, n_age)
    check_format_tt(tt_vaccinations, t)

    check_duration(duration_of_immunity)
    check_duration(duration_of_maternal_immunity)

    check_initial_conditions(S_0, n_age)
    check_initial_conditions(R_0, n_age)
    #check_initial_conditions(M_0, 2) oop this

    #format variables for model
    pars_list <- list(n_age = n_age)

    pars_list <- format_age_rates(type_class, pars_list, age_group_sizes)

    pars_list <- format_death_rate(type_class, pars_list, death_rates, tt_death_rates)

    pars_list <- format_birth_rate(type_class, pars_list, birth_rates, tt_birth_rates)

    pars_list <- format_foi(type_class, pars_list, force_of_infection, tt_force_of_infection)

    pars_list <- format_vaccine_efficacy(type_class, pars_list, vaccine_efficacy)

    pars_list <- format_vaccine_efficacy_disease(type_class, pars_list, vaccine_efficacy_disease)
    
    pars_list <- format_waning(type_class, pars_list, duration_of_immunity)

    pars_list <- format_maternal_waning(type_class, pars_list, duration_of_maternal_immunity)

    pars_list <- format_initial_conditions(type_class, pars_list, S_0, "S")

    pars_list <- format_initial_conditions(type_class, pars_list, R_0, "R")

    pars_list <- format_initial_conditions_M(type_class, pars_list, M_0)

    pars_list <- format_vaccinations(type_class, pars_list, vaccinations, tt_vaccinations)
    
    pars_list <- format_additional(type_class, pars_list, additional_parameters)

    type_class@parameters <- pars_list

    model_instance <- do.call(type_class@model_function, type_class@parameters)
    output <- model_instance$run(t)

    type_class@output <- output
    
    type_class
}