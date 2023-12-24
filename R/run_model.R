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
#' @param vaccination_coverage Coverage of vaccine
#' @param tt_vaccination_coverage Times at which vaccination rate changes
#' @param duration_of_immunity Duration of protection (in days) from vaccines and natural immunity
#' @param duration_of_maternal_immunity Duration of maternal immunity
#' @param S_0 Initial susceptible population
#' @param R_0 Initial recovered population
#' @param M_0 Initial maternal immunity population
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
    vaccination_coverage,
    tt_vaccination_coverage = NULL,
    duration_of_immunity,
    duration_of_maternal_immunity,
    S_0,
    R_0,
    M_0
    ){
    
    type_class <- call_type(type)

    #check formats of the parameters
    check_format_t(t)
    check_format_age_group_sizes(age_group_sizes)

    n_age <- length(age_group_sizes) + 1

    check_format_age_group_par(death_rates, tt_death_rates, n_age)
    check_format_tt(tt_death_rates, t)

    check_format_age_group_par(birth_rates, tt_birth_rates, n_age)
    check_format_tt(tt_birth_rates, t)

    check_format_age_group_par(force_of_infection, tt_force_of_infection, n_age)
    check_format_tt(tt_force_of_infection, t)

    check_format_percentage(vaccine_efficacy)

    check_format_age_group_par(vaccination_coverage, tt_vaccination_coverage, n_age)
    check_format_percentage(vaccination_coverage)
    check_format_tt(tt_vaccination_coverage, t)

    check_duration(duration_of_immunity)
    check_duration(duration_of_maternal_immunity)

    check_initial_conditions(S_0, n_age)
    check_initial_conditions(R_0, n_age)
    check_initial_conditions(M_0, 2)

    #format variables for model
    




    #n_age = n_age,
    #age_rate = age_rate,
    #death_rate = death_rate,
    #birth_rate = birth_rate,
    #child_bearing_ages = child_bearing_ages,
    #maternal_waning = maternal_waning,
    #foi = foi,
    #vaccine_efficacy = vaccine_efficacy,
    #vaccination_rate = vaccination_rate,
    #waning = waning,
    #M_0 = M_0,
    #S_0 = S_0,
    #R_0 = R_0,
    
    
}