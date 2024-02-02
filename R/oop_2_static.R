#' Define the static_model model class
#' @noRd
setClass(
    "static_model",
    contains = "IVODE_model"
)
#' Method for gz to format death rate
#' @noRd
setMethod(
    "format_death_rate",
    signature(type = "static_model"),
    function(type, pars_list, death_rate, tt_death_rate) {
        return(format_time_par(type, pars_list, death_rate, tt_death_rate, "crude_death_rate"))
    }
)
#' Method for gz to format birth rate
#' @noRd
setMethod(
    "format_birth_rate",
    signature(type = "static_model"),
    function(type, pars_list, birth_rate, tt_birth_rate) {
        pars_list <- format_time_par(type, pars_list, birth_rate, tt_birth_rate, "crude_birth_rate")
        #determine which age groups are child bearing
        age_sizes <- 1/pars_list$age_rate
        pars_list$child_bearing <- as.numeric(cumsum(age_sizes) >= (15*365) & cumsum(age_sizes) <= (45*365))
        return(pars_list)
    }
)
#' Method for gz to format foi
#' @noRd
setMethod(
    "format_foi",
    signature(type = "static_model"),
    function(type, pars_list, foi, tt_foi) {
        if(is.null(foi)){
            foi <- 0
            tt_foi <- 0
        } else {
            if(any(foi > 1)){
            stop("As static model foi is risk of infection, must be less than 1")
            }
        }
        pars_list <- format_time_par(type, pars_list, foi, tt_foi, "crude_foi")
        return(pars_list)
    }
)
#' Default to format maternal waning
#' @noRd
setMethod(
    "format_maternal_waning",
    signature(type = "static_model"),
    function(type, pars_list, duration_of_maternal_immunity) {
        age_group_sizes <- c(1/pars_list$age_rate, 0)
        #select endpoint closest to maternal immunity duration given
        age_group_ends <- cumsum(age_group_sizes)
        pars_list$n_maternal <- which.min(abs(age_group_ends - duration_of_maternal_immunity))
        if(abs(age_group_ends[pars_list$n_maternal] - duration_of_maternal_immunity) > 365/12) {
            warning(paste0(
                "simulated duration of natural immunity is ", age_group_ends[pars_list$n_maternal]*12/365,
                " months, not ", duration_of_maternal_immunity*12/365,
                " months, ensure this discprepancy is less than one month to remove this warning"
            ))
        }
        return(pars_list)
    }
)
#' format initial conditions for gaza model
#' @noRd
setMethod(
    "format_initial_conditions_M",
    signature(type = "static_model"),
    function(type, pars_list, M_0) {
        if (!is.null(M_0)) {
            warning("M_0 cannot currently be specified for this model type, will be initalised at 0")
        }
        pars_list$M_0 <- rep(0, pars_list$n_maternal)
        return(pars_list)
    }
)
#' Gaza specific
#' @noRd
setMethod(
    "format_vaccinations",
    signature(type = "static_model"),
    function(type, pars_list, vaccinations, tt_vaccinations) {
        if (is.null(tt_vaccinations)) {
            tt_vaccinations <- 0
            #convert par to matrix
            vaccinations <- matrix(vaccinations, nrow = 1)
        }
        check_format_percentage(vaccinations)
        #convert from coverage to rate

        vaccine_efficacy <- pars_list$vaccine_efficacy
        vaccine_efficacy_disease <- pars_list$vaccine_efficacy_disease
        
        vaccine_efficacy_disease_adjusted <- (vaccine_efficacy_disease - vaccine_efficacy) / (1 - vaccine_efficacy)

        vaccinations_partial <- vaccinations_complete <- vaccinations

        for (t in 1:nrow(vaccinations)) {
            vaccinations_complete[t, ] <- vaccinations[t, ] * vaccine_efficacy
            vaccinations_partial[t, ] <- vaccinations[t, ] * (1-vaccine_efficacy) * vaccine_efficacy_disease_adjusted
        }

        pars_list$vaccine_efficacy <- pars_list$vaccine_efficacy_disease <- NULL

        pars_list$vaccination_coverage <- vaccinations_complete
        pars_list$vaccination_partial_coverage <- vaccinations_partial
        pars_list$tt_vaccination_coverage <- tt_vaccinations
        return(pars_list)
    }
)
#' gz method to format vaccine doses
#' @noRd
setMethod(
    "format_additional",
    signature(type = "static_model"),
    function(type, pars_list, additional_parameters) {
        if(!"prop_death" %in% names(additional_parameters)){
            warning("Please define additional_parameters$prop_death, which is the relative risk of death for each age group, assuming this is flat")
            prop_death <- rep(1, length(pars_list$age_group_sizes) + 1)
        }
        prop_death <- additional_parameters$prop_death
        check_format_age_group_par_no_tt(prop_death, pars_list$n_age)
        pars_list$prop_death <- prop_death
        if(!"adjust_for_crude_foi" %in% names(additional_parameters)){
            pars_list$adjust_for_crude_foi <- 0
        } else {
            pars_list$adjust_for_crude_foi <- as.numeric(additional_parameters$adjust_for_crude_foi)
        }

        return(pars_list)
    }
)