#' Define the IVODE_model class
#' @noRd
setClass(
    "IVODE_model",
    representation(
        model_function = "function",
        parameters = "list",
        output = "array"
    )
)
#' Define the static_model model class
#' @noRd
setClass(
    "static_model",
    contains = "IVODE_model"
)
#' Define the static_model model class
#' @noRd
setClass(
    "dynamic_model",
    contains = "static_model"
)
#' Function to get generate an object of given types class
#' @noRd
call_type <- function(type) {
    obj <- new(type, model_function = get(type)$new)
    return(obj)
}
#' Format the age_group_sizes to age rates
#' @noRd
setGeneric(
    "format_age_rates",
    function(type, pars_list, age_group_sizes) {
        standardGeneric("format_age_rates")
    }
)
#' Default to format the age_group_sizes to age rates
#' @noRd
setMethod(
    "format_age_rates",
    signature(type = "IVODE_model"),
    function(type, pars_list, age_group_sizes) {
        pars_list$age_rate <- c(1/age_group_sizes, 0)
        return(pars_list)
    }
)
#' Format the time_par to time changing rates
#' @noRd
setGeneric(
    "format_time_par",
    function(type, pars_list, time_par, tt_time_par, par_name) {
        standardGeneric("format_time_par")
    }
)
#' Default to format the age_group_par to time changing rates
#' @noRd
setMethod(
    "format_time_par",
    signature(type = "IVODE_model"),
    function(type, pars_list, time_par, tt_time_par, par_name) {
        if (is.null(tt_time_par)) {
            tt_time_par <- 0
        }
        pars_list[[par_name]] <- time_par
        pars_list[[paste0("tt_", par_name)]] <- tt_time_par
        return(pars_list)
    }
)
#' Format the age_group_par to time changing rates
#' @noRd
setGeneric(
    "format_age_group_par",
    function(type, pars_list, age_group_par, tt_age_group_par, par_name) {
        standardGeneric("format_age_group_par")
    }
)
#' Default to format the age_group_par to time changing rates
#' @noRd
setMethod(
    "format_age_group_par",
    signature(type = "IVODE_model"),
    function(type, pars_list, age_group_par, tt_age_group_par, par_name) {
        if (is.null(tt_age_group_par)) {
            tt_age_group_par <- 0
            #convert par to matrix
            age_group_par <- matrix(age_group_par, nrow = 1, ncol = pars_list$n_age)
        }
        pars_list[[par_name]] <- age_group_par
        pars_list[[paste0("tt_", par_name)]] <- tt_age_group_par
        return(pars_list)
    }
)
#' Format death rate
#' @noRd
setGeneric(
    "format_death_rate",
    function(type, pars_list, death_rate, tt_death_rate) {
        standardGeneric("format_death_rate")
    }
)
#' Default to format death rate
#' @noRd
setMethod(
    "format_death_rate",
    signature(type = "IVODE_model"),
    function(type, pars_list, death_rate, tt_death_rate) {
        return(
            format_age_group_par(
                type,
                pars_list,
                death_rate,
                tt_death_rate,
                "death_rate"
            )
        )
    }
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
#' Format birth rate
#' @noRd
setGeneric(
    "format_birth_rate",
    function(type, pars_list, birth_rate, tt_birth_rate) {
        standardGeneric("format_birth_rate")
    }
)
#' Default to format birth rate
#' @noRd
setMethod(
    "format_birth_rate",
    signature(type = "IVODE_model"),
    function(type, pars_list, birth_rate, tt_birth_rate) {
        return(
            format_age_group_par(
                type,
                pars_list,
                birth_rate,
                tt_birth_rate,
                "birth_rate"
            )
        )
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
#' Format foi
#' @noRd
setGeneric(
    "format_foi",
    function(type, pars_list, foi, tt_foi) {
        standardGeneric("format_foi")
    }
)
#' Default to format birth rate
#' @noRd
setMethod(
    "format_foi",
    signature(type = "IVODE_model"),
    function(type, pars_list, foi, tt_foi) {
        return(
            format_age_group_par(
                type,
                pars_list,
                foi,
                tt_foi,
                "foi"
            )
        )
    }
)
#' Method for gz to format foi
#' @noRd
setMethod(
    "format_foi",
    signature(type = "static_model"),
    function(type, pars_list, foi, tt_foi) {
        if(any(foi > 1)){
            stop("As static model foi is risk of infection, must be less than 1")
        }
        pars_list <- format_time_par(type, pars_list, foi, tt_foi, "crude_foi")
        return(pars_list)
    }
)
#' Method for dynamic to format foi
#' @noRd
setMethod(
    "format_foi",
    signature(type = "dynamic_model"),
    function(type, pars_list, foi, tt_foi) {
        if(any(foi < 1)){
            warning("As dynamic model foi is R0, expecting number greater than 1")
        }
        pars_list <- format_time_par(type, pars_list, foi, tt_foi, "R0")
        return(pars_list)
    }
)
#' Format vaccine efficacy
#' @noRd
setGeneric(
    "format_vaccine_efficacy",
    function(type, pars_list, vaccine_efficacy) {
        standardGeneric("format_vaccine_efficacy")
    }
)
#' Default to format vaccine efficacy
#' @noRd
setMethod(
    "format_vaccine_efficacy",
    signature(type = "IVODE_model"),
    function(type, pars_list, vaccine_efficacy) {
        pars_list$vaccine_efficacy <- vaccine_efficacy
        return(pars_list)
    }
)
#' Format vaccine efficacy disease
#' @noRd
setGeneric(
    "format_vaccine_efficacy_disease",
    function(type, pars_list, vaccine_efficacy_disease) {
        standardGeneric("format_vaccine_efficacy_disease")
    }
)
#' Default to format vaccine efficacy disease
#' @noRd
setMethod(
    "format_vaccine_efficacy_disease",
    signature(type = "IVODE_model"),
    function(type, pars_list, vaccine_efficacy_disease) {
        pars_list$vaccine_efficacy_disease <- vaccine_efficacy_disease
        return(pars_list)
    }
)
#' Format waning
#' @noRd
setGeneric(
    "format_waning",
    function(type, pars_list, duration_of_immunity) {
        standardGeneric("format_waning")
    }
)
#' Default to format waning
#' @noRd
setMethod(
    "format_waning",
    signature(type = "IVODE_model"),
    function(type, pars_list, duration_of_immunity) {
        pars_list$waning <- 1/duration_of_immunity
        return(pars_list)
    }
)
#' Format maternal waning
#' @noRd
setGeneric(
    "format_maternal_waning",
    function(type, pars_list, duration_of_maternal_immunity) {
        standardGeneric("format_maternal_waning")
    }
)
#' Default to format maternal waning
#' @noRd
setMethod(
    "format_maternal_waning",
    signature(type = "IVODE_model"),
    function(type, pars_list, duration_of_maternal_immunity) {
        pars_list$maternal_waning <- 1/duration_of_maternal_immunity
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
#' Format infection periods
#' @noRd
setGeneric(
    "format_infection_periods",
    function(type, pars_list, duration, name) {
        standardGeneric("format_infection_periods")
    }
)
#' Default to format infection periods
#' @noRd
setMethod(
    "format_infection_periods",
    signature(type = "IVODE_model"),
    function(type, pars_list, duration, name) {
        if(!is.null(duration)) {
            warning(paste0(name, " not defined for this model type, set to NULL to avoid this warning"))
        }
        return(pars_list)
    }
)
#' Default to format infection periods
#' @noRd
setMethod(
    "format_infection_periods",
    signature(type = "dynamic_model"),
    function(type, pars_list, duration, name) {
        pars_list[[paste0(name, "_transition_rate")]] <- 1/duration

        return(pars_list)
    }
)
#' Format initial conditions
#' @noRd
setGeneric(
    "format_initial_conditions",
    function(type, pars_list, initial_conditions, name) {
        standardGeneric("format_initial_conditions")
    }
)
#' Default to format initial conditions
#' @noRd
setMethod(
    "format_initial_conditions",
    signature(type = "IVODE_model"),
    function(type, pars_list, initial_conditions, name) {
        pars_list[[paste0(name, "_0")]] <- initial_conditions
        return(pars_list)
    }
)
#' Format initial conditions
#' @noRd
setGeneric(
    "format_initial_conditions_M",
    function(type, pars_list, M_0) {
        standardGeneric("format_initial_conditions_M")
    }
)
#' Default to format initial conditions
#' @noRd
setMethod(
    "format_initial_conditions_M",
    signature(type = "IVODE_model"),
    function(type, pars_list, M_0) {
        format_initial_conditions(type, pars_list, M_0, "M")
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
#' Format initial conditions
#' @noRd
setGeneric(
    "format_initial_conditions_I",
    function(type, pars_list, I_0) {
        standardGeneric("format_initial_conditions_I")
    }
)
#' Default to format initial conditions
#' @noRd
setMethod(
    "format_initial_conditions_I",
    signature(type = "IVODE_model"),
    function(type, pars_list, I_0) {
        if (!is.null(I_0)) {
            warning("I_0 not defined for this model, set to NULL to avoid this warning")
        }
        pars_list
    }
)
#' format initial conditions for gaza model
#' @noRd
setMethod(
    "format_initial_conditions_I",
    signature(type = "dynamic_model"),
    function(type, pars_list, I_0) {
        format_initial_conditions(type, pars_list, I_0, "I")
    }
)
#' Format vaccine doses
#' @noRd
setGeneric(
    "format_vaccinations",
    function(type, pars_list, vaccinations, tt_vaccinations) {
        standardGeneric("format_vaccinations")
    }
)
#' Default to format vaccine doses
#' @noRd
setMethod(
    "format_vaccinations",
    signature(type = "IVODE_model"),
    function(type, pars_list, vaccinations, tt_vaccinations) {
        if (is.null(tt_vaccinations)) {
            tt_vaccinations <- 0
            #convert par to matrix
            vaccinations <- matrix(vaccinations, nrow = 1)
        }
        pars_list$vaccine_doses <- vaccinations
        pars_list$tt_vaccine_doses<- tt_vaccinations
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
#' Format vaccine doses
#' @noRd
setGeneric(
    "format_additional",
    function(type, pars_list, additional_parameters) {
        standardGeneric("format_additional")
    }
)
#' gz method to format vaccine doses
#' @noRd
setMethod(
    "format_additional",
    signature(type = "IVODE_model"),
    function(type, pars_list, additional_parameters) {
        if(!is.null(additional_parameters)){
            warning("additional_parameters not defined for this model, set to NULL to avoid this warning")
        }
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
#' gz method to format vaccine doses
#' @noRd
setMethod(
    "format_additional",
    signature(type = "dynamic_model"),
    function(type, pars_list, additional_parameters) {
        if(!"prop_death" %in% names(additional_parameters)){
            warning("Please define additional_parameters$prop_death, which is the relative risk of death for each age group, assuming this is flat")
            prop_death <- rep(1, length(pars_list$age_group_sizes) + 1)
        }
        prop_death <- additional_parameters$prop_death
        check_format_age_group_par_no_tt(prop_death, pars_list$n_age)
        pars_list$prop_death <- prop_death
        return(pars_list)
    }
)
#' run actual model
#' @noRd
setGeneric(
    "call_model",
    function(type, t) {
        standardGeneric("call_model")
    }
)
#' Default to run model via odin
#' @noRd
setMethod(
    "call_model",
    signature(type = "IVODE_model"),
    function(type, t) {
        model_instance <- do.call(type@model_function, type@parameters)
        output <- model_instance$run(t)

        type@output <- output

        type
    }
)