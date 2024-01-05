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
#' Define the deterministic model class
#' @noRd
setClass(
    "deterministic",
    contains = "IVODE_model"
)
#' Define the deterministic_gz model class
#' @noRd
setClass(
    "deterministic_gz",
    contains = "IVODE_model"
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
            age_group_par <- matrix(age_group_par, nrow = 1)
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
    signature(type = "deterministic_gz"),
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
    signature(type = "deterministic_gz"),
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
#' Method for gz to format birth rate
#' @noRd
setMethod(
    "format_foi",
    signature(type = "deterministic_gz"),
    function(type, pars_list, foi, tt_foi) {
        pars_list <- format_time_par(type, pars_list, foi, tt_foi, "crude_foi")
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
    signature(type = "deterministic_gz"),
    function(type, pars_list, vaccinations, tt_vaccinations) {
        if (is.null(tt_vaccinations)) {
            tt_vaccinations <- 0
            #convert par to matrix
            vaccinations <- matrix(vaccinations, nrow = 1)
        }
        pars_list$vaccination_rate <- vaccinations
        pars_list$tt_vaccination_rate <- tt_vaccinations
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
#' Default to format vaccine doses
#' @noRd
setMethod(
    "format_additional",
    signature(type = "IVODE_model"),
    function(type, pars_list, additional_parameters) {
        if(!is.null(additional_parameters)) {
            stop("additional_parameters not defined for this model type")
        }
        return(pars_list)
    }
)
#' gz method to format vaccine doses
#' @noRd
setMethod(
    "format_additional",
    signature(type = "deterministic_gz"),
    function(type, pars_list, additional_parameters) {
        if(!"prop_death" %in% names(additional_parameters)){
            stop("Please define additional_parameters$prop_death, which is the relative risk of death for each age group")
        }
        prop_death <- additional_parameters$prop_death
        check_format_age_group_par_no_tt(prop_death, pars_list$n_age)
        pars_list$prop_death <- prop_death
        return(pars_list)
    }
)