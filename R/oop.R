#' Function to get generate an object of given types class
#' @noRd
call_type <- function(type) {
    obj <- list(
        model_function = get(type)$new, #this might be a little iffy?
        parameters = list()
    )
    class(obj) <- c(type, "IVODE_model")
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
    signature(type = 'ANY'),
    function(type, pars_list, age_group_sizes) {
        pars_list$age_rate <- c(1/age_group_sizes, 0)
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
    signature(type = 'ANY'),
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
    signature(type = 'ANY'),
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
    signature(type = 'ANY'),
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
    signature(type = 'ANY'),
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
    signature(type = 'ANY'),
    function(type, pars_list, initial_conditions, name) {
        pars_list[[paste0(name, "_0")]] <- initial_conditions
        return(pars_list)
    }
)
#' Format vaccine doses
#' @noRd
setGeneric(
    "format_vaccine_doses",
    function(type, pars_list, vaccine_doses, tt_vaccine_doses) {
        standardGeneric("format_vaccine_doses")
    }
)
#' Default to format vaccine doses
#' @noRd
setMethod(
    "format_vaccine_doses",
    signature(type = 'ANY'),
    function(type, pars_list, vaccine_doses, tt_vaccine_doses) {
        if (is.null(tt_vaccine_doses)) {
            tt_vaccine_doses <- 0
            #convert par to matrix
            vaccine_doses <- matrix(vaccine_doses, nrow = 1)
        }
        pars_list$vaccine_doses <- vaccine_doses
        pars_list$tt_vaccine_doses <- tt_vaccine_doses
        return(pars_list)
    }
)