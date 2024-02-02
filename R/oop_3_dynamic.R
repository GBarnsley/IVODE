#' Define the static_model model class
#' @noRd
setClass(
    "dynamic_model",
    contains = "static_model"
)
#' Method for dynamic to format foi
#' @noRd
setMethod(
    "format_foi",
    signature(type = "dynamic_model"),
    function(type, pars_list, foi, tt_foi) {
        if(is.null(foi)){
            foi <- 0
            tt_foi <- 0
        } else {
            if(any(foi < 1)){
                warning("As dynamic model foi is R0, expecting number greater than 1, use NULL to avoid this warning")
            }
        }
        pars_list <- format_time_par(type, pars_list, foi, tt_foi, "R0")
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
#' format initial conditions for gaza model
#' @noRd
setMethod(
    "format_initial_conditions_I",
    signature(type = "dynamic_model"),
    function(type, pars_list, I_0) {
        format_initial_conditions(type, pars_list, I_0, "I")
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
#' dynamic specific matchings
#' @noRd
setMethod(
    "get_matchings",
    signature(type = "dynamic_model"),
    function(type) {
        list(
            Susceptible = c("S", "VD"),
            Immune = c("V", "R", "M"),
            `Immune(Acquired)` = "R",
            `Immune(Vaccine)` = "V",
            `Immune(Maternal)` = "M",
            `Immune(Disease)` = c("V", "R", "M", "VD"),
            Doses = "vaccination_doses",
            Population = c("S", "R", "M", "V", "VD", "I", "E")
        )
    }
)