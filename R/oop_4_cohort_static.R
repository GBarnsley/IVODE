#' Define the static_model model class
#' @noRd
setClass(
    "cohort_static_model",
    contains = "static_model"
)
#' format initial conditions for cohort model
#' @noRd
setMethod(
    "format_initial_conditions_I",
    signature(type = "cohort_static_model"),
    function(type, pars_list, I_0) {
        pars_list <- format_initial_conditions(type, pars_list, rep(0, pars_list$n_age), "V")
        format_initial_conditions(type, pars_list, rep(0, pars_list$n_age), "VD")
    }
)
#' Default to format the age_group_sizes to age rates
#' @noRd
setMethod(
    "format_additional",
    signature(type = "cohort_static_model"),
    function(type, pars_list, additional_parameters) {
        if(!"cohort_size" %in% names(additional_parameters)) {
            stop("Please define additional_parameters$cohort_size")
        }
        pars_list$cohort_size <- additional_parameters$cohort_size

        pars_list <- format_additional(structure(list(), class = "static_model"), pars_list, additional_parameters)

        #derived parameters for the model
        initial_indexes <- list(
            S = 1 + seq(1, pars_list$n_age - 1),
            R = 1 + pars_list$n_age + seq(1, pars_list$n_age - 1),
            V = 1 + (2 * pars_list$n_age) + seq(1, pars_list$n_age - 1),
            VD = 1 + (3 * pars_list$n_age) + seq(1, pars_list$n_age - 1),
            M_t_S = 1 + (4 * pars_list$n_age) + pars_list$n_maternal
        )
        if(pars_list$n_maternal > 1) {
            initial_indexes$M <- 1 + (4 * pars_list$n_age) + seq(1, pars_list$n_maternal - 1)
        }

        update_parameters <- list(
            n_maternal = pars_list$n_maternal,
            vaccination_coverage = pars_list$vaccination_coverage,
            vaccination_partial_coverage = pars_list$vaccination_partial_coverage,
            tt_vaccination_coverage = pars_list$tt_vaccination_coverage
        )

        pars_list[c("age_rate", "vaccination_coverage", "vaccination_partial_coverage", "tt_vaccination_coverage")] <- NULL

        pars_list$initial_indexes <- initial_indexes
        pars_list$update_parameters <- update_parameters
        pars_list
    }
)
#' @noRd
update_parameters <- function(parameters, output, initial_indexes, n_maternal) {
    #apply ageing
    parameters$S_0 <- c(0, output[initial_indexes[["S"]]])
    parameters$R_0 <- c(0, output[initial_indexes[["R"]]])
    parameters$V_0 <- c(0, output[initial_indexes[["V"]]])
    parameters$VD_0 <- c(0, output[initial_indexes[["VD"]]])
    parameters$M_0 <- c(0, output[initial_indexes[["M"]]])
    parameters$S_0[n_maternal + 1] <- output[initial_indexes$M_t_S] + parameters$S_0[n_maternal + 1]
    #apply vaccination
    return(parameters)
}
#' Method to run cohort model
#' @noRd
setMethod(
    "call_model",
    signature(type = "cohort_static_model"),
    function(type, t) {
        cohort_update_times_end <- seq(0, max(t) + type@parameters$cohort_size, type@parameters$cohort_size)
        cohort_update_times_start <- cohort_update_times_end[-length(cohort_update_times_end)]
        cohort_update_times_end <- cohort_update_times_end[-1]
        #select parameters
        parameters <- type@parameters
        initial_indexes <- parameters$initial_indexes
        update_parameters <- parameters$update_parameters
        parameters[c("initial_indexes", "update_parameters", "cohort_size")] <- NULL

        model_instance <- do.call(type@model_function, parameters)
        actual_output <- NULL
        for (i in seq_along(cohort_update_times_start)) {
            this_t <- c(
                cohort_update_times_start[i],
                t[t >= cohort_update_times_start[i] & t < cohort_update_times_end[i]],
                cohort_update_times_end[i]
            )
            #run model
            do.call(model_instance$set_user, parameters)
            output <- model_instance$run(this_t)
            parameters <- update_parameters(parameters, output[nrow(output),], initial_indexes, parameters$n_maternal)
            if(length(this_t) > 2) {
                if(is.null(actual_output)) {
                    actual_output <- output[-c(1, nrow(output)),]
                } else {
                    actual_output <- rbind(actual_output, output[-c(1, nrow(output)),])
                }
            }
        }

        type@output <- actual_output

        type
    }
)