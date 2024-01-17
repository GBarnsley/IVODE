#' Format the output of simulate
#' @param output output of simulate
#' @param compartments list of compartments you want to return
#' @param reduce_age logical, if TRUE, summarise the age groups
#' @return data frame of results
#' @export
setGeneric(
    "format_output",
    function(output, compartments, reduce_age = FALSE) {
        standardGeneric("format_output")
    }
)
#' Default to format output
#' @inheritParams format_output
#' @export
setMethod(
    "format_output",
    signature(output = 'ANY'),
    function(output, compartments, reduce_age) {
        #check requested compartments
        available_compartments <- c("Susceptible", "Immune", "Immune(Acquired)", "Immune(Vaccine)", "Immune(Maternal)", "Immune(Disease)", "Doses", "Population")
        if (any(!compartments %in% available_compartments)) {
            stop(paste("compartments must be one of", paste0(available_compartments, collapse = ", ")))
        }

        #match to compartment names
        matchings <- list(
            Susceptible = c("S", "VD"),
            Immune = c("V", "R", "M"),
            `Immune(Acquired)` = "R",
            `Immune(Vaccine)` = "V",
            `Immune(Maternal)` = "M",
            `Immune(Disease)` = c("V", "R", "M", "VD"),
            Doses = "vaccination_doses",
            Population = c("S", "R", "M", "V", "VD")
        )
        #convert output to correct format
        n_age <- output@parameters$n_age
        reformatted_output <- as.data.frame(output@output) %>%
            tidyr::pivot_longer(cols = !t, names_to = "raw_compartment", values_to = "value") %>%
            dplyr::mutate(
                age_group = as.numeric(stringr::str_extract(.data$raw_compartment, "([0-9]+)")),
                compartment = stringr::str_replace(.data$raw_compartment, "[0-9\\[\\]]+", "")
            ) %>%
            dplyr::select(!"raw_compartment") %>%
            dplyr::group_by(.data$t, .data$age_group)

        #create data frame
        new_output <- purrr::map_dfr(matchings[compartments], function(matches, reformatted_output) {
            reformatted_output %>%
                dplyr::filter(.data$compartment %in% matches) %>%
                dplyr::summarise(
                    value = sum(.data$value),
                    .groups = "drop_last"
                )
        }, reformatted_output, .id = "compartment")

        if(reduce_age){
            new_output <- new_output %>%
                dplyr::group_by(.data$t, .data$compartment) %>%
                dplyr::summarise(
                    value = sum(.data$value),
                    .groups = "drop"
                )
        } else {
            new_output <- new_output %>%
                dplyr::ungroup()
        }

        return(new_output)
    }
)