#' Check the format of t
#' @noRd
check_format_t <- function(t) {
    if (!is.vector(t)) {
        stop("t must be a vector")
    }
    if (!is.numeric(t)) {
        stop("t must be numeric")
    }
    if (any(diff(t) <= 0)) {
        stop("t strictly increasing")
    }
}
#' Check the format of age_group_sizes
#' @noRd
check_format_age_group_sizes <- function(age_group_sizes) {
    if (!is.vector(age_group_sizes)) {
        stop("age_group_sizes must be a vector")
    }
    if (!is.numeric(age_group_sizes)) {
        stop("age_group_sizes must be numeric")
    }
    if (any(age_group_sizes < 0)) {
        stop("age_group_sizes must be positive")
    }
    if (length(age_group_sizes) <= 2) {
        stop("age_group_sizes must have more than two entries (the first two ages can have maternal immunity)")
    }
}
#' Check the format of age_group_par
#' @noRd
check_format_age_group_par <- function(par, tt_par, n_age) {
    par_name <- deparse(substitute(par))
    if (!is.numeric(par)) {
        stop(paste(par_name, "must be numeric"))
    }
    if (any(par < 0)) {
        stop(paste(par_name, "must be positive"))
    }
    if (!is.null(tt_par)) {
         if (!is.matrix(par)) {
            stop(paste(par_name, "must be a matrix"))
        }
        n_t <- length(tt_par)
        if (dim(par) != c(n_age, n_t)) {
            stop(paste0(par_name, " must have the same number of columns as tt_", par_name, " and the same number of rows as n_age"))
        }
    } else {
        if (!is.vector(par)) {
            stop(paste(par_name, "must be a vector"))
        }
        if (length(par) != n_age) {
            stop(paste(par_name, "must have the same number of entries as n_age"))
        }
    }
}
#' Check the format of tt_par
#' @noRd
check_format_tt <- function(tt_par, t) {
    tt_par_name <- deparse(substitute(tt_par))
    if (!is.null(tt_par)) {
        if (!is.vector(tt_par)) {
            stop(paste(tt_par_name, "must be a vector"))
        }
        if (!is.numeric(tt_par)) {
            stop(paste(tt_par_name, "must be numeric"))
        }
        if (any(diff(tt_par) <= 0)) {
            stop(paste(tt_par_name, "must be strictly increasing"))
        }
        if (tt_par[1] > t[1]) {
            stop(paste(tt_par_name, "must be less than or equal to t[1]"))
        }
    }
}
#' Check the format of percentage
#' @noRd
check_format_percentage <- function(percentage) {
    percentage_name <- deparse(substitute(percentage))
    if (!is.numeric(percentage)) {
        stop(paste(percentage_name, "must be numeric"))
    }
    if (any(percentage < 0 | percentage > 1)) {
        stop(paste(percentage_name, "must be between 0 and 1, inclusive"))
    }
}
#' Check the format of duration
#' @noRd
check_duration <- function(duration) {
    duration_name <- deparse(substitute(duration))
    if (!is.numeric(duration)) {
        stop(paste(duration_name, "must be numeric"))
    }
    if (any(duration < 0)) {
        stop(paste(duration_name, "must be positive"))
    }
}
#' Check the format of initial conditions
#' @noRd
check_initial_conditions <- function(initial_conditions, n_size) {
    initial_conditions_name <- deparse(substitute(initial_conditions))
    if (!is.vector(initial_conditions)) {
        stop(paste(initial_conditions_name, "must be a vector"))
    }
    if (!is.numeric(initial_conditions)) {
        stop(paste(initial_conditions_name, "must be numeric"))
    }
    if (length(initial_conditions) != n_size) {
        stop(paste(initial_conditions_name, "must have", n_size, "entries"))
    }
    if (any(initial_conditions < 0)) {
        stop(paste(initial_conditions_name, "must be positive"))
    }
}