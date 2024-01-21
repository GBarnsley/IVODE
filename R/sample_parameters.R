#' Function to sample R0, duration of infectiousness and pre-infectioussness for a given disease
#' @param disease Which disease to sample
#' @param n_samples number of samples to draw, if NULL takes the central value
#' @export
sample_parameters <- function(disease, n_samples = NULL) {
    parameters <- list( ##need to adjust the infectious max/min
        `bacterial dysentery` = list(pre_infectious = 2, R0_max = 2.2, R0_min = 1.1, infectious_max = 27, infectious_min = 7),
        cholera = list(pre_infectious = 1.4, R0_max = 2.73, R0_min = 1.11, infectious_max = 5, infectious_min = 3),
        diphtheria = list(pre_infectious = 1.4,R0_max = 7.1, R0_min = 1.7, infectious_max = 12.8, infectious_min = 5.2),
        `hepatitis A` = list(pre_infectious = 16, R0_max = 2.7, R0_min = 1.1, infectious_max = 21, infectious_min = 21),
        `hepatitis E` = list(pre_infectious = 34, R0_max = 8.5, R0_min = 2.11, infectious_max = 40, infectious_min = 21),
        measles = list(pre_infectious = 10, R0_max = 32, R0_min = 6, infectious_max = 8, infectious_min = 8),
        `meningococcal meningitis` = list(pre_infectious = 2, R0_max = 2.5, R0_min = 1.31, infectious_max = 12, infectious_min = 2),
        pertussis = list(pre_infectious = 9, R0_max = 17, R0_min = 5.5, infectious_max = 16, infectious_min = 5),
        `polio - vaccine-derived` = list(pre_infectious = 4, R0_max = 12, R0_min = 1.62, infectious_max = 16.8, infectious_min = 16.8),
        `polio - wildtype` = list(pre_infectious = 4, R0_max = 12, R0_min = 1.62, infectious_max = 16.8, infectious_min = 16.8),
        `typhoid fever` = list(pre_infectious = 10, R0_max = 2.8, R0_min = 2.8, infectious_max = 28, infectious_min = 20),
        #mine:
        `Hib disease` = list(pre_infectious = 1/1, R0_max = 3.275, R0_min = 3.275, infectious_min = 50, infectious_max = 80), #https://karger.com/neo/article-abstract/50/2/114/367324/Colonisation-of-Haemophilus-influenzae-and?redirectedFrom=PDF
        `pneumococcal disease` = list(pre_infectious = 1/1, R0_max = 3.275, R0_min = 3.275, infectious_min = 80, infectious_max = 100), #get better estimate of r0 https://karger.com/neo/article-abstract/50/2/114/367324/Colonisation-of-Haemophilus-influenzae-and?redirectedFrom=PDF
        rotavirus = list(pre_infectious = 2, R0_max = 30, R0_min = 40, infectious_min = 4, infectious_max = 8) #r0 seems very high
    )[[disease]]
    if(is.null(n_samples)){
        list(
            duration_of_pre_infectious = parameters$pre_infectious,
            R0 = mean(c(parameters$R0_max, min = parameters$R0_min)),
            duration_of_infectious = mean(c(parameters$infectious_max, min = parameters$infectious_min))
        )
    } else {
        list(
            duration_of_pre_infectious = rep(parameters$pre_infectious, n_samples),
            R0 = stats::runif(n_samples, max = parameters$R0_max, min = parameters$R0_min),
            duration_of_infectious = stats::runif(n_samples, max = parameters$infectious_max, min = parameters$infectious_min)
        )
    }
}