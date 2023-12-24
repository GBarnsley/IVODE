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