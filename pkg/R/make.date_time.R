make.date_time <- function(add_vec = 1:7, var_start = Sys.Date(), var_form = "%Y-%m-%d 00:00:00", var_tz = "", var_interval = "days"){
#' Date-Time Sequence Generator
#'
#' \code{make.date_time} serves as a wrapper for \code{\link[stringi]{stri_datetime_add}}
#'
#' @param add_vec (numeric): Vector of integer values to add
#' @param var_start (datetime): Vector of integer values used to generate datetime results
#' @param var_form (string): Format for the datetime values
#' @param var_tz (string): Timezone for the datetime values
#' @param var_interval (string): The unit of time for \code{add_vec}
#'
#' @return A temporal vector
#'
#' @family Data Generation
#'
#' @export

	stringi::stri_datetime_add(time = as.Date(var_start), value = add_vec, unit = var_interval, tz = var_tz) |> format(var_form)
}
