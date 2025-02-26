#' Append spc chart values to data frame
#'
#' @param numerator A numeric vector
#' @param denominator A numeric vector
#' @param period_end A vector of dates of type Date format: %d/%m/%Y:12/11/2021
#' @param spccharttype A string identifying the type of spc chart. Default "p"
#' @param multiplier A string identifying the multiplication factor. Default 1
#'
#' @return The input data with appended spc data
#'
#' @family Pattern detection functions
#' @export
#' @examples -
appendSpcVal <- function(numerator, denominator, period_end,
                         spccharttype = "p", multiplier = 1) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  x <- `:=` <- NULL

  # Create data table
  input_data <- data.table::data.table(numerator, denominator, period_end,
                           multiplier, spccharttype)

  # Create an SPC chart for this indicator and establishment combination
  ind_est_spc <- qicharts2::qic(x = period_end,
                                y = numerator,
                                n = denominator,
                                data = input_data,
                                chart = spccharttype,
                                multiply = multiplier)

  spc_data <- ind_est_spc$data |> data.table::as.data.table()
  # select only the columns we want to keep
  spc_data[, period_end := as.Date(x, format = "%d/%m/%Y")]
  data.table::setnames(spc_data,
                       c("y", "cl", "ucl.95",
                         "lcl.95", "ucl", "lcl"),
                       c("spc_y", "spc_cl", "spc_ul95",
                         "spc_ll95", "spc_ul99", "spc_ll99"))
}
