#' Check validity of data for SPC
#'
#' @param numerator A numeric vector
#' @param denominator A numeric vector
#' @param period_end A vector of dates of type Date format: %d/%m/%Y:15/12/2022
#'
#' @return The input data with added validSPC flag for filtering
#'
#' @export
#' @family Pattern detection functions
#' @examples -
#'
validSPC <- function(numerator, denominator, period_end) {
  # Dealing with undefined global functions or variables
  `:=` <- NULL
  #create data table
  input_data <- data.table::data.table(numerator, denominator, period_end,
                                       validSPC = 1)
  #check denominator aren't all 0's
  input_data[denominator == 0, validSPC := 0]
  #check if the number of valid data points is 1 if so set it to false
  input_data[sum(validSPC) == 1, validSPC := 0]
  return(input_data)
}
