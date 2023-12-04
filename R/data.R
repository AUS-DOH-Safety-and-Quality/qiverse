#' Indicator Data
#'
#' Example set of indicators used as an input to the data cleaning step for the
#' Multiple Indicator Sigma Chart.
#'
#' @format ## `indicator_data`
#' A data frame with 6 rows and 5 columns:
#' \describe{
#'   \item{indicator_theme}{The theme (group) that the indicator belongs to}
#'   \item{indicator}{The name of the indicator itself}
#'   \item{multiplier}{The multiplier applied to the indicator}
#'   \item{data_type}{The type of funnel plot used in the FunnelPlotR package}
#'   \item{betteris}{The direction of favourability for the indicator. I.e. set
#'   to "Lower" when the indicator is improving as it decreases, and set to
#'   "Higher" when the indicator is improving as it increases.}
#' }
"indicator_data"

#' Funnel Data
#'
#' Example funnel plot data used as an input to the data cleaning step for the
#' Multiple Indicator Sigma Chart.
#'
#' @format ## `funnel_data`
#' A data frame with 66 rows and 4 columns:
#' \describe{
#'   \item{indicator}{The indicator name that matches the indicator column from
#'   the indicator_data}
#'   \item{group}{The group (hospital) for which this row contains data from}
#'   \item{numerator}{The numerator value for the indicator and hospital
#'   combination}
#'   \item{denominator}{The denominator value for the indicator and hospital
#'   combination}
#' }
"funnel_data"
