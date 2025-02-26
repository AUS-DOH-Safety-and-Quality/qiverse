#' Example SPC Data
#'
#' Example set of indicators used as an input SPC Funnel Matrix Prep Data Step
#'
#' @format ## `example_spc_data`
#' A data frame with 1584 rows and 10 columns:
#' \describe{
#'   \item{indicator}{The unique identifier for the indicator itself}
#'   \item{group}{The unique identifier for the group/establishment}
#'   \item{period_end}{The end date of each SPC period}
#'   \item{period_start}{The start date of each SPC period}
#'   \item{numerator}{The numerator value for the indicator, group and period
#'   combination}
#'   \item{denominator}{The denominator value for the indicator, group and
#'   period combination}
#'   \item{multiplier}{The multiplier applied to the indicator}
#'   \item{better_is}{The direction of favourability for the indicator. I.e. set
#'   to "Lower" when the indicator is improving as it decreases, and set to
#'   "Higher" when the indicator is improving as it increases.}
#'   \item{spc_chart_type}{The type of SPC used in the qiverse.data.qipatterns
#'   package}
#'   \item{funnel_chart_type}{The type of funnel plot used in the FunnelPlotR
#'   package}
#' }
"example_spc_data"

#' Example Indicator Data
#'
#' Example set of indicators used as an input to the data cleaning step for the
#' Multiple Indicator Sigma Chart.
#'
#' @format ## `example_indicator_data`
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
"example_indicator_data"

#' Example Funnel Data
#'
#' Example funnel plot data used as an input to the data cleaning step for the
#' Multiple Indicator Sigma Chart.
#'
#' @format ## `example_funnel_data`
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
"example_funnel_data"
