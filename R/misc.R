#' Generate a Multi-Indicator Sigma Chart in Plotly for a Chosen Group
#'
#' This function generates a Multi-Indicator Sigma Chart in Plotly for a chosen
#' group. The function requires the funnel data and indicator metadata to be
#' provided as data.frames/data.tables. The function will then prepare the data
#' for the MISC chart and plot the chart in Plotly.
#'
#' @param funnel_data A data.frame/data.table of the funnel data with the
#' columns indicator, group, numerator and denominator. See ?example_funnel_data
#' for an example and more information on the fields required.
#' @param indicator_data A data.frame/data.table of the indicator metadata with
#' the columns indicator_theme, indicator, multiplier, data_type and betteris.
#' See ?example_indicator_data for an example and more information on the fields
#' required.
#' @param chosen_group A string of the group to be displayed on the chart.
#' @param hover_colour The background colour for the hoverinfo (default =
#' "#00667B")
#' @param control_colour The colour of the 95% and 99% control limits for the plot
#' (default = "#00667B")
#' @param worse_colour The colour of the bar in the unfavourable direction
#' (default = "#E46C0A")
#' @param neutral_colour The colour of the bar in the favourable direction
#' (default = "#A6A6A6")
#' @param better_colour The colour of the bar in the favourable direction
#' (default = "#00B0F0")
#' @param y_dp The number of decimal places displayed on the chart (default = 2)
#' @return A data.table with the required fields for the MISC plotly chart.
#'
#' @import data.table
#'
#' @export
#' @examples -
#' \dontrun{
#'   library(qiverse.qimisc)
#'   misc_data <- misc(
#'     funnel_data = example_funnel_data,
#'     indicator_data = example_indicator_data,
#'     chosen_group = "A"
#'   )
#' }
#'

misc <- function(
    funnel_data,
    indicator_data,
    chosen_group,
    hover_colour = "#00667B",
    control_colour = "#00667B",
    worse_colour = "#E46C0A",
    neutral_colour = "#A6A6A6",
    better_colour = "#00B0F0",
    y_dp = 2
) {

  # Prepare MISC data
  misc_data <- misc_prep_data(example_funnel_data, example_indicator_data)

  # Output MISC plotly
  misc_plotly(
    misc_data[group == chosen_group],
    hover_colour = hover_colour,
    control_colour = control_colour,
    worse_colour = worse_colour,
    neutral_colour = neutral_colour,
    better_colour = better_colour
  )
}

