#' Prepare data for the Multiple Indicator Sigma Chart
#'
#' @param funnel_data A data.frame/data.table of the funnel data with the
#' columns indicator, group, numerator and denominator. See ?example_funnel_data
#' for an example and more information on the fields required.
#' @param indicator_data A data.frame/data.table of the indicator metadata with
#' the columns indicator_theme, indicator, multiplier, data_type and betteris.
#' See ?example_indicator_data for an example and more information on the fields
#' required.
#'
#' @return A data.table with the required fields for the MISC plotly chart.
#'
#' @export
#' @examples -
#' \dontrun{
#'   library(qiverse.qimisc)
#'   misc_data <- misc_prep_data(example_funnel_data, example_indicator_data)
#' }

# Format Data ####
misc_prep_data <- function(funnel_data, indicator_data) {
  # Extract funnel data for all unique indicators ####
  ## Merge the data into a data.table
  data <- merge(funnel_data, indicator_data, by = "indicator",
                all.x = TRUE) |>
    data.table::as.data.table()
  ## Loop through each unique indicator to generate funnel limits and Z-scores
  data.funnel <- lapply(indicator_data$indicator, function(ind) {
    # Create subset of data based on chosen indicator
    data.ind <- data[indicator == ind]

    # Do not generate funnel data for indicator with only one group
    if (data.ind[, .N] > 1) {
      # Generate funnel plot data
      funnel <- FunnelPlotR::funnel_plot(
        denominator=data.ind$denominator,
        numerator=data.ind$numerator,
        group = data.ind$group,
        limit=99,
        data_type = data.ind[1, data_type],
        sr_method = "CQC",
        multiplier = data.ind[1, multiplier],
        draw_unadjusted = TRUE,
        draw_adjusted = TRUE,
        label = NA,
        highlight  = NA
      )

      # Extract out values , Uzscore, control limits
      funnel_data <- funnel$aggregated_data[, c("group", "rr", "Uzscore",
                                                "LCL95", "UCL95", "LCL99",
                                                "UCL99")] |>
        as.data.table()
      funnel_data[, group := as.character(group)]
      setnames(funnel_data, "rr", "value")

      # Merge back in funnel data
      data.ind <- merge(data.ind, funnel_data, by = "group", all.x = TRUE, sort = FALSE)

      # Find centreline
      data.ind[, cl := sum(numerator)/sum(denominator) * multiplier]

      # Output
      data.ind
    }
  }) |>
    # Stack list of data.tables into single table
    rbindlist()

  # Apply a series of transformation steps to transform data
  # directly into plotly function formats ####
  ## Apply multiplier to value, to make consistency between value and limits
  data.funnel[, value := value*multiplier]

  ## Apply betteris to Uzscore for consistency in reporting
  data.funnel[betteris == "Lower", Uzscore_betteris := Uzscore * (-1)]
  data.funnel[betteris == "Higher", Uzscore_betteris := Uzscore * (1)]

  ## Set suffix for hovertext label
  ### Set percentage
  data.funnel[data_type == "PR" & multiplier == 1, suffix := "%"]
  ### Set rates per x
  data.funnel[data_type == "PR" & multiplier != 1,
              suffix := paste0(" per ", formatC(multiplier, digits = 0,
                                                format = "f", big.mark = ","))]
  ### Set blank for others
  data.funnel[is.na(suffix), suffix := ""]

  ## Multiply by 100 for those that are proportions and multiplier 1
  data.funnel[data_type == "PR" & multiplier == 1,
              ":=" (value = value * 100,
                    UCL99 = UCL99 * 100,
                    cl = cl * 100,
                    LCL99 = LCL99 * 100)]

  # Change names of columns to uppercase
  setnames(data.funnel, names(data.funnel), toupper(names(data.funnel)))

  # Return output ####
  return(data.funnel)
}

