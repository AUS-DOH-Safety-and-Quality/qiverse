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
#' @import data.table
#'
#' @export
#' @examples -
#' \dontrun{
#'   library(qiverse.qimisc)
#'   misc_data <- misc_prep_data(example_funnel_data, example_indicator_data)
#' }

# Format Data ####
misc_prep_data <- function(funnel_data, indicator_data) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  indicator <- data_type <- multiplier <- group <- cl <-
    numerator <- denominator <- value <- multiplier <- betteris <-
    Uzscore_betteris <- Uzscore <- data_type <- suffix <- UCL99 <- LCL99 <- #nolint
    `:=`  <- .N <- NULL #nolint

  # Load Data
  input_funnel_data <- data.table::copy(funnel_data)
  input_indicator_data <- data.table::copy(indicator_data)

  # Extract funnel data for all unique indicators ####
  ## Merge the data into a data.table
  input_data <- merge(input_funnel_data, input_indicator_data, by = "indicator",
                      all.x = TRUE) |>
    data.table::as.data.table()
  ## Loop through each unique indicator to generate funnel limits and Z-scores
  data_funnel <- lapply(indicator_data$indicator, function(ind) {
    # Create subset of data based on chosen indicator
    data_ind <- input_data[indicator == ind]

    # Do not generate funnel data for indicator with only one group
    if (data_ind[, .N] > 1) {
      # Generate funnel plot data
      funnel <- FunnelPlotR::funnel_plot(
        denominator = data_ind$denominator,
        numerator = data_ind$numerator,
        group = data_ind$group,
        limit = 99,
        data_type = data_ind[1, data_type],
        sr_method = "CQC",
        multiplier = data_ind[1, multiplier],
        draw_unadjusted = TRUE,
        draw_adjusted = TRUE,
        label = NA,
        highlight  = NA
      )

      # Extract out values , Uzscore, control limits
      funnel_data <- funnel$aggregated_data[, c("group", "Uzscore",
                                                "LCL99", "UCL99")] |>
        data.table::as.data.table()
      funnel_data[, group := as.character(group)]

      # Merge back in funnel data
      data_ind <- merge(data_ind, funnel_data, by = "group", all.x = TRUE,
                        sort = FALSE)

      # Find centreline
      data_ind[, cl := sum(numerator) / sum(denominator) * multiplier]

      # Output
      data_ind
    }
  }) |>
    # Stack list of data.tables into single table
    data.table::rbindlist()

  # Apply Multiplier Fix
  ## Multiply by 100 for those that are proportions and multiplier 1
  data_funnel[data_type == "PR" & multiplier == 1,
              ":=" (UCL99 = UCL99 * 100,
                    cl = cl * 100,
                    LCL99 = LCL99 * 100)]

  # Change names of columns to lowercase
  data.table::setnames(data_funnel, names(data_funnel),
                      tolower(names(data_funnel)))

  # Return output ####
  return(data_funnel)
}
