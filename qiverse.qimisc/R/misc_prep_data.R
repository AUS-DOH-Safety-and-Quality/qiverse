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
#' @examples
#' \dontrun{
#'   library(qiverse.qimisc)
#'   library(qiverse.data)
#'   misc_data <- misc_prep_data(example_funnel_data, example_indicator_data)
#' }


# Overdispersion adjustment for UCL99, LCL99, and UZscore added on 2025-01-02
# Please add overdispersion (logical data type) as an overdispersion flag.
# If the value does not exist in the dataset, it will automatically process it as overdispersion = FALSE.


# Format Data ####
misc_prep_data <- function(funnel_data, indicator_data) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  indicator <- data_type <- multiplier <- group <- cl <-
    numerator <- denominator <- value <- multiplier <- betteris <-
    Uzscore_betteris <- Uzscore <- data_type <- suffix <- UCL99 <- LCL99 <-
    OD99LCL <- OD99UCL <- s <- ODUzscore <- #nolint
    `:=`  <- .N <- overdispersion <- NULL #nolint

  #  not sure if order of the functions matters, I suspect it doesn't, but the order of the functions is different betwen misc_plotly() and misc_prep_data()
  # Load Data
  input_funnel_data <- data.table::copy(funnel_data)
  input_indicator_data <- data.table::copy(indicator_data)

  # Extract funnel data for all unique indicators ####
  ## Merge the data into a data.table
  input_data <- merge(input_funnel_data, input_indicator_data, by = "indicator",
                      all.x = TRUE) |>
    data.table::as.data.table()

  # Error handler for cases when data does not have overdispersion
  # *NOTE* Please perform this check after merging the data.
  if (!"overdispersion" %in% colnames(input_data)) {
    input_data[, overdispersion := FALSE]
  }
  else {
    input_data[is.na(overdispersion), overdispersion := FALSE]
  }

  ## Loop through each unique indicator to generate funnel limits and Z-scores
  data_funnel <- lapply(indicator_data$indicator, function(ind) {
    # Create subset of data based on chosen indicator
    data_ind <- input_data[indicator == ind]

    # Do not generate funnel data for indicator with only one group
    if (data_ind[, .N] > 1) {

      # Create Funnel plot
      FunnelPlotR_version <-
        asNamespace("FunnelPlotR")$`.__NAMESPACE__.`$spec[["version"]]

      # For FunnelPlotR versions below 0.5.0
      if (FunnelPlotR_version < "0.5.0") {
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
      }

      # For FunnelPlotR version 0.5.0 and above
      if (FunnelPlotR_version >= "0.5.0") {
        funnel <- FunnelPlotR::funnel_plot(
          .data = data_ind,
          denominator = denominator,
          numerator = numerator,
          group = group,
          limit = 99,
          data_type = data_ind[1, data_type],
          sr_method = "CQC",
          multiplier = data_ind[1, multiplier],
          draw_unadjusted = TRUE,
          draw_adjusted = TRUE,
          label = NA,
          highlight  = NA
        )
      }

      # Extract out values , Uzscore, control limits
      funnel_data <- funnel$aggregated_data[, c("group", "rr", "Uzscore",
                                                "LCL99", "UCL99",
                                                "OD99LCL", "OD99UCL",  "s")] |>
        data.table::as.data.table()
      funnel_data[, group := as.character(group)]

      # Add tau2 as a new column
      funnel_data$tau2 <- funnel$tau2

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
                    LCL99 = LCL99 * 100,
                    OD99LCL = OD99LCL * 100,
                    OD99UCL = OD99UCL * 100)]

  # Function to map an indirectly-standardised ratio (SR) value
  #   (i.e., observed/predicted) to a z-score which reflects the value's position
  #   on a funnel plot.
  #
  # FunnelPlotR uses exact Poisson limits for SR data with no OD adjustment,
  #   implemented using the Chi-Square distribution for continuous values:
  #   https://github.com/nhs-r-community/FunnelPlotR/blob/main/R/calculate_limits.R#L36
  #
  sr_to_zscore <- function(rr, denominator) {
    # Check whether value is above/below the centerline
    #   as the df for values in the upper tail (above centerline)
    #   should have +1 added
    offset <- ifelse(rr > 1, 1, 0)

    # Map the observed ratio to a probability (in [0-1]) via the Chi-Square CDF
    log_p <- pchisq(rr * 2 * denominator, 2 * (denominator + offset), log.p = TRUE)

    # Use the standard-normal quantile function to map the probability to
    #  a z-score
    qnorm(log_p, log.p = TRUE)
  }

  # Map the SR Ratios to z-scores
  data_funnel[data_type == "SR" & overdispersion == FALSE, Uzscore := sr_to_zscore(
    rr = rr,
    denominator = denominator
  )]
  data_funnel[, rr := NULL]

  # Apply overdispersion
  # calculating Z score for over dispersion
  # brought it from Power BI visual for funnel chart since it need to be align to it (by Andrew Johnson)
  # Rescale z-score to be equal to y - CL
  # scaled_z <- funnel_plot$aggregated_data$Uzscore * funnel_plot$aggregated_data$s
  # Combine standard error and overdispersion variance
  # combined_se <- sqrt(funnel_plot$aggregated_data$s^2 + funnel_plot$tau2)
  # Calculate ZScore additionally scaled for overdispersion
  # funnel_plot$aggregated_data$Uzscore <- scaled_z / combined_se
  data_funnel$ODUzscore <- (data_funnel$Uzscore * data_funnel$s) / sqrt(data_funnel$s^2 + data_funnel$tau2)

  #overwrite the values when over dispersion flag is true
  data_funnel[overdispersion == TRUE,
              `:=` (UCL99 = OD99UCL,
                    LCL99 = OD99LCL,
                    Uzscore = ODUzscore)]

  # Change names of columns to lowercase
  data.table::setnames(data_funnel, names(data_funnel),
                       tolower(names(data_funnel)))

  # Return output ####
  # Subset the data and Return output ####
  return(subset(data_funnel, select = c("group", "indicator", "numerator", "denominator", "multiplier", "data_type",
                                        "betteris", "indicator_theme", "uzscore", "lcl99", "ucl99", "cl")))

}
