#' Pattern Detection
#'
#' @param indicator A vector of indicator codes
#' @param group A vector of group ids
#' @param period_end A vector of dates of type Date format: %d/%m/%Y:12/11/2021
#' @param numerator A numeric vector
#' @param denominator A numeric vector
#' @param multiplier A string identifying the multiplication factor. Default 1
#' @param better_is A character string, shows direction of positive change.
#' @param spc_chart_type A string identifying the type of spc chart. Default "p"
#' @param funnel_chart_type A string identifying the type of funnel plot.
#' Default "PR"
#' @param parent_indicator A vector used for indicator grouping.
#' @param indicator_name A vector of descriptive names for indicators
#' @param group_name A vector of descriptive names for groups
#' @param funnel_data_points A vector of which data points are included in
#' the funnel plot calculation
#' @param trend_size The number of points in a trend pattern
#' @param shift_size The number of points in a shift pattern
#'
#' @import data.table
#'
#' @return A data table of unique indicator group rows with any
#' identified patterns
#' @family Pattern detection functions
#' @export
pattern_detection <- function(
    indicator,
    group,
    period_end,
    numerator,
    denominator,
    multiplier,
    better_is,
    overdispersion = FALSE,
    spc_chart_type = "p",
    funnel_chart_type = "PR",
    parent_indicator = NA,
    indicator_name = indicator,
    group_name = group,
    funnel_data_points = "Yes",
    trend_size = 5,
    shift_size = 7
) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  . <- fpl_astro <- unique_key <- spc_astro <- spc_shift <- spc_trend <-
    spc_twointhree <- astro <- shift <- trend <- twointhree <-
    `:=` <- valid_spc <- NULL
  
  #create data table
  input_data <- data.table::data.table(indicator, group, period_end, numerator,
                                       denominator, multiplier, spc_chart_type,
                                       funnel_chart_type, parent_indicator, better_is,
                                       overdispersion, indicator_name, group_name,
                                       funnel_data_points)

  #Create aggregate data for spc"s by indicator
  aggregate <- input_data[, .(numerator = sum(numerator),
                              denominator = sum(denominator),
                              group = "Aggregate",
                              group_name = "Aggregate",
                              funnel_data_points = unique(funnel_data_points),
                              fpl_rr = NA_integer_, fpl_ll95 = NA_integer_,
                              fpl_ul95 = NA_integer_, fpl_ll99 = NA_integer_,
                              fpl_ul99 = NA_integer_,
                              fpl_row_value = NA_integer_,
                              fpl_astro = as.Date(NA)),
                          by = .(indicator, multiplier, period_end,
                                 better_is, overdispersion, spc_chart_type, funnel_chart_type,
                                 parent_indicator, indicator_name)]
  
  data.table::setcolorder(aggregate,
                          c("indicator", "group", "period_end",
                            "numerator", "denominator", "multiplier",
                            "spc_chart_type", "funnel_chart_type", "parent_indicator",
                            "better_is", "overdispersion", "indicator_name", "group_name",
                            "funnel_data_points", "fpl_rr", "fpl_ll95",
                            "fpl_ul95", "fpl_ll99", "fpl_ul99", "fpl_row_value",
                            "fpl_astro"))
  
  #Filter to current funnel, for each indicator calculate the funnel plot values
  input_data_funnel <- input_data[funnel_data_points == "Yes",
                                    qiverse.qipatterns::append_fpl_val(
                                    numerator, denominator,
                                    group, funnel_chart_type = funnel_chart_type[1],
                                    multiplier = multiplier[1], better_is = better_is[1],
                                    overdispersion = overdispersion[1],
                                    period_end = max(period_end)),
                                  by = .(indicator)]
  #console feedback
  cat("Funnel Patterns Completed", "\n")
  
  #merge back the funnel data to main
  input_data <- merge(input_data, input_data_funnel, all = TRUE,
                      by = c("indicator", "group"))
  #append the aggregate values onto the data table where there is more than one group
  if(length(unique(group)) > 1) {
    input_data <- rbind(input_data, aggregate)
  }
  
  #Validate that each indicator and group combination has enough data points to make an SPC
  input_data_valid <- input_data[, qiverse.qipatterns::valid_spc(numerator,
                                                                 denominator,
                                                                 period_end),
                                 by = c("indicator", "group")]
  cat("SPC Data Validated", "\n")
  #merge back the validation status to main
  input_data <- merge(input_data, input_data_valid, all = TRUE,
                      by = c("indicator", "group", "period_end",
                             "numerator", "denominator"))
  #filter out invalid data
  input_data <- input_data[valid_spc == TRUE]
  
  cat("Start SPC Pattern Detection", "\n")
  #calculate spc values
  input_data_spc <- input_data[, qiverse.qipatterns::pattern_rules(
    numerator, denominator, period_end,
    unique_key = paste0(indicator, "_", group),
    spc_chart_type, multiplier,
    better_is, trend_size, shift_size)]
  #merge pattern data with main data
  input_data <- merge(
    input_data[, .(unique_key = paste0(indicator, "_", group),
                   indicator, group, parent_indicator,
                   group_name, indicator_name, period_end,
                   funnel_data_points, fpl_astro)],
    input_data_spc,
    by = c("unique_key", "period_end"),
    all.x = TRUE,
    sort = FALSE
  )
  #remove the join key
  input_data[, unique_key := NULL]
  
  #console feedback
  cat("SPC Patterns Completed", "\n")
  
  #summarise data by indicator group
  #input_data <- input_data[group == "Aggregate", funnel_data_points := "Yes"] #nolint
  #find the latest patterns for each combination
  input_data <- input_data[funnel_data_points == "Yes", by = .(indicator_name,
                                                               group_name,
                                                               parent_indicator,
                                                               better_is),
                           .(numerator = sum(numerator),
                             denominator = as.numeric(format(sum(denominator),
                                                             nsmall = 2)),
                             astro = max(spc_astro, na.rm = TRUE),
                             shift = max(spc_shift, na.rm = TRUE),
                             trend = max(spc_trend, na.rm = TRUE),
                             twointhree = max(spc_twointhree, na.rm = TRUE),
                             fpl_astro = max(fpl_astro, na.rm = TRUE)
                           )] |> suppressWarnings()
  
  #max introduces infinites when no patterns are identified for a combination
  input_data[is.infinite(astro), astro := NA]
  input_data[is.infinite(shift), shift := NA]
  input_data[is.infinite(trend), trend := NA]
  input_data[is.infinite(twointhree), twointhree := NA]
  input_data[is.infinite(fpl_astro), fpl_astro := NA]
  
  return(input_data)
}
