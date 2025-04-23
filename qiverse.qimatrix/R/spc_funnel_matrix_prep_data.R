#' Format data for the SPC Funnel Matrix Visuals
#'
#' Extract all favourable and unfavourable SPC and Funnel Plot
#' patterns, and export into the SPC Funnel Matrix format.
#'
#' @param indicator A vector to denote the indicator group.
#' Used to calculate aggregate SPC and Funnel Plots
#' @param group A vector of group names.
#' Used to aggregate and group points on plots
#' @param period_end A date (or character of format "yyyy-mm-dd")
#' for the end date of the period.
#' @param period_start A date (or character of format "yyyy-mm-dd")
#' for the start date of the period.
#' @param numerator A vector of the numerator. Used to calculate
#' the value for SPC and Funnel plots
#' @param denominator A vector of the denominator Used to calculate
#' the value for SPC and Funnel plots
#' @param multiplier Scale relative risk and funnel by this factor. Default to
#' 1, but 100 sometime used, e.g. in some group mortality ratios.
#' @param better_is A string identifying the direction that is favourable for
#' the indicator. "Higher" for points below the lower control limit to be
#' unfavourable, "Lower" for points above the upper control limit to be
#' unfavourable, and "Neutral" if the direction is not stated.
#' Default is "Higher"
#' @param parent_group_name A vector of parent group names which are to be
#' displayed in the tooltip. These are the major categories for the groups.
#' Default is "All"
#' @param parent_indicator A vector used for indicator parent grouping, i.e.
#' grouping together maternity related indicators into a Maternity group.
#' Default is "All"
#' @param spc_chart_type A string identifying the type of spc chart. Default "p"
#' @param funnel_chart_type A string identifying the type of funnel plot.
#' Default "PR"
#' @param indicator_name A vector of descriptive names for indicator.
#' Default indicator variable
#' @param group_name A vector of descriptive names for groups
#' Default group variable
#' @param funnel_data_points A vector of "Yes" or NA, marks which data points
#' are included in the funnel plot calculation. Default "Yes"
#'
#' @return A data.table with the required fields for the SPC Funnel Matrix.
#'
#' @import data.table
#'
#' @export
#' @examples
#' \dontrun{
#'   library(qiverse.qimatrix)
#'   library(qiverse.data)
#'   spc_funnel_matrix_data <- spc_funnel_matrix_prep_data(
#'     indicator = example_spc_data$indicator,
#'     group = example_spc_data$group,
#'     period_end = example_spc_data$period_end,
#'     period_start = example_spc_data$period_start,
#'     numerator = example_spc_data$numerator,
#'     denominator = example_spc_data$denominator,
#'     multiplier = example_spc_data$multiplier,
#'     better_is = example_spc_data$better_is,
#'     parent_group_name = "All",
#'     parent_indicator = "All",
#'     spc_chart_type = example_spc_data$spc_chart_type,
#'     funnel_chart_type = example_spc_data$funnel_chart_type,
#'     funnel_data_points = "Yes"
#'   )
#' }
spc_funnel_matrix_prep_data <- function(
  indicator,
  group,
  period_end,
  period_start,
  numerator,
  denominator,
  multiplier = 1,
  better_is = "Higher",
  parent_group_name = "All",
  parent_indicator = "All",
  spc_chart_type = "p",
  funnel_chart_type = "PR",
  indicator_name = indicator,
  group_name = group,
  funnel_data_points = "Yes"
) {

  # Dealing with undefined global functions or variables
  . <-  #nolint
    pattern_period_start <- pattern_period_end <-
    fpl_astro <- astro <- trend <- twointhree <- shift <-
    fpl_astro_unfav <- fpl_astro_fav <-
    fpl_astro_unfav_flag <- fpl_astro_fav_flag <-
    astro_unfav <- astro_fav <- astro_unfav_flag <- astro_fav_flag <-
    trend_unfav <- trend_fav <- trend_unfav_flag <- trend_fav_flag <-
    twointhree_unfav <- twointhree_fav <-
    twointhree_unfav_flag <- twointhree_fav_flag <-
    shift_unfav <- shift_fav <- shift_unfav_flag <- shift_fav_flag <-
    spc_unfav <- spc_fav <- spc_unfav_flag <- spc_fav_flag <-
    spc_flag <- spc_pattern <- fpl_flag <- unique_id <- parent_group <-
    NULL

  # This script extracts all favourable and unfavourable SPC and Funnel Plot
  # patterns, and exports this into the SPC Funnel Matrix format.

  #create data table
  input_data <- data.table::data.table(
    indicator, group, period_end,
    period_start, numerator,
    denominator, multiplier, parent_group_name, spc_chart_type,
    funnel_chart_type, parent_indicator, better_is,
    indicator_name, group_name,
    funnel_data_points
  )

  # Invert better_is to detect favourable patterns
  worseis <- fifelse(better_is == "Higher", "Lower", "Higher")

  # Apply pattern detection script for unfavourable and favourable
  unfav <- qiverse.qipatterns::pattern_detection(
    indicator, group,
    period_end, numerator, denominator, multiplier,
    better_is, spc_chart_type, funnel_chart_type,
    parent_indicator, indicator_name,
    group_name, funnel_data_points)
  fav <- qiverse.qipatterns::pattern_detection(
    indicator, group,
    period_end, numerator, denominator, multiplier,
    worseis, spc_chart_type, funnel_chart_type,
    parent_indicator, indicator_name,
    group_name, funnel_data_points)

  # Rename columns
  setnames(unfav, old = c("fpl_astro", "astro", "shift",
                          "trend", "twointhree", "parent_indicator",
                          "indicator_name",  "group_name"),
           new =c("fpl_astro_unfav", "astro_unfav", "shift_unfav",
                  "trend_unfav", "twointhree_unfav", "parent_indicator",
                  "indicator", "group"))

  setnames(fav, old = c("fpl_astro", "astro", "shift",
                        "trend", "twointhree", "parent_indicator",
                        "indicator_name",  "group_name"),
           new = c("fpl_astro_fav", "astro_fav", "shift_fav",
                   "trend_fav", "twointhree_fav", "parent_indicator",
                   "indicator", "group"))

  unfav[, c("numerator", "denominator") := NULL]
  fav[, c("numerator", "denominator") := NULL]

  # Export into a single patterns file ####
  patterns <- rbind(
    unfav[, .(indicator, group, parent_indicator)],
    fav[, .(indicator, group, parent_indicator)]
  ) |>
    unique() |>
    merge(unfav, by = c("indicator", "group", "parent_indicator"),
                      all.x = TRUE) |>
    merge(fav, by = c("indicator", "group", "parent_indicator"),
                      all.x = TRUE)

  ## get last 12 months for each indicator
  pattern_end_date <- input_data[
    funnel_data_points == "Yes",
    .(pattern_period_start = min(as.Date(period_start, format = "%d/%m/%Y")),
      pattern_period_end = max(as.Date(period_end, format = "%d/%m/%Y"))),
    keyby = .(indicator_name)]

  # Merge funnel dates into data
  patterns <- merge(patterns, pattern_end_date,
                                by.x = "indicator", by.y = "indicator_name",
                                all.x = TRUE)

  ## Add flags for astros
  patterns[astro_unfav >= pattern_period_start &
             astro_unfav <= pattern_period_end &
             (astro_unfav > astro_fav | is.na(astro_fav)),
           astro_unfav_flag := "Y"]
  patterns[is.na(astro_unfav_flag), astro_unfav_flag := "N"]

  patterns[astro_fav >= pattern_period_start &
             astro_fav <= pattern_period_end &
             (astro_fav > astro_unfav | is.na(astro_unfav)),
           astro_fav_flag := "Y"]
  patterns[is.na(astro_fav_flag), astro_fav_flag := "N"]

  ## Add flags for trend
  patterns[trend_unfav >= pattern_period_start &
             trend_unfav <= pattern_period_end &
             (trend_unfav > trend_fav | is.na(trend_fav)),
           trend_unfav_flag := "Y"]
  patterns[is.na(trend_unfav_flag), trend_unfav_flag := "N"]
  patterns[trend_fav >= pattern_period_start &
             trend_fav <= pattern_period_end &
             (trend_fav > trend_unfav | is.na(trend_unfav)),
           trend_fav_flag := "Y"]
  patterns[is.na(trend_fav_flag), trend_fav_flag := "N"]

  ## Add flags for twointhree
  patterns[twointhree_unfav >= pattern_period_start &
             twointhree_unfav <= pattern_period_end &
             (twointhree_unfav > twointhree_fav | is.na(twointhree_fav)),
           twointhree_unfav_flag := "Y"]
  patterns[is.na(twointhree_unfav_flag), twointhree_unfav_flag := "N"]
  patterns[twointhree_fav >= pattern_period_start &
             twointhree_fav <= pattern_period_end &
             (twointhree_fav > twointhree_unfav | is.na(twointhree_unfav)),
           twointhree_fav_flag := "Y"]
  patterns[is.na(twointhree_fav_flag), twointhree_fav_flag := "N"]

  ## Add flags for shift
  patterns[shift_unfav >= pattern_period_start &
             shift_unfav <= pattern_period_end &
             (shift_unfav > shift_fav | is.na(shift_fav)),
           shift_unfav_flag := "Y"]
  patterns[is.na(shift_unfav_flag), shift_unfav_flag := "N"]
  patterns[shift_fav >= pattern_period_start &
             shift_fav <= pattern_period_end &
             (shift_fav > shift_unfav | is.na(shift_unfav)),
           shift_fav_flag := "Y"]
  patterns[is.na(shift_fav_flag), shift_fav_flag := "N"]

  ## Add flags for funnel
  patterns[fpl_astro_unfav == pattern_period_end, fpl_astro_unfav_flag := "Y"]
  patterns[is.na(fpl_astro_unfav_flag), fpl_astro_unfav_flag := "N"]
  patterns[fpl_astro_fav == pattern_period_end, fpl_astro_fav_flag := "Y"]
  patterns[is.na(fpl_astro_fav_flag), fpl_astro_fav_flag := "N"]

  # Combine flags together
  patterns[, spc_unfav := pmax(astro_unfav, shift_unfav, trend_unfav,
                               twointhree_unfav, na.rm = TRUE)]
  patterns[, spc_fav := pmax(astro_fav, shift_fav, trend_fav, twointhree_fav,
                             na.rm = TRUE)]
  patterns[is.na(spc_unfav) & !is.na(spc_fav), spc_fav_flag := "Y"]
  patterns[!is.na(spc_unfav) & is.na(spc_fav), spc_unfav_flag := "Y"]
  patterns[!is.na(spc_unfav) & !is.na(spc_fav) & spc_unfav >= spc_fav,
           spc_unfav_flag := "Y"]
  patterns[!is.na(spc_unfav) & !is.na(spc_fav) & spc_fav > spc_unfav,
           spc_fav_flag := "Y"]

  patterns[spc_unfav_flag == "Y", spc_flag := "Unfavourable SPC Patterns"]
  patterns[spc_fav_flag == "Y", spc_flag := "Favourable SPC Patterns"]
  patterns[is.na(spc_flag), spc_flag := "Neutral"]

  patterns[fpl_astro_unfav_flag == "Y",
           fpl_flag := "Unfavourable Funnel Plot Outlier"]
  patterns[fpl_astro_fav_flag == "Y",
           fpl_flag := "Favourable Funnel Plot Outlier"]
  patterns[is.na(fpl_flag), fpl_flag := "Neutral"]

  spc_patterns_long <- rbind(
    # Convert favourable SPC patterns to long
    patterns[spc_flag != "Neutral" & spc_fav_flag == "Y",
             .(indicator, group,
               astro = ifelse(astro_fav_flag == "Y", "8786", NA_character_),
               trend = ifelse(trend_fav_flag == "Y", "9443", NA_character_),
               twointhree = ifelse(twointhree_fav_flag == "Y", "8532", NA_character_),
               shift = ifelse(shift_fav_flag == "Y", "9442", NA_character_)
             )] |>
      melt(
        id.vars = c("indicator", "group"),
        measure.vars = c("astro", "trend", "twointhree", "shift"),
        value.name = "spc_pattern"
      ) |>
      _[!is.na(spc_pattern), .(indicator, group, spc_pattern)],
    # Convert unfavourable SPC patterns to long
    patterns[spc_flag != "Neutral" & spc_unfav_flag == "Y",
             .(indicator, group,
               astro = ifelse(astro_unfav_flag == "Y", "8786", NA_character_),
               trend = ifelse(trend_unfav_flag == "Y", "9443", NA_character_),
               twointhree = ifelse(twointhree_unfav_flag == "Y", "8532", NA_character_),
               shift = ifelse(shift_unfav_flag == "Y", "9442", NA_character_)
             )] |>
      melt(
        id.vars = c("indicator", "group"),
        measure.vars = c("astro", "trend", "twointhree", "shift"),
        value.name = "spc_pattern"
      ) |>
      _[!is.na(spc_pattern), .(indicator, group, spc_pattern)]
  )
  spc_patterns_long[, unique_id := paste0(indicator, "_", group)]

  ## output ####
  # Merge with full data
  output_patterns <- input_data |>
    merge(
      input_data[
        funnel_data_points == "Yes",
        .(pattern_period_start = min(as.Date(period_start, format = "%d/%m/%Y")), #nolint
          pattern_period_end = max(as.Date(period_end, format = "%d/%m/%Y"))),
        keyby = .(indicator_name)],
      by = "indicator_name",
      all.x = TRUE,
      sort = FALSE) |>
    _[period_start >= pattern_period_start & period_end <= pattern_period_end] |> #nolint
    _[, .(unique_id = paste0(indicator_name, "_", group_name),
          parent_indicator = parent_indicator, indicator = indicator_name,
          group = group_name, parent_group = parent_group_name)] |>
    unique() |>
    ## Merge patterns back on it
    merge(
      patterns[, .(unique_id = paste0(indicator, "_", group),
                   spc_flag, fpl_flag)],
      by = "unique_id", all.x = TRUE, sort = FALSE
    ) |>
    ## Those without patterns are deemed neutral
    _[is.na(spc_flag), spc_flag := "Neutral"] |>
    _[is.na(fpl_flag), fpl_flag := "Neutral"]
  data.table::setorder(output_patterns, parent_indicator, indicator,
                       parent_group, group)

  # Return Output
  return(
    list(
      spc_funnel_matrix = output_patterns |> as.data.table(),
      spc_patterns_long = spc_patterns_long
    )
  )
}
