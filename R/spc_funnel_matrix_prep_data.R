#' Format data for the SPC Funnel Matrix Visuals
#'
#' Extract all favourable and unfavourable SPC and Funnel Plot
#' patterns, and export into the SPC Funnel Matrix format.
#'
#' @param indicator A vector to denote the indicator group.
#' Used to calculate aggregate SPC and Funnel Plots
#' @param establishment A vector of group names.
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
#' 1, but 100 sometime used, e.g. in some hospital mortality ratios.
#' @param betteris A string identifying the direction that is favourable for
#' the indicator. "Higher" for points below the lower control limit to be
#' unfavourable, "Lower" for points above the upper control limit to be
#' unfavourable, and "Neutral" if the direction is not stated.
#' Default is "Higher"
#' @param parent_group_name A vector of parent group names which are to be
#' displayed in the tooltip. These are the major categories for the groups, i.e.
#' HSP names. Default is "All"
#' @param indicatorgroup A vector used for indicator grouping i.e. QSG Theme.
#' Default is "All"
#' @param spccharttype A string identifying the type of spc chart. Default "p"
#' @param funnelcharttype A string identifying the type of funnel plot.
#' Default "PR"
#' @param descriptionshort A vector of descriptive names for indicator.
#' Default indicator variable
#' @param shorthospitalname A vector of descriptive names for establishments
#' Default establishment variable
#' @param funneldatapoints A vector of "Yes" or NA, marks which data points
#' are included in the funnel plot calculation. Default "Yes"
#'
#' @return A data.table with the required fields for the SPC Funnel Matrix.
#'
#' @import data.table
#'
#' @export
#' @examples -
#' \dontrun{
#'   library(qiverse.qimatrix)
#'   spc_funnel_matrix_data <- spc_funnel_matrix_prep_data(
#'     indicator,
#'     establishment,
#'     period_end,
#'     period_start,
#'     numerator,
#'     denominator,
#'     multiplier = 1,
#'     betteris = "Higher",
#'     parent_group_name = "All",
#'     indicatorgroup = "All",
#'     spccharttype = "p",
#'     funnelcharttype = "PR",
#'     descriptionshort = indicator,
#'     shorthospitalname = establishment,
#'     funneldatapoints = "Yes"
#'   )
#' }
spc_funnel_matrix_prep_data <- function(
  indicator,
  establishment,
  period_end,
  period_start,
  numerator,
  denominator,
  multiplier = 1,
  betteris = "Higher",
  parent_group_name = "All",
  indicatorgroup = "All",
  spccharttype = "p",
  funnelcharttype = "PR",
  descriptionshort = indicator,
  shorthospitalname = establishment,
  funneldatapoints = "Yes"
) {

  # Dealing with undefined global functions or variables
  . <- `QSG Recommendation` <- hospital <-  #nolint
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
    indicator, establishment, period_end,
    period_start, numerator,
    denominator, multiplier, parent_group_name, spccharttype,
    funnelcharttype, indicatorgroup, betteris,
    descriptionshort, shorthospitalname,
    funneldatapoints
  )

  # Invert betteris to detect favourable patterns
  worseis <- fifelse(betteris == "Higher", "Lower", "Higher")

  # Apply pattern detection script for unfavourable and favourable
  unfav <- qiverse.qipatterns::pattern_detection(
    indicator, establishment,
    period_end, numerator, denominator, multiplier,
    betteris, spccharttype, funnelcharttype,
    indicatorgroup, descriptionshort,
    shorthospitalname, funneldatapoints)
  fav <- qiverse.qipatterns::pattern_detection(
    indicator, establishment,
    period_end, numerator, denominator, multiplier,
    worseis, spccharttype, funnelcharttype,
    indicatorgroup, descriptionshort,
    shorthospitalname, funneldatapoints)

  ## Remove standing downs
  #unfav <- unfav[`QSG Recommendation` != "Standing down"]
  #fav <- fav[`QSG Recommendation` != "Standing down"]

  # Rename columns
  unfav <- unfav[, .(indicator, hospital, indicatorgroup,
                     fpl_astro_unfav = fpl_astro,
                     astro_unfav = astro, shift_unfav = shift,
                     trend_unfav = trend, twointhree_unfav = twointhree)]
  fav <- fav[, .(indicator, hospital, indicatorgroup,
                 fpl_astro_fav = fpl_astro,
                 astro_fav = astro, shift_fav = shift,
                 trend_fav = trend, twointhree_fav = twointhree)]

  # Export into a single patterns file ####
  patterns <- rbind(
    unfav[, .(indicator, hospital, indicatorgroup)],
    fav[, .(indicator, hospital, indicatorgroup)]
  ) |>
    unique() |>
    merge(unfav, by = c("indicator", "hospital", "indicatorgroup"),
                      all.x = TRUE) |>
    merge(fav, by = c("indicator", "hospital", "indicatorgroup"),
                      all.x = TRUE)

  ## get last 12 months for each indicator
  pattern_end_date <- input_data[
    funneldatapoints == "Yes",
    .(pattern_period_start = min(as.Date(period_start, format = "%d/%m/%Y")),
      pattern_period_end = max(as.Date(period_end, format = "%d/%m/%Y"))),
    keyby = .(descriptionshort)]

  # Merge funnel dates into data
  patterns <- merge(patterns, pattern_end_date,
                                by.x = "indicator", by.y = "descriptionshort",
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
             .(indicator, hospital,
               astro = ifelse(astro_fav_flag == "Y", "8786", NA_character_),
               trend = ifelse(trend_fav_flag == "Y", "9443", NA_character_),
               twointhree = ifelse(twointhree_fav_flag == "Y", "8532", NA_character_),
               shift = ifelse(shift_fav_flag == "Y", "9442", NA_character_)
             )] |>
      melt(
        id.vars = c("indicator", "hospital"),
        measure.vars = c("astro", "trend", "twointhree", "shift"),
        value.name = "spc_pattern"
      ) |>
      _[!is.na(spc_pattern), .(indicator, hospital, spc_pattern)],
    # Convert unfavourable SPC patterns to long
    patterns[spc_flag != "Neutral" & spc_unfav_flag == "Y",
             .(indicator, hospital,
               astro = ifelse(astro_unfav_flag == "Y", "8786", NA_character_),
               trend = ifelse(trend_unfav_flag == "Y", "9443", NA_character_),
               twointhree = ifelse(twointhree_unfav_flag == "Y", "8532", NA_character_),
               shift = ifelse(shift_unfav_flag == "Y", "9442", NA_character_)
             )] |>
      melt(
        id.vars = c("indicator", "hospital"),
        measure.vars = c("astro", "trend", "twointhree", "shift"),
        value.name = "spc_pattern"
      ) |>
      _[!is.na(spc_pattern), .(indicator, hospital, spc_pattern)]
  )
  spc_patterns_long[, unique_id := paste0(indicator, "_", hospital)]

  ## output ####
  # Merge with full data
  output_patterns <- input_data |>
    merge(
      input_data[
        funneldatapoints == "Yes",
        .(pattern_period_start = min(as.Date(period_start, format = "%d/%m/%Y")), #nolint
          pattern_period_end = max(as.Date(period_end, format = "%d/%m/%Y"))),
        keyby = .(descriptionshort)],
      by = "descriptionshort",
      all.x = TRUE,
      sort = FALSE) |>
    _[period_start >= pattern_period_start & period_end <= pattern_period_end] |> #nolint
    _[, .(unique_id = paste0(descriptionshort, "_", shorthospitalname),
          indicatorgroup = indicatorgroup, indicator = descriptionshort,
          hospital = shorthospitalname, parent_group = parent_group_name)] |>
    unique() |>
    ## Merge patterns back on it
    merge(
      patterns[, .(unique_id = paste0(indicator, "_", hospital),
                   spc_flag, fpl_flag)],
      by = "unique_id", all.x = TRUE, sort = FALSE
    ) |>
    ## Those without patterns are deemed neutral
    _[is.na(spc_flag), spc_flag := "Neutral"] |>
    _[is.na(fpl_flag), fpl_flag := "Neutral"]
  data.table::setorder(output_patterns, indicatorgroup, indicator,
                       parent_group, hospital)

  # Return Output
  return(
    list(
      spc_funnel_matrix = output_patterns |> as.data.table(),
      spc_patterns_long = spc_patterns_long
    )
  )
}
