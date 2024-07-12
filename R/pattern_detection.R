#' Pattern Detection
#'
#' @param indicator A vector of indicator codes
#' @param establishment A vector of establishment ids
#' @param period_end A vector of dates of type Date format: %d/%m/%Y:12/11/2021
#' @param numerator A numeric vector
#' @param denominator A numeric vector
#' @param multiplier A string identifying the multiplication factor. Default 1
#' @param betteris A character string, shows direction of positive change.
#' @param spccharttype A string identifying the type of spc chart. Default "p"
#' @param funnelcharttype A string identifying the type of funnel plot.
#' Default "PR"
#' @param indicatorgroup A vector used for indicator grouping i.e. QSG Theme
#' @param descriptionshort A vector of descriptive names for indicators
#' @param shorthospitalname A vector of descriptive names for establishments
#' @param funneldatapoints A vector of which data points are included in
#' the funnel plot calculation
#' @param trend_size The number of points in a trend pattern
#' @param shift_size The number of points in a shift pattern
#'
#' @import data.table
#'
#' @return A data table of unique indicator establishment rows with any
#' identified patterns and qsg recommendations
#' @family Pattern detection functions
#' @export
#'
#' @examples -
pattern_detection <- function(indicator, establishment, period_end, numerator,
                              denominator, multiplier, betteris,
                              spccharttype = "p", funnelcharttype = "PR",
                              indicatorgroup = NA, descriptionshort = indicator,
                              shorthospitalname = establishment,
                              funneldatapoints = "Yes", trend_size = 5,
                              shift_size = 7) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  . <- fpl_astro <- unique_key <- spc_astro <- spc_shift <- spc_trend <-
    spc_twointhree <- fpl_astro <- astro <- shift <- trend <- twointhree <-
      `:=` <- validSPC <- NULL

  #create data table
  input_data <- data.table::data.table(indicator, establishment, period_end,
                                       numerator,
                           denominator, multiplier, spccharttype,
                           funnelcharttype, indicatorgroup, betteris,
                           descriptionshort, shorthospitalname,
                           funneldatapoints)

  #Create aggregate data for spc"s by indicator
  aggregate <- input_data[, .(numerator = sum(numerator),
                              denominator = sum(denominator),
                              establishment = "Aggregate",
                              shorthospitalname = "Aggregate",
                              funneldatapoints = unique(funneldatapoints),
                              fpl_rr = NA_integer_, fpl_ll95 = NA_integer_,
                              fpl_ul95 = NA_integer_, fpl_ll99 = NA_integer_,
                              fpl_ul99 = NA_integer_,
                              fpl_row_value = NA_integer_,
                              fpl_astro = as.Date(NA)),
                              by = .(indicator, multiplier, period_end,
                                     betteris, spccharttype, funnelcharttype,
                                     indicatorgroup, descriptionshort)]

  data.table::setcolorder(aggregate,
                          c("indicator", "establishment", "period_end",
                            "numerator", "denominator", "multiplier",
                            "spccharttype", "funnelcharttype", "indicatorgroup",
                            "betteris", "descriptionshort", "shorthospitalname",
                            "funneldatapoints", "fpl_rr", "fpl_ll95",
                            "fpl_ul95", "fpl_ll99", "fpl_ul99", "fpl_row_value",
                            "fpl_astro"))

  #Filter to current funnel, for each indicator calculate the funnel plot values
  input_data_funnel <- input_data[funneldatapoints == "Yes",
                                  qiverse.qipatterns::appendFplVal(
                                    numerator, denominator,
                                    establishment, funnelcharttype[1],
                                    multiplier[1], betteris[1],
                                    max(period_end)),
                                  by = .(indicator)]
  #console feedback
  cat("Funnel Patterns Completed", "\n")

  #merge back the funnel data to main
  input_data <- merge(input_data, input_data_funnel, all = TRUE,
                           by = c("indicator", "establishment"))
  #append the aggregate values onto the data table
  input_data <- rbind(input_data, aggregate)

  #Validate that each indicator hospital has enough data points to make an SPC
  input_data_valid <- input_data[, qiverse.qipatterns::validSPC(numerator,
                                                            denominator,
                                                            period_end),
                                by = c("indicator", "establishment")]
  cat("SPC Data Validated", "\n")
  #merge back the validation status to main
  input_data <- merge(input_data, input_data_valid, all = TRUE,
                           by = c("indicator", "establishment", "period_end",
                                  "numerator", "denominator"))
  #filter out invalid data
  input_data <- input_data[validSPC == TRUE]

  cat("Start SPC Pattern Detection", "\n")
  #calculate spc values
  input_data_spc <- input_data[, qiverse.qipatterns::pattern_rules(
    numerator, denominator, period_end,
    unique_key = paste0(indicator, "_", establishment),
    spccharttype, multiplier,
    betteris, fpl_astro, trend_size, shift_size)]
  #merge pattern data with main data
  input_data <- merge(
    input_data[, .(unique_key = paste0(indicator, "_", establishment),
                   indicator, establishment, indicatorgroup,
                   shorthospitalname, descriptionshort, period_end,
                   funneldatapoints)],
    input_data_spc,
    by = c("unique_key", "period_end"),
    all.x = TRUE,
    sort = FALSE
  )
  #remove the join key
  input_data[, unique_key := NULL]

  #console feedback
  cat("SPC Patterns Completed", "\n")

  #summarise data by indicator establishment
  #input_data <- input_data[establishment == "Aggregate", funneldatapoints := "Yes"] #nolint
  #find the latest patterns for each combination
  input_data <- input_data[funneldatapoints == "Yes", by = .(descriptionshort,
                                      shorthospitalname,
                                      indicatorgroup,
                                      betteris),
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
