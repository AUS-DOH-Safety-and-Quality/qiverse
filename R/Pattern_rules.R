#' Apply pattern rules
#'
#' @param numerator A numeric vector
#' @param denominator A numeric vector. Set as NA for g and t spccharttype.
#' @param period_end A vector of dates of type Date format: %d/%m/%Y:15/12/2022
#' @param unique_key A key to identify each indicator establishment combination
#' @param spccharttype A string identifying the type of spc chart. Default "p"
#' @param multiplier A string identifying the multiplication factor. Default 1
#' @param betteris A character string, shows direction of positive change.
#' "Higher" or "Lower"
#' @param fpl_astro Date of Astronomical Point pattern, period end max of funnel
#' @param trend_size The number of points in a trend pattern
#' @param shift_size The number of points in a shift pattern
#'
#' @return The input data with appended identified patterns
#'
#' @export
#' @family Pattern detection functions
#' @examples -
pattern_rules <- function(numerator, denominator, period_end,
                          unique_key, spccharttype = "p", multiplier = 1,
                          betteris, fpl_astro, trend_size = 5,
                          shift_size = 7) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  . <- Index_Order <- spc_y <- spc_cl <- spc_stdev <- spc_ll99 <- spc_ll95 <-
    spc_ul95 <- spc_ul99 <- spc_diff <- spc_mr <- spc_amr <- spc_ulmr <-
    spc_amr2 <- spc_astro <- spc_y_diff <- spc_trend_cumsum <- spc_trend <-
    spc_twointhree_working <- spc_twointhree_cumsum <- spc_shift_working <-
    spc_shift_cumsum <- spc_astro <- spc_trend <- spc_twointhree <-
    spc_shift <- `:=` <- .N <- NULL

  #create data table
  input_dt <- data.table::data.table(unique_key, numerator, denominator,
                                     period_end, spccharttype, multiplier,
                                     betteris, fpl_astro)

  # Set initial order
  input_dt[, Index_Order := 1:input_dt[, .N]]

  # For p chart
  input_dt_p <- input_dt[spccharttype == "p"]
  if (input_dt_p[, .N] > 0) {
    # Set y
    input_dt_p[, spc_y := numerator / denominator]
    # Set cl
    input_dt_p[
      input_dt_p[,
                 .(spc_cl = sum(numerator) / sum(denominator)),
                 by = unique_key],
      on = .(unique_key = unique_key),
      spc_cl := spc_cl
    ]
    # Calculate standard deviation
    input_dt_p[,
               spc_stdev := sqrt(spc_cl * (1 - spc_cl) / denominator)]
    # Calculate control limits
    input_dt_p[, spc_ll99 := spc_cl - 3 * spc_stdev]
    input_dt_p[, spc_ll95 := spc_cl - 2 * spc_stdev]
    input_dt_p[, spc_ul95 := spc_cl + 2 * spc_stdev]
    input_dt_p[, spc_ul99 := spc_cl + 3 * spc_stdev]
    # truncate limits to 0 and 1
    input_dt_p[spc_ul95 > 1 & is.finite(spc_ul95),
               spc_ul95 := 1]
    input_dt_p[spc_ul99 > 1 & is.finite(spc_ul99),
               spc_ul99 := 1]
    input_dt_p[spc_ll95 < 0 & is.finite(spc_ll95),
               spc_ll95 := 0]
    input_dt_p[spc_ll99 < 0 & is.finite(spc_ll99),
               spc_ll99 := 0]
    # Clean up
    input_dt_p[, spc_stdev := NULL]
  }

  # For i chart
  input_dt_i <- input_dt[spccharttype == "i"]
  if (input_dt_i[, .N] > 0) {
    # Set y
    input_dt_i[, spc_y := numerator / denominator]
    # Set centreline
    input_dt_i[
      input_dt_i[,
                 .(spc_cl = mean(spc_y)),
                 by = unique_key],
      on = .(unique_key = unique_key),
      spc_cl := spc_cl
    ]
    # Average moving range
    input_dt_i[, spc_diff := spc_y - spc_cl]
    input_dt_i[, spc_mr := abs(spc_diff - data.table::shift(spc_diff, 1)),
               by = .(unique_key)]
    input_dt_i[
      input_dt_i[,
                 .(spc_amr = mean(spc_mr, na.rm = TRUE)),
                 by = unique_key],
      on = .(unique_key = unique_key),
      spc_amr := spc_amr
    ]
    # Upper limit for moving ranges
    input_dt_i[, spc_ulmr := 3.267 * spc_amr]

    # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
    input_dt_i_amr2 <- input_dt_i[spc_mr < spc_ulmr]
    input_dt_i[
      input_dt_i_amr2[,
                      .(spc_amr2 = mean(spc_mr, na.rm = TRUE)),
                      by = unique_key],
      on = .(unique_key = unique_key),
      spc_amr2 := spc_amr2
    ]

    # Calculate standard deviation, Montgomery, 6.33
    input_dt_i[, spc_stdev := spc_amr2 / 1.128]

    # Calculate control limits
    input_dt_i[, spc_ll99 := spc_cl - 3 * spc_stdev]
    input_dt_i[, spc_ll95 := spc_cl - 2 * spc_stdev]
    input_dt_i[, spc_ul95 := spc_cl + 2 * spc_stdev]
    input_dt_i[, spc_ul99 := spc_cl + 3 * spc_stdev]

    # Clean up
    input_dt_i[, spc_diff := NULL]
    input_dt_i[, spc_mr := NULL]
    input_dt_i[, spc_ulmr := NULL]
    input_dt_i[, spc_amr := NULL]
    input_dt_i[, spc_amr2 := NULL]
    input_dt_i[, spc_stdev := NULL]

  }

  # For g chart
  input_dt_g <- input_dt[spccharttype == "g"]
  if (input_dt_g[, .N] > 0) {
    # Set y
    input_dt_g[, spc_y := numerator]
    # Set cl
    input_dt_g[
      input_dt_g[,
                 .(spc_cl = mean(numerator, na.rm = TRUE)),
                 by = unique_key],
      on = .(unique_key = unique_key),
      spc_cl := spc_cl
    ]
    # Calculate standard deviation, Montgomery, p. 319
    input_dt_g[,
               spc_stdev := sqrt(spc_cl * (spc_cl + 1))]
    # Calculate control limits
    input_dt_g[, spc_ll99 := spc_cl - 3 * spc_stdev]
    input_dt_g[, spc_ll95 := spc_cl - 2 * spc_stdev]
    input_dt_g[, spc_ul95 := spc_cl + 2 * spc_stdev]
    input_dt_g[, spc_ul99 := spc_cl + 3 * spc_stdev]
    input_dt_g[spc_ll99 < 0, spc_ll99 := 0]
    input_dt_g[spc_ll95 < 0, spc_ll95 := 0]

    # Set centre line to theoretical median, Provost (2011) p. 228
    # input_dt_g[, spc_cl := 0.693 * spc_cl] # nolint

    # Set centre line to actual median
    input_dt_g[, spc_cl := stats::median(spc_y)]

    # Clean up
    input_dt_g[, spc_stdev := NULL]
  }

  # For t chart
  input_dt_t <- input_dt[spccharttype == "t"]
  if (input_dt_t[, .N] > 0) {
    # Set y
    input_dt_t[, spc_y := numerator ^ (1 / 3.6)]
    # Set centreline
    input_dt_t[
      input_dt_t[,
                 .(spc_cl = mean(spc_y)),
                 by = unique_key],
      on = .(unique_key = unique_key),
      spc_cl := spc_cl
    ]
    # Average moving range
    input_dt_t[, spc_diff := spc_y - spc_cl]
    input_dt_t[, spc_mr := abs(spc_diff - data.table::shift(spc_diff, 1)),
               by = .(unique_key)]
    input_dt_t[
      input_dt_t[,
                 .(spc_amr = mean(spc_mr, na.rm = TRUE)),
                 by = unique_key],
      on = .(unique_key = unique_key),
      spc_amr := spc_amr
    ]
    # Upper limit for moving ranges
    input_dt_t[, spc_ulmr := 3.267 * spc_amr]

    # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
    input_dt_t_amr2 <- input_dt_t[spc_mr < spc_ulmr]
    input_dt_t[
      input_dt_t_amr2[,
                      .(spc_amr2 = mean(spc_mr, na.rm = TRUE)),
                      by = unique_key],
      on = .(unique_key = unique_key),
      spc_amr2 := spc_amr2
    ]

    # Calculate standard deviation, Montgomery, 6.33
    input_dt_t[, spc_stdev := spc_amr2 / 1.128]

    # Calculate control limits
    input_dt_t[, spc_ll99 := spc_cl - 3 * spc_stdev]
    input_dt_t[, spc_ll95 := spc_cl - 2 * spc_stdev]
    input_dt_t[, spc_ul95 := spc_cl + 2 * spc_stdev]
    input_dt_t[, spc_ul99 := spc_cl + 3 * spc_stdev]

    # Back transform centre line and control limits
    input_dt_t[, spc_y := spc_y^3.6]
    input_dt_t[, spc_cl := spc_cl^3.6]
    input_dt_t[, spc_ll99 := spc_ll99^3.6]
    input_dt_t[, spc_ll95 := spc_ll95^3.6]
    input_dt_t[, spc_ul95 := spc_ul95^3.6]
    input_dt_t[, spc_ul99 := spc_ul99^3.6]
    input_dt_t[is.na(spc_ll99) | spc_ll99 < 0, spc_ll99 := 0]
    input_dt_t[is.na(spc_ll95) | spc_ll95 < 0, spc_ll95 := 0]

    # Clean up
    input_dt_t[, spc_diff := NULL]
    input_dt_t[, spc_mr := NULL]
    input_dt_t[, spc_ulmr := NULL]
    input_dt_t[, spc_amr := NULL]
    input_dt_t[, spc_amr2 := NULL]
    input_dt_t[, spc_stdev := NULL]

  }

  ######### CAN ADD MORE TYPES OF CHARTS IN HERE ##########

  # Combine outputs
  input_df <- rbind(
    input_dt_p,
    input_dt_i,
    input_dt_g,
    input_dt_t,
    fill = TRUE
  )

  # Reorder back to original order
  data.table::setorder(input_dt, Index_Order)
  input_dt[, Index_Order := NULL]

  #find the max period end of data present
  max_period_end <- max(input_dt[, period_end])
  #find 1 year before max period end to find spc period start
  min_period_end <- as.Date(paste(lubridate::mday(max_period_end),
                                  lubridate::month(max_period_end),
                                  (lubridate::year(max_period_end) - 1),
                                  sep = "/"),
                            "%d/%m/%Y")

  # PAT010 Identifies the date of the most recent astronomical point for SPC
  # charts beyond a 3 sigma control limit.

  input_df[betteris == "Higher" & spc_y < spc_ll99, spc_astro := period_end]
  input_df[betteris == "Lower"  & spc_y > spc_ul99, spc_astro := period_end]


  # PAT030 TREND: identifies the most recent date of five consecutively
  # increasing/decreasing unfavourable points of data.
  # The first point is inclusive (four increasing after one point).
  # Flag the date with "Yes", for any particular indicator and hospital
  # combination.

  #Trend
  ## Determine direction of change from consecutive observations

  #the difference in y values between a point and the point preceeding it
  input_df[, spc_y_diff := ifelse(betteris == "Lower", 1,
                                  ifelse(betteris == "Higher", -1, NA)) *
             (spc_y - data.table::shift(spc_y, 1, type = "lag")),
           by = unique_key]
  #first value has no preceding point catch
  input_df[is.na(spc_y_diff), spc_y_diff := 0]
  #if a is less than b, the trend is moving favourably for those two points
  input_df[spc_y_diff < 0, spc_y_diff := -1]
  #if a is more than b, the trend is move unfavourably for those two points
  input_df[spc_y_diff > 0, spc_y_diff := 1]

  #Create custom function to count spc trends
  spc_trend_counter <- function(data) {
    sapply(seq_along(data), function(i) {
      # Only run if unfavourable
      if (data[i] >= 0) {
        # End index is current record
        end_index <- i
        # Start index is after the last -1, or the first record
        latest_favourable_trend <- c(1:i)[data[1:i] == -1]
        start_index <- ifelse(length(latest_favourable_trend) == 0, 1,
                              max(latest_favourable_trend) + 1)
        sum(data[start_index:end_index])
      } else {
        NA
      }
    })
  }

  ## Calculate cumulative sum of trend_size consecutive observations
  input_df[, spc_trend_cumsum := spc_trend_counter(spc_y_diff),
           by = unique_key]

  ## Check whether trend hits limit, then flag
  #cumulative sum needs to be 1 less than the trend size as we are counting
  #the gaps in between points
  input_df[spc_trend_cumsum == (trend_size - 1),
           spc_trend := period_end]

  ## Clean up
  input_df[, spc_y_diff := NULL][, spc_trend_cumsum := NULL]

  # PAT040 TWO IN THREE (Proposed): most recent date of where two in three
  # consecutive points which are
  # unfavourably within the 95% and 99% control limits
  # (2 sigma deviation from the mean).

  ## Flag those that meet two sigma above/below
  input_df[betteris == "Higher" & spc_y < spc_ll95,
           spc_twointhree_working := 1]
  input_df[betteris == "Lower"  & spc_y > spc_ul95,
           spc_twointhree_working := 1]
  input_df[is.na(spc_twointhree_working), spc_twointhree_working := 0]

  ## Calculate cumulative sum of three consecutive observations
  input_df[, spc_twointhree_cumsum :=
             Reduce(`+`, data.table::shift(spc_twointhree_working, 0:2)),
           by = unique_key]
  ## For those with 2 in three at first two observations
  input_df[is.na(spc_twointhree_cumsum), spc_twointhree_cumsum :=
             Reduce(`+`, data.table::shift(spc_twointhree_working, 0:1)),
           by = unique_key]

  ## Where there is at least 2 above 2 sigma and
  #that the point is above 2 sigma, then flag
  input_df[spc_twointhree_cumsum >= 2 & spc_twointhree_working == 1,
           spc_twointhree := period_end]

  ## Clean up
  input_df[, spc_twointhree_working := NULL][, spc_twointhree_cumsum := NULL]

  # PAT070 SHIFT (Current: Identifies the date of the final point in the most
  # recent unfavourable SHIFT (defined as 7 consecutive points which are
  # unfavourable above/below the mean). Flag the date with "Yes",
  # for any particular indicator and hospital combination.

  # Shift ####
  ## Determine which size the observation sits and flag it
  input_df[betteris == "Higher" & spc_y < spc_cl, spc_shift_working := 1]
  input_df[betteris == "Higher" & spc_y > spc_cl, spc_shift_working := -1]
  input_df[betteris == "Lower"  & spc_y > spc_cl, spc_shift_working := 1]
  input_df[betteris == "Lower"  & spc_y < spc_cl, spc_shift_working := -1]
  input_df[spc_y == spc_cl, spc_shift_working := 0]
  input_df[is.na(spc_shift_working), spc_shift_working := 0]

  #Create custom function to count spc shifts
  spc_shift_counter <- function(data) {
    sapply(seq_along(data), function(i) {
      # Only run if unfavourable
      if (data[i] >= 0) {
        # End index is current record
        end_index <- i
        # Start index is after the last -1, or the first record
        latest_favourable_shift <- c(1:i)[data[1:i] == -1]
        start_index <- ifelse(length(latest_favourable_shift) == 0, 1,
                              max(latest_favourable_shift) + 1)
        sum(data[start_index:end_index])
      } else {
        NA
      }
    })
  }

  ## Calculate cumulative sum of shift_size from consecutive observations
  input_df[, spc_shift_cumsum := spc_shift_counter(spc_shift_working),
           by = unique_key]

  ## Check whether shift hits limit, then flag
  input_df[spc_shift_cumsum >= shift_size, spc_shift := period_end]

  ## Clean up
  input_df[, spc_shift_working := NULL]
  input_df[, spc_shift_cumsum := NULL]

  #Remove patterns more than 12 months from last data point - some sites with
  #incomplete data may show more than 12 months from max displayed period
  input_df[spc_shift < min_period_end, spc_shift := NA]

  input_df <- input_df[, .(unique_key, period_end, numerator, denominator,
                           spccharttype,
                           multiplier, betteris, fpl_astro, spc_y, spc_cl,
                           spc_ul99, spc_ul95, spc_ll95, spc_ll99, spc_astro,
                           spc_trend, spc_twointhree, spc_shift)]

  # Add back in spccharttype run
  input_df <- rbind(
    input_df,
    input_dt[spccharttype == "run",
             .(unique_key, period_end, numerator, denominator,
               spccharttype, multiplier, betteris, fpl_astro)],
    fill = TRUE
  )

  return(input_df)
}
