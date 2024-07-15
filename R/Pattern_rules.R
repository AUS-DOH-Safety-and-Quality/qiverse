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
pattern_rules <- function(
    numerator,
    denominator,
    period_end,
    unique_key,
    spccharttype = "p",
    multiplier = 1,
    betteris,
    fpl_astro,
    trend_size = 5,
    shift_size = 7
) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  . <- index_order <- spc_y <- spc_cl <- spc_stdev <- spc_ll99 <- spc_ll95 <-
    spc_ul95 <- spc_ul99 <- spc_diff <- spc_mr <- spc_amr <- spc_ulmr <-
    spc_amr2 <- spc_astro <- spc_y_diff <- spc_trend_cumsum <- spc_trend <-
    spc_trend_flag <- spc_twointhree_working <- spc_twointhree_cumsum <-
    spc_twointhree_flag <- spc_shift_working <- spc_shift_cumsum <-
    spc_shift_flag <- spc_astro <- spc_trend <- spc_twointhree <-
    spc_shift <- `:=` <- .N <- NULL

  #create data table
  input_dt <- data.table::data.table(unique_key, numerator, denominator,
                                     period_end, spccharttype, multiplier,
                                     betteris, fpl_astro)

  # Copy input data table to preserve original copy for run charts
  init_input_dt <- data.table::copy(input_dt)

  # Set initial order
  input_dt[, index_order := 1:input_dt[, .N]]

  # Set SPC Limits
  input_dt_p <- qiverse.qipatterns:::.spc_limits_p(input_dt[spccharttype == "p"]) #nolint
  input_dt_i <- qiverse.qipatterns:::.spc_limits_i(input_dt[spccharttype == "i"]) #nolint
  input_dt_g <- qiverse.qipatterns:::.spc_limits_g(input_dt[spccharttype == "g"]) #nolint
  input_dt_t <- qiverse.qipatterns:::.spc_limits_t(input_dt[spccharttype == "t"]) #nolint

  # Combine outputs
  input_dt <- rbind(
    input_dt_p,
    input_dt_i,
    input_dt_g,
    input_dt_t,
    fill = TRUE
  )

  # Reorder back to original order
  data.table::setorder(input_dt, index_order)
  input_dt[, index_order := NULL]

  # Check if datatable is not empty before running patterns
  if (input_dt[,.N] != 0) {

    # PAT010 Identifies the date of the most recent astronomical point for SPC
    # charts beyond a 3 sigma control limit.

    input_dt[betteris == "Higher" & spc_y < spc_ll99, spc_astro := period_end]
    input_dt[betteris == "Lower"  & spc_y > spc_ul99, spc_astro := period_end]


    # PAT030 TREND: identifies the most recent date of five consecutively
    # increasing/decreasing unfavourable points of data.
    # The first point is inclusive (four increasing after one point).
    # Flag the date with "Yes", for any particular indicator and hospital
    # combination.

    #Trend
    ## Determine direction of change from consecutive observations

    #the difference in y values between a point and the point preceeding it
    input_dt[, spc_y_diff := ifelse(betteris == "Lower", 1,
                                    ifelse(betteris == "Higher", -1, NA)) *
               (spc_y - data.table::shift(spc_y, 1, type = "lag")),
             by = unique_key]
    #first value has no preceding point catch
    input_dt[is.na(spc_y_diff), spc_y_diff := 0]
    #if a is less than b, the trend is moving favourably for those two points
    input_dt[spc_y_diff < 0, spc_y_diff := -1]
    #if a is more than b, the trend is move unfavourably for those two points
    input_dt[spc_y_diff > 0, spc_y_diff := 1]

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
    input_dt[, spc_trend_cumsum := spc_trend_counter(spc_y_diff),
             by = unique_key]

    ## Check whether trend hits limit, then flag
    #cumulative sum needs to be 1 less than the trend size as we are counting
    #the gaps in between points
    input_dt[spc_trend_cumsum >= (trend_size - 1),
             spc_trend_flag := 1]

    # Flag all points in the trend
    spc_trend_all_points <- function(data) {
      sapply(seq_along(data), function(i) {
        # Start index is the current record
        start_index <- i
        # End index is the current record + trend_size - 1
        end_index <- i + trend_size - 1
        # Check if there are any trends within the next trend_size - 1 points
        sum(!is.na(data[start_index:end_index]))
      })
    }
    input_dt[, spc_trend_flag := spc_trend_all_points(spc_trend_flag),
             by = unique_key]

    # Assign dates to points in trend
    input_dt[spc_trend_flag != 0, spc_trend := period_end]

    ## Clean up
    input_dt[, spc_y_diff := NULL] |>
      _[, spc_trend_cumsum := NULL] |>
      _[, spc_trend_flag := NULL]

    # PAT040 TWO IN THREE (Proposed): most recent date of where two in three
    # consecutive points which are
    # unfavourably within the 95% and 99% control limits
    # (2 sigma deviation from the mean).

    ## Flag those that meet two sigma above/below
    input_dt[betteris == "Higher" & spc_y < spc_ll95,
             spc_twointhree_working := 1]
    input_dt[betteris == "Lower"  & spc_y > spc_ul95,
             spc_twointhree_working := 1]
    input_dt[is.na(spc_twointhree_working), spc_twointhree_working := 0]

    ## Calculate cumulative sum of three consecutive observations
    input_dt[, spc_twointhree_cumsum :=
               Reduce(`+`, data.table::shift(spc_twointhree_working, 0:2)),
             by = unique_key]
    ## For those with 2 in three at first two observations
    input_dt[is.na(spc_twointhree_cumsum), spc_twointhree_cumsum :=
               Reduce(`+`, data.table::shift(spc_twointhree_working, 0:1)),
             by = unique_key]

    ## Where there is at least 2 above 2 sigma and
    #that the point is above 2 sigma, then flag
    input_dt[spc_twointhree_cumsum >= 2 & spc_twointhree_working == 1,
             spc_twointhree := period_end]

    # Flag other items in two in three that are outside 2 sigma
    spc_twointhree_other <- function(data_twointhree_working, data_twointhree) {
      sapply(seq_along(data_twointhree_working), function(i) {
        # start index is the current record
        start_index <- i
        # end index is the current record + 2
        end_index <- i + 2
        # check if there are any two in three patterns within the next 2 points
        flag <- sum(!is.na(data_twointhree[start_index:end_index]))>0
        # multiply with 2 sigma flag
        flag * data_twointhree_working[i]
      })
    }
    input_dt[, spc_twointhree_flag := spc_twointhree_other(
      spc_twointhree_working,
      spc_twointhree),
      by = unique_key]

    # Assign dates to points in two in three
    input_dt[spc_twointhree_flag == 1, spc_twointhree := period_end]

    ## Clean up
    input_dt[, spc_twointhree_working := NULL] |>
      _[, spc_twointhree_cumsum := NULL] |>
      _[, spc_twointhree_flag := NULL]

    # PAT070 SHIFT (Current: Identifies the date of the final point in the most
    # recent unfavourable SHIFT (defined as 7 consecutive points which are
    # unfavourable above/below the mean). Flag the date with "Yes",
    # for any particular indicator and hospital combination.

    # Shift ####
    ## Determine which size the observation sits and flag it
    input_dt[betteris == "Higher" & spc_y < spc_cl, spc_shift_working := 1]
    input_dt[betteris == "Higher" & spc_y > spc_cl, spc_shift_working := -1]
    input_dt[betteris == "Lower"  & spc_y > spc_cl, spc_shift_working := 1]
    input_dt[betteris == "Lower"  & spc_y < spc_cl, spc_shift_working := -1]
    input_dt[spc_y == spc_cl, spc_shift_working := 0]
    input_dt[is.na(spc_shift_working), spc_shift_working := 0]

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
    input_dt[, spc_shift_cumsum := spc_shift_counter(spc_shift_working),
             by = unique_key]

    ## Check whether shift hits limit, then flag
    input_dt[spc_shift_cumsum >= shift_size, spc_shift_flag := 1]

    # Flag all points in the trend
    spc_shift_all_points <- function(data) {
      sapply(seq_along(data), function(i) {
        # Start index is the current record
        start_index <- i
        # End index is the current record + shift_size - 1
        end_index <- i + shift_size - 1
        # Check if there are any trends within the next shift_size - 1 points
        sum(!is.na(data[start_index:end_index]))
      })
    }
    input_dt[, spc_shift_flag := spc_shift_all_points(spc_shift_flag),
             by = unique_key]

    # Assign dates to points in trend
    input_dt[spc_shift_flag != 0, spc_shift := period_end]

    ## Clean up
    input_dt[, spc_shift_working := NULL] |>
      _[, spc_shift_cumsum := NULL] |>
      _[, spc_shift_flag := NULL]

    input_dt <- input_dt[, .(unique_key, period_end, numerator, denominator,
                             spccharttype,
                             multiplier, betteris, fpl_astro, spc_y, spc_cl,
                             spc_ul99, spc_ul95, spc_ll95, spc_ll99, spc_astro,
                             spc_trend, spc_twointhree, spc_shift)]
  }

  # Add back in spccharttype run
  input_dt <- rbind(
    input_dt,
    init_input_dt[spccharttype == "run",
                  .(unique_key, period_end, numerator, denominator,
                    spccharttype, multiplier, betteris, fpl_astro)],
    fill = TRUE
  )

  return(input_dt)
}
