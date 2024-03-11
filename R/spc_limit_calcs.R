################################################################
## Name: Internal SPC Limit Calculations
## Purpose: This script calculates the SPC limits based on the spccharttype,
##          multiplier and input data.
## Lead Developer: Peter Li (Lead QUality Analyst - HQIU)
################################################################

# SPC p-charts
.spc_limits_p <- function(input_data) {
  if (input_data[, .N] > 0) {
    # Set y
    input_data[, spc_y := numerator / denominator]
    # Set cl
    input_data[
      input_data[,
                 .(spc_cl = sum(numerator) / sum(denominator)),
                 by = unique_key],
      on = .(unique_key = unique_key),
      spc_cl := spc_cl
    ]
    # Calculate standard deviation
    input_data[,
               spc_stdev := sqrt(spc_cl * (1 - spc_cl) / denominator)]
    # Calculate control limits
    input_data[, spc_ll99 := spc_cl - 3 * spc_stdev]
    input_data[, spc_ll95 := spc_cl - 2 * spc_stdev]
    input_data[, spc_ul95 := spc_cl + 2 * spc_stdev]
    input_data[, spc_ul99 := spc_cl + 3 * spc_stdev]
    # truncate limits to 0 and 1
    input_data[spc_ul95 > 1 & is.finite(spc_ul95),
               spc_ul95 := 1]
    input_data[spc_ul99 > 1 & is.finite(spc_ul99),
               spc_ul99 := 1]
    input_data[spc_ll95 < 0 & is.finite(spc_ll95),
               spc_ll95 := 0]
    input_data[spc_ll99 < 0 & is.finite(spc_ll99),
               spc_ll99 := 0]
    # Clean up
    input_data[, spc_stdev := NULL]
  }

  # Return
  return(input_data)
}

# SPC i-charts
.spc_limits_i <- function(input_data) {
  if (input_data[, .N] > 0) {
    # Set y
    input_data[, spc_y := numerator / denominator]
    # Set centreline
    input_data[
      input_data[,
                 .(spc_cl = mean(spc_y)),
                 by = unique_key],
      on = .(unique_key = unique_key),
      spc_cl := spc_cl
    ]
    # Average moving range
    input_data[, spc_diff := spc_y - spc_cl]
    input_data[, spc_mr := abs(spc_diff - data.table::shift(spc_diff, 1)),
               by = .(unique_key)]
    input_data[
      input_data[,
                 .(spc_amr = mean(spc_mr, na.rm = TRUE)),
                 by = unique_key],
      on = .(unique_key = unique_key),
      spc_amr := spc_amr
    ]
    # Upper limit for moving ranges
    input_data[, spc_ulmr := 3.267 * spc_amr]

    # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
    input_data_amr2 <- input_data[spc_mr < spc_ulmr]
    input_data[
      input_data_amr2[,
                      .(spc_amr2 = mean(spc_mr, na.rm = TRUE)),
                      by = unique_key],
      on = .(unique_key = unique_key),
      spc_amr2 := spc_amr2
    ]

    # Calculate standard deviation, Montgomery, 6.33
    input_data[, spc_stdev := spc_amr2 / 1.128]

    # Calculate control limits
    input_data[, spc_ll99 := spc_cl - 3 * spc_stdev]
    input_data[, spc_ll95 := spc_cl - 2 * spc_stdev]
    input_data[, spc_ul95 := spc_cl + 2 * spc_stdev]
    input_data[, spc_ul99 := spc_cl + 3 * spc_stdev]

    # Clean up
    input_data[, spc_diff := NULL]
    input_data[, spc_mr := NULL]
    input_data[, spc_ulmr := NULL]
    input_data[, spc_amr := NULL]
    input_data[, spc_amr2 := NULL]
    input_data[, spc_stdev := NULL]
  }

  # Return
  return(input_data)
}

# SPC g-charts
.spc_limits_g <- function(input_data) {
  if (input_data[, .N] > 0) {
    # Set y
    input_data[, spc_y := numerator]
    # Set cl
    input_data[
      input_data[,
                 .(spc_cl = mean(numerator, na.rm = TRUE)),
                 by = unique_key],
      on = .(unique_key = unique_key),
      spc_cl := spc_cl
    ]
    # Calculate standard deviation, Montgomery, p. 319
    input_data[,
               spc_stdev := sqrt(spc_cl * (spc_cl + 1))]
    # Calculate control limits
    input_data[, spc_ll99 := spc_cl - 3 * spc_stdev]
    input_data[, spc_ll95 := spc_cl - 2 * spc_stdev]
    input_data[, spc_ul95 := spc_cl + 2 * spc_stdev]
    input_data[, spc_ul99 := spc_cl + 3 * spc_stdev]
    input_data[spc_ll99 < 0, spc_ll99 := 0]
    input_data[spc_ll95 < 0, spc_ll95 := 0]

    # Set centre line to theoretical median, Provost (2011) p. 228
    # input_data[, spc_cl := 0.693 * spc_cl] # nolint

    # Set centre line to actual median
    input_data[, spc_cl := stats::median(spc_y)]

    # Clean up
    input_data[, spc_stdev := NULL]

  }

  # Return
  return(input_data)
}

# SPC t-charts
.spc_limits_t <- function(input_data) {
  if (input_data[, .N] > 0) {
    # Set y
    input_data[, spc_y := numerator ^ (1 / 3.6)]
    # Set centreline
    input_data[
      input_data[,
                 .(spc_cl = mean(spc_y)),
                 by = unique_key],
      on = .(unique_key = unique_key),
      spc_cl := spc_cl
    ]
    # Average moving range
    input_data[, spc_diff := spc_y - spc_cl]
    input_data[, spc_mr := abs(spc_diff - data.table::shift(spc_diff, 1)),
               by = .(unique_key)]
    input_data[
      input_data[,
                 .(spc_amr = mean(spc_mr, na.rm = TRUE)),
                 by = unique_key],
      on = .(unique_key = unique_key),
      spc_amr := spc_amr
    ]
    # Upper limit for moving ranges
    input_data[, spc_ulmr := 3.267 * spc_amr]

    # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
    input_data_amr2 <- input_data[spc_mr < spc_ulmr]
    input_data[
      input_data_amr2[,
                      .(spc_amr2 = mean(spc_mr, na.rm = TRUE)),
                      by = unique_key],
      on = .(unique_key = unique_key),
      spc_amr2 := spc_amr2
    ]

    # Calculate standard deviation, Montgomery, 6.33
    input_data[, spc_stdev := spc_amr2 / 1.128]

    # Calculate control limits
    input_data[, spc_ll99 := spc_cl - 3 * spc_stdev]
    input_data[, spc_ll95 := spc_cl - 2 * spc_stdev]
    input_data[, spc_ul95 := spc_cl + 2 * spc_stdev]
    input_data[, spc_ul99 := spc_cl + 3 * spc_stdev]

    # Back transform centre line and control limits
    input_data[, spc_y := spc_y^3.6]
    input_data[, spc_cl := spc_cl^3.6]
    input_data[, spc_ll99 := spc_ll99^3.6]
    input_data[, spc_ll95 := spc_ll95^3.6]
    input_data[, spc_ul95 := spc_ul95^3.6]
    input_data[, spc_ul99 := spc_ul99^3.6]
    input_data[is.na(spc_ll99) | spc_ll99 < 0, spc_ll99 := 0]
    input_data[is.na(spc_ll95) | spc_ll95 < 0, spc_ll95 := 0]

    # Clean up
    input_data[, spc_diff := NULL]
    input_data[, spc_mr := NULL]
    input_data[, spc_ulmr := NULL]
    input_data[, spc_amr := NULL]
    input_data[, spc_amr2 := NULL]
    input_data[, spc_stdev := NULL]
  }

  # Return
  return(input_data)
}
