#necessity of this function needs to be made clearer (doesn't appear to be used in the data pack generation or Pattern-Detection repo)

#Pattern detection stripped

#' @title Generate pattern detection output for
#' Statistical Process Control charts
#'
#' @description Generate the pattern detection output for the input data.
#' Can handle either input_df formatted to the squis format or individual
#' vectors for numerator, denominator and period_end, and strings for
#' spccharttype, multiplier, betteris, indicator and establishment.
#'
#' Wrapper for the [runPat()] function
#'
#' @param input_df A dataframe filtered to the squis format.
#' @param numerator A numeric vector
#' @param denominator A numeric vector
#' @param period_end A character vector
#' @param spccharttype A character string
#' @param multiplier A numeric value
#' @param betteris A character string, either "Lower" or "Higher"
#' @param indicator A character string
#' @param establishment A character string
#' @param trend_size A numeric value for the number of points for a trend
#' pattern (default = 5)
#' @param shift_size A numeric value for the number of points for a shift
#' pattern (defualt = 7)
#'
#' @return A dataframe consisting of the pattern detection output with
#' added indicator and establishment
#' @export
#'
#' @examples test <- runPatterns(numerator = c(1,4,3,6,3),
#'                     denominator = c(2,4,6,8,3),
#'                     period_end = c("2022/01/01", "2022/02/01",
#'                     "2022/03/01","2022/04/01", "2022/05/01"),
#'                     spccharttype = "p",
#'                     multiplier = 100,
#'                     betteris = "Lower",
#'                     indicator = "Test Indicator",
#'                     establishment = "Test Est")

runPatterns <- function(input_df = NULL, numerator = NULL, denominator = NULL,
                      period_end = NULL, spccharttype = NULL,
                      multiplier = NULL, betteris = NULL,
                      indicator = NULL, establishment = NULL,
                      trend_size = 5, shift_size = 7) {

  # Dealing with undefined global functions or variables
  .data <- NULL

  if (is.data.frame(input_df)) {
    numerator <- input_df$numerator
    denominator <- input_df$denominator
    period_end <- input_df$period_end
    spccharttype <- input_df$spccharttype[1]
    multiplier <- input_df$multiplier[1]
    betteris <- input_df$betteris[1]
    indicator <- input_df$descriptionshort[1]
  }

  if (length(numerator) <= 2 || sum(numerator) == 0) {
    exit <- data.frame(Astro = NA_character_,
                       Shift = NA_character_,
                       Trend = NA_character_,
                       TwoInThree = NA_character_) |>
      dplyr::mutate(Indicator = ifelse(is.null(indicator),
                                #descriptionshort is a variable used in the naming but isn't a
                                # parameter. That is likely to break for non-HQIU users
                                input_df$descriptionshort[1],
                                indicator),
            # can the entity name be customised? Package is most useful for health but not all
            #entities are hospitals
             Hospital = ifelse(is.null(establishment),
            #  shorthospitalname is a variable used in the naming but isn't a
            # parameter. That is likely to break for non-HQIU users
                               input_df$shorthospitalname[1],
                              establishment),
             .before = .data$Astro)
    return(exit)
  }

  stopifnot("Ensure numerator, denominator and period_end are of equal length" =
              length(numerator) == length(denominator) &&
              length(denominator) == length(period_end))

  runPat(numerator = numerator, denominator = denominator,
         period_end = period_end, spccharttype = spccharttype,
         multiplier = multiplier, betteris = betteris,
         trend_size = trend_size, shift_size = shift_size) |>
  dplyr::mutate(Indicator = ifelse(is.null(indicator),
                                   #  description is a variable used in the naming but isn't a
                                   # parameter. That is likely to break for non-HQIU users
                             input_df$descriptionshort[1],
                             indicator),
         Hospital = ifelse(is.null(establishment),
                            input_df$shorthospitalname[1],
                           establishment),
         .before = .data$Astro)
}

#' Generate pattern detection output for Funnel plots
#'
#' @description Generate the pattern detection output for the input data.
#' Can handle either input_df formatted to the squis format or individual
#' vectors for numerator, denominator, period_end and indicator, and strings for
#' funnelcharttype, multiplier, betteris and establishment.
#'
#' Wrapper for the [runFPat()] function
#'
#' @param input_df A dataframe filtered to the squis format.
#' @param numerator A numeric vector
#' @param denominator A numeric vector
#' @param funnelcharttype A character string
#' @param multiplier A numeric value
#' @param betteris A character string, either "Lower" or "Higher"
#' @param indicator A character string
#' @param establishment A character vector
#'
#' @import magrittr
#' @return A dataframe consisting of the pattern detection output with
#' added indicator and establishment
#' @export
#'
#' @examples -
runFPatterns <- function(input_df = NULL, numerator = NULL, denominator = NULL,
                        funnelcharttype = NULL,
                        multiplier = NULL, betteris = NULL,
                        indicator = NULL, establishment = NULL) {
  # Dealing with undefined global functions or variables
  .data <- NULL

  if (is.data.frame(input_df)) {
    numerator <- input_df$numerator
    denominator <- input_df$denominator
    multiplier <- input_df$multiplier[1]
    betteris <- input_df$betteris[1]
    funnelcharttype <- input_df$funnelcharttype[1]
    establishment <- input_df$shorthospitalname
    indicator <- input_df$descriptionshort[1]
  }

  runFPat(numerator = numerator, denominator = denominator,
         multiplier = multiplier, betteris = betteris,
         funnelcharttype = funnelcharttype, establishment = establishment) |>
    dplyr::mutate(Indicator = ifelse(is.null(indicator),
                              input_df$descriptionshort, indicator),
                              .before = .data$group) |>
    dplyr::rename(Hospital = .data$group,
           Funnel = .data$outlier)
}


#' Generate SPC chart pattern detection output over input data
#'
#' @param numerator A numeric vector
#' @param denominator A numeric vector
#' @param period_end A character vector
#' @param spccharttype A character string
#' @param multiplier A numeric value
#' @param betteris A character string, either "Lower" or "Higher"
#' @param trend_size A numeric value for the number of points for a trend
#' pattern (default = 5)
#' @param shift_size A numeric value for the number of points for a shift
#' pattern (default = 7)
#'
#' @return A dataframe containing the most recent pattern for each pattern type
#' @export
#'
#' @examples
#' runPat(
#'   numerator = c(1,4,3,6,3),
#'   denominator = c(2,4,6,8,3),
#'   period_end = c("2022/01/01", "2022/02/01", "2022/03/01",
#'   "2022/04/01", "2022/05/01"),
#'   spccharttype = "p",
#'   multiplier = 100,
#'   betteris = "Lower",
#'   trend_size = 5,
#'   shift_size = 7
#' )
runPat <- function(numerator, denominator, period_end,
                   spccharttype, multiplier, betteris,
                   trend_size = 5, shift_size = 7) {
  # Dealing with undefined global functions or variables
  .data <- NULL

  # Create an SPC chart for this indicator and establishment combination
  SPC <- qicharts2::qic(x = period_end,
                        y = numerator,
                        n = denominator,
                        chart = spccharttype,
                        multiply = multiplier)$data |>
    dplyr::select(c("x", "y", "cl", "ucl.95", "lcl.95", "ucl", "lcl")) |>
    dplyr::mutate(betteris = betteris)

  output_list <- list()

  # Default lcl.95 to 0 if empty (affects T-chart)
  SPC$lcl.95[SPC$lcl.95 < 0 | is.na(SPC$lcl.95)] <- 0

  #Astro

  output_list$Astro <- dplyr::if_else(SPC$y >= SPC$ucl &
                                        SPC$betteris == "Lower" |
                                        SPC$y <= SPC$lcl &
                                        SPC$betteris == "Higher",
                                        SPC$x, NA_character_)

  #Trend

  count <- 1
  # For each value in the dataset, determine if it is greater than the next
  # value, if it is, the trend is decreasing
  for (i in 2:length(SPC$y)){
    if (SPC$betteris[i] == "Lower" &&
        SPC$y[i] > SPC$y[i - 1] ||
        SPC$betteris[i] == "Higher" &&
        SPC$y[i] < SPC$y[i - 1]) {
      count <- count + 1
    } else {
      count <- 1
    }
    if (count >= trend_size) {
      output_list$Trend[i] <-  SPC$x[i]
    } else {
      output_list$Trend[i] <- NA_character_
    }
  }

  #Shift

  count <- 0
  # for each value in the dataset take the "betteris" column and determine the
  # opposite to find the unfavourable outcome range
  for (i in seq_along(SPC$y)) {
    #determines if a point is above/below the centre line
    if ((SPC$betteris[i] == "Lower" &&
         SPC$y[i] > SPC$cl[i]) ||
        (SPC$betteris[i] == "Higher" &&
         SPC$y[i] < SPC$cl[i])) {
      count <- count + 1
    } else {
      count <- 0
    }
    #Determines if shift_size points in a row are above/below centre line
    if (count >= shift_size) {
      output_list$Shift[i] <- SPC$x[i]
    } else {
      output_list$Shift[i] <- NA_character_
    }
  }

  #Two in three

  # for each value in the dataset, determine if it above the warning limit,
  # if it is, determine if either of the next two point are also above the
  # warning limit
  # iterates over 1:length - 2 as the tests involve setting values for
  # i to i+2, if iteration over i: length, the last values would write to
  # empty rows
  output_list$TwoInThree[seq_along(SPC$y)] <- NA_character_
  for (i in seq(length(SPC$y) - 2)) {
    # determine if value is above upper warning limit and below upper control
    # limit according to "Betteris"
    if (SPC$betteris[i] == "Lower") {
      if (SPC$y[i] >= SPC$ucl.95[i]) {
        if (SPC$y[i + 1] >= SPC$ucl.95[i + 1]) {
          output_list$TwoInThree[i + 1] <- SPC$x[i + 1]
          }
        if (SPC$y[i + 2] >= SPC$ucl.95[i + 2]) {
          output_list$TwoInThree[i + 2] <- SPC$x[i + 2]
        }
      } else if (SPC$y[i + 1] >= SPC$ucl.95[i + 1] &&
                 SPC$y[i + 2] >= SPC$ucl.95[i + 2]) {
        output_list$TwoInThree[i + 2] <- SPC$x[i + 2]
      }else {
        output_list$TwoInThree[i:i + 2] <- NA_character_
      }
    }
    if (SPC$betteris[i] == "Higher") {
      if (SPC$y[i] <= SPC$lcl.95[i]) {
        if (SPC$y[i + 1] <= SPC$lcl.95[i + 1]) {
          output_list$TwoInThree[i + 1] <- SPC$x[i + 1]
        }
        if (SPC$y[i + 2] <= SPC$lcl.95[i + 2]) {
          output_list$TwoInThree[i + 2] <- SPC$x[i + 2]
        }
      }else if (SPC$y[i + 1] <= SPC$lcl.95[i + 1] &&
                SPC$y[i + 2] <= SPC$lcl.95[i + 2]) {
        output_list$TwoInThree[i + 2] <- SPC$x[i + 2]
      } else {
        output_list$TwoInThree[i:i + 2] <- NA_character_
      }
    }
  }

  # Helper function to remove NA's
  clear_na <- function(input_df) {
    input_df[is.nan(input_df)] <- NA
    input_df[is.infinite(input_df)] <- NA
    return(input_df)
  }

  # output
  output_df <- suppressWarnings(dplyr::summarise(
    output_list |>
      as.data.frame(),
    Astro = max(.data$Astro, na.rm = TRUE) |> clear_na(),
    Shift = max(.data$Shift, na.rm = TRUE) |> clear_na(),
    Trend = max(.data$Trend, na.rm = TRUE) |> clear_na(),
    TwoInThree = max(.data$TwoInThree, na.rm = TRUE) |>
      clear_na()))
return(output_df)
}

#' Generate funnel plot pattern detection output over input data
#'
#' @param numerator A numeric vector
#' @param denominator A numeric vector
#' @param establishment A character vector
#' @param funnelcharttype A character string
#' @param multiplier A numeric value
#' @param betteris A character string, either "Lower" or "Higher"
#'
#' @return A dataframe containing the outliers for the funnel plot
#' @export
#'
#' @examples -
runFPat <- function(numerator, denominator, establishment,
                    funnelcharttype, multiplier, betteris) {
  # Dealing with undefined global functions or variables
  .data <- NULL

  FunnelPlotR_version <-
    asNamespace("FunnelPlotR")$`.__NAMESPACE__.`$spec[["version"]]
  # For FunnelPlotR versions below 0.5.0
  if (FunnelPlotR_version < "0.5.0") {
    ind_funnel <- FunnelPlotR::funnel_plot(
      denominator = denominator,
      numerator = numerator,
      group = establishment,
      limit = 99,
      data_type = funnelcharttype,
      sr_method = "CQC",
      multiplier = multiplier
    )
  }

  # For FunnelPlotR version 0.5.0 and above
  if (FunnelPlotR_version >= "0.5.0") {
    ind_funnel <- FunnelPlotR::funnel_plot(
      .data = data.frame(
        denominator = denominator,
        numerator = numerator,
        group = establishment
      ),
      denominator = denominator,
      numerator = numerator,
      group = establishment,
      limit = 99,
      data_type = funnelcharttype,
      sr_method = "CQC",
      multiplier = multiplier
    )
  }

  ind_funnel_data <- ind_funnel$plot$data |>
    dplyr::mutate(betteris = betteris, multiplier = multiplier,
           fpl_row_value = .data$rr * .data$multiplier,
           outlier = dplyr::if_else(betteris == "Lower" &
                             .data$fpl_row_value > .data$UCL99 |
                             betteris == "Higher" &
                             .data$fpl_row_value < .data$LCL99,
                             "Yes", NA_character_)) |>
    dplyr::select(.data$group, .data$outlier)
  return(ind_funnel_data)

}
