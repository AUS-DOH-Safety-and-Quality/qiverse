#' Draw a statistical process control chart in plotly
#'
#' @param x A vector of the x-axis for the SPC chart. Usually dates.
#' @param numerator A vector of the numerator (observed events/counts) values.
#' Used as the numerator of the Y-axis
#' @param denominator A vector of denominator (predicted/population etc.) Used
#' as denominator of the Y-axis and the scale of the x-axis. Only required when
#' when necessary.
#' @param data_type A string identifying the type of data used for in the plot,
#' the adjustment used and the reference point. One of: "i" for indirectly
#' standardised ratios, "p" for proportions, "r" for ratios
#' of counts, "run" for run charts, "g" for geometric charts and "t" for time
#' charts. Default is "p".
#' @param multiplier Scale relative risk and funnel by this factor. Default to
#' 1.
#' @param better_is A string identifying the direction that is favourable for
#' the indicator. "Higher" for points below the lower control limit to be
#' unfavourable, "Lower" for points above the upper control limit to be
#' unfavourable, and "Neutral" if the direction is not stated. Default is
#' "Higher".
#' @param title A string for the title of the plot.
#' @param spc_period_start A date (or character of format #' "yyyy-mm-dd")
#' for the start date of the SPC period.
#' @param spc_period_end A date (or character of format #' "yyyy-mm-dd")
#' for the end date of the SPC period.
#' @param x_axis_label A value denoting the x-axis label. Usually NA or a specified label.
#' @param y_axis_label A value denoting the y-axis label. Usually NA, a specified label or "Rate",
#' for items which are proportion based with multipliers.
#' @param brand_colour Hex code for the colour of the funnel limits
#' @param actual_colour Hex code for the colour actual data points
#' @param annotation_marker_colour Hex code for the colour actual data points
#' @param line_width A value denoting the width of the line in pixels
#' (default = 2)
#' @param marker_size A value denoting the size of the markers in pixels
#' (default = 10)
#' @param y_dp Number of decimal points for the y-axis ticks and hoverinfo
#' @param y_format Set to either "Numeric" or "Percentage" to format the y-axis
#' @param x_format The format for the x-axis. Default is "%b %Y".
#' @param patterns Either set to "Yes", "No" or as a dataframe with only a
#' single row with the columns "Astro", "Trend", "TwoInThree", "Shift". This is
#' the output of the runPat function in qiverse.qipatterns, or a filtered
#' pattern dataframe. The "Yes" option automatically applies this function to
#' the data. The "No" option does not display any patterns. (default = "Yes")
#' @param pattern_text_ay Set the y offset for pattern detection text in
#' pixels (default = 50)
#' @param pattern_arrow_width Set the width of the pattern arrow in pixels
#' (default = 2)
#' @param pattern_font_size Numeric value for the font size of the annotation.
#' Default is 25
#' @param trend_size Set the number of points required for a trend to be
#' detected (default = 5)
#' @param shift_size Set the number of points required for a shift to be
#' detected (default = 7)
#' @param target Set a target value for the SPC chart. If NA, no target will be
#' displayed. (default = NA)
#' @param target_annotation A string to annotate the target line with a text on the plot.
#' A value of NA indicates no annotation. (default = NA)
#' @param target_options A list of parameters to set the target line:
#' * legend_name: A string to set the legend name for the target line
#' * line_colour: A hex code to set the colour of the target line
#' * line_width: A numeric value to set the width of the target line
#' * line_dash: A string to set the dash of the target line. Options are
#' "solid", "dot", "dash", "longdash", "dashdot", "longdashdot"
#'
#' (default = list(legend_name = "Target", line_colour = "#FF0000",
#' line_width = 2, line_dash = "solid"))
#' @param nhs_colours_enable A boolean to enable NHS colours for the SPC chart.
#' (default = TRUE)
#' @param nhs_colours_options A list of parameters to enable NHS colours for the SPC
#' chart. (default = list(improvement_direction = better_is,
#' direction_to_flag = "Both", colours = list(neutral = "#490092",
#' improvement = "#00B0F0", deterioration = "#E46C0A", common_cause = "#A6A6A6"))
#' @param nhs_icons_enable A boolean to enable NHS icons for the SPC chart. This
#' option is only available if nhs_colours_enable is TRUE. (default = FALSE)
#' @param nhs_icons_options A list of options for the NHS icons:
#' * flag_last_point_only: A boolean to enable flagging of the last point only
#' when determining the NHS icons. (default = FALSE)
#' * sizex: A numeric value to set the size of the NHS icons as a proportion of
#' the plot's width. (default = 0.075)
#' * sizey: A numeric value to set the size of the NHS icons as a proportion of
#' the plot's height. (default = 0.15)
#' (default = list(flag_last_point_only = FALSE, sizex = 0.075, sizey = 0.15))
#' @param show_legend A boolean to enable legend for the SPC Chart
#' (default = FALSE)
#' @param legend_font_size A numeric value to set the font size of the legend
#' (default = 12)
#' @param source_text Set source text of the chart. If empty ("") or NA, no
#' source will be displayed (default = "Healthcare Quality Intelligence Unit")
#'
#' @import data.table
#'
#' @return Statistical process control chart in plotly for the specific
#' indicator and groups.
#'
#'  ## Additional requirements for this function
#'  NA
#' @export
#' @examples
#' \dontrun{
#' spc_plotly_create(
#'   x = seq(from = as.Date('2019-02-01'),
#'           to = as.Date('2022-01-01'),
#'           by = 'month')-1,
#'   numerator = c(20,25,17,22,18,18,30,29,18,23,17,18,
#'                 15,17,19,21,30,19,15,17,22,24,20,13,
#'                 14,18,14,21,17,27,24,30,25,24,26,24),
#'   denominator = c(86,91,99,110,96,110,96,97,105,87,94,89,
#'                   102,106,107,95,106,132,95,117,97,99,108,106,
#'                   101,96,96,98,105,117,94,77,97,90,106,107),
#'   data_type = 'p',
#'   multiplier = 1,
#'   better_is = "Lower",
#'   title = paste0("Example Indicator", " - ", "Hospital"),
#'   spc_period_start = "2019-01-01",
#'   spc_period_end = "2021-12-31",
#'   x_axis_label = "Date",
#'   y_axis_label = "Proportion",
#'   brand_colour = "#00667B",
#'   actual_colour = "black",
#'   annotation_marker_colour = "grey",
#'   y_dp = 1,
#'   y_format = "Percentage",
#'   x_format = "%b %Y",
#'   patterns = "Yes",
#'   pattern_text_ay = 50,
#'   trend_size = 5,
#'   shift_size = 7,
#'   nhs_colours_enable = TRUE,
#'   show_legend = FALSE,
#'   legend_font_size = 12,
#'   source_text = 'Healthcare Quality Intelligence Unit'
#' )
#' }
spc_plotly_create <- function(
    x,
    numerator,
    denominator = NULL,
    data_type = "p",
    multiplier = 1,
    better_is = "Lower",
    title = "",
    spc_period_start = NA,
    spc_period_end = NA,
    x_axis_label = NA,
    y_axis_label = NA,
    brand_colour = "#00667B",
    actual_colour = "black",
    annotation_marker_colour = "grey",
    line_width = 2,
    marker_size = 10,
    y_dp = 1,
    y_format = "Percentage",
    x_format = "%b %Y",
    patterns = "Yes",
    pattern_text_ay = 50,
    pattern_arrow_width = 2,
    pattern_font_size = 25,
    trend_size = 5,
    shift_size = 7,
    target = NA,
    target_annotation = NA,
    target_options = list(
      legend_name = "Target",
      line_colour = "#FF0000",
      line_width = 2,
      line_dash = "solid"
    ),
    nhs_colours_enable = TRUE,
    nhs_colours_options = list(
      improvement_direction = better_is,
      direction_to_flag = "Both",
      colours = list(
        neutral = "#490092",
        improvement = "#00B0F0",
        deterioration = "#E46C0A",
        common_cause = "#A6A6A6"
      )
    ),
    nhs_icons_enable = FALSE,
    nhs_icons_options = list(
      flag_last_point_only = FALSE,
      sizex = 0.075,
      sizey = 0.15
    ),
    show_legend = FALSE,
    legend_font_size = 12,
    source_text = "Healthcare Quality Intelligence Unit"
) {

  # Helper function to remove NA's
  clear_na <- function(input_df) {
    input_df[is.nan(input_df)] <- NA
    input_df[is.infinite(input_df)] <- NA
    return(input_df)
  }

  # Dealing with undefined global functions or variables
  .data <- . <- period_end <- spc_astro <- spc_trend <- spc_twointhree <-
    spc_shift <- `:=` <- spc_imp_text <- .N <- spc_astro_imp <- spc_trend_imp <-
    spc_twointhree_imp <- spc_shift_imp <- spc_det_text <- spc_astro_det <-
    spc_trend_det <- spc_twointhree_det <- spc_shift_det <-
    actual_marker_fill <- actual_marker_border <- NULL

  # Set the spc chart headings
  spc_heading <- title
  spc_period_start <- as.Date(spc_period_start)
  spc_period_end   <- as.Date(spc_period_end)
  spc_sub_heading <- paste(format(spc_period_start, format = "%b-%y"),
                           format(spc_period_end, format = "%b-%y"),
                           sep = " to ")

  # Calculate the limits using the qic package
  ## If no denominator is provided, do not use
  if (is.null(denominator)) {
    hqiu_spc <- qicharts2::qic(
      x = x,
      y = numerator,
      chart = data_type,
      multiply = multiplier,
      y.percent = TRUE)
  } else {
    hqiu_spc <- qicharts2::qic(
      x = x,
      y = numerator,
      n = denominator,
      chart = data_type,
      multiply = multiplier,
      y.percent = TRUE)
  }

  # Pull out the data from the ggplot2 object hqiu_spc
  hqiu_spc_df <- hqiu_spc$data
  # Default lcl.95 to 0 if empty (affects T-chart)
  hqiu_spc_df$lcl.95[hqiu_spc_df$lcl.95 < 0 | is.na(hqiu_spc_df$lcl.95)] <- 0
  # Convert to date if x is originally date format
  if (inherits(x, "Date")) {
    hqiu_spc_df$x <- hqiu_spc_df$x |> as.Date()
  }
  # Reorder dataset to match existing order
  hqiu_spc_df <- hqiu_spc_df[match(x, hqiu_spc_df$x), ]
  #Clear NaN and Inf values from cl to determine if spc was successful
  hqiu_spc_df <- dplyr::mutate(hqiu_spc_df,
                               dplyr::across(.cols = .data$ucl.95,
                                             .fns = clear_na)) |>
    tidyr::drop_na(.data$y)

  #SPC Plot

  #Checks if confidence limits are below 0, if so set to 0, as negative values
  # are not possible
  if (!is.na(hqiu_spc_df$ucl.95[1])) {
    if (hqiu_spc_df$lcl.95[1] < 0) {
      hqiu_spc_df$lcl.95 <- 0
    }
    if (hqiu_spc_df$lcl[1] < 0) {
      hqiu_spc_df$lcl <- 0
    }
  }

  #change y axis label to rate per multiplier as is only label "rate"
  if (is.na(x_axis_label)) {
    x_axis_label_full <- ""
    b_padding <- 80
    y_padding <- -0.1
  } else {
    x_axis_label_full <- x_axis_label
    b_padding <- 100
    y_padding <- -0.2
  }

  #change y axis label to rate per multiplier as is only label "rate"
  if (is.na(y_axis_label)) {
    y_axis_label_full <- ""
  } else if (y_axis_label == "Rate") {
    y_axis_label_full <- paste("Rate per",
                               formatC(multiplier,
                                       digits = 0, format = "f",
                                       big.mark = ","),
                               sep = " ")
  } else {
    y_axis_label_full <- y_axis_label
  }

  # Set up formats for labels, ticks and scale y figures appropriately
  y_format_d3 <- paste0(
    ",.",
    y_dp,
    ifelse(y_format == "Percentage", "%", "f")
  )

  # adjust scaling of y values for multiplier
  hover_scaling <- ifelse(y_format == "Percentage", 100, 1) # nolint

  # Compute patterns for NHS colours
  if (nhs_colours_enable & data_type != "run") {
    # Run patterns for improvement and deterioration
    nhs_pat_improvement <- qiverse.qipatterns::pattern_rules(
      numerator = numerator,
      denominator = denominator,
      period_end = as.character(x),
      unique_key = NA,
      spc_chart_type = data_type,
      multiplier = multiplier,
      better_is = ifelse(nhs_colours_options$improvement_direction == "Lower", "Higher", "Lower"),
      trend_size = trend_size,
      shift_size = shift_size
    )
    nhs_pat_deterioration <- qiverse.qipatterns::pattern_rules(
      numerator = numerator,
      denominator = denominator,
      period_end = as.character(x),
      unique_key = NA,
      spc_chart_type = data_type,
      multiplier = multiplier,
      better_is = ifelse(nhs_colours_options$improvement_direction == "Lower", "Lower", "Higher"),
      trend_size = trend_size,
      shift_size = shift_size
    )

    # Combine NHS patterns to a single dataframe
    nhs_pat <- data.table::merge.data.table(
      nhs_pat_improvement[, .(period_end, spc_astro_imp = spc_astro,
                              spc_trend_imp = spc_trend,
                              spc_twointhree_imp = spc_twointhree,
                              spc_shift_imp = spc_shift)],
      nhs_pat_deterioration[, .(period_end, spc_astro_det = spc_astro,
                                spc_trend_det = spc_trend,
                                spc_twointhree_det = spc_twointhree,
                                spc_shift_det = spc_shift)],
      by = 'period_end',
      all.x = TRUE,
      sort = FALSE
    )

    # Flag any improvement
    ## Assign the patterns text when improvement is detected
    nhs_pat[, spc_imp_text := sapply(1:nhs_pat[,.N], function(i) {
      patterns <- c(
        ifelse(!is.na(nhs_pat[i, spc_astro_imp]), 'Astronomical', NA),
        ifelse(!is.na(nhs_pat[i, spc_trend_imp]), 'Trend', NA),
        ifelse(!is.na(nhs_pat[i, spc_twointhree_imp]), 'Two in Three', NA),
        ifelse(!is.na(nhs_pat[i, spc_shift_imp]), 'Shift', NA)
      )
      patterns <- patterns[!is.na(patterns)]
      if (length(patterns) == 0) {
        NA
      } else {
        paste0(
          patterns[!is.na(patterns)],
          collapse = ", "
        )
      }
    })]

    # Flag any deterioration
    ## Assign the patterns text when deterioration is detected
    nhs_pat[, spc_det_text := sapply(1:nhs_pat[,.N], function(i) {
      patterns <- c(
        ifelse(!is.na(nhs_pat[i, spc_astro_det]), 'Astronomical', NA),
        ifelse(!is.na(nhs_pat[i, spc_trend_det]), 'Trend', NA),
        ifelse(!is.na(nhs_pat[i, spc_twointhree_det]), 'Two in Three', NA),
        ifelse(!is.na(nhs_pat[i, spc_shift_det]), 'Shift', NA)
      )
      patterns <- patterns[!is.na(patterns)]
      if (length(patterns) == 0) {
        NA
      } else {
        paste0(
          patterns[!is.na(patterns)],
          collapse = ", "
        )
      }
    })]

    # Assign colours
    if (nhs_colours_options$direction_to_flag == "Both") {
      nhs_pat[, actual_marker_fill :=
                # Prioritise deterioration as fill colour
                ifelse(
                  !is.na(spc_det_text),
                  nhs_colours_options$colours$deterioration,
                  # Then improvement
                  ifelse(
                    !is.na(spc_imp_text),
                    nhs_colours_options$colours$improvement,
                    nhs_colours_options$colours$common_cause
                  )
                )
      ]
      # Assign border colour when flagging both
      nhs_pat[, actual_marker_border :=
                # Set improvement as border colour for marker
                ifelse(
                  !is.na(spc_det_text) & !is.na(spc_imp_text),
                  nhs_colours_options$colours$improvement,
                  NA_character_
                )
      ]
    } else if (nhs_colours_options$direction_to_flag == "Improvement") {
      nhs_pat[, actual_marker_fill := ifelse(
        !is.na(spc_imp_text),
        nhs_colours_options$colours$improvement,
        nhs_colours_options$colours$common_cause
      )]
      nhs_pat[, actual_marker_border := NA_character_]
    } else if (nhs_colours_options$direction_to_flag == "Deterioration") {
      nhs_pat[, actual_marker_fill := ifelse(
        !is.na(spc_det_text),
        nhs_colours_options$colours$deterioration,
        nhs_colours_options$colours$common_cause
      )]
      nhs_pat[, actual_marker_border := NA_character_]
    }

    # Set missing marker border to fill colour
    nhs_pat[is.na(actual_marker_border),
            actual_marker_border := actual_marker_fill]

    # Update Colours if better_is was set to Neutral
    if (better_is == "Neutral") {
      nhs_pat[actual_marker_fill != nhs_colours_options$colours$common_cause,
              actual_marker_fill := nhs_colours_options$colours$neutral]
      nhs_pat[actual_marker_border != nhs_colours_options$colours$common_cause,
              actual_marker_border := nhs_colours_options$colours$neutral]
    }

    # Merge back onto hqiu_spc_df
    hqiu_spc_df <- merge(
      hqiu_spc_df,
      nhs_pat[, .(period_end, spc_imp_text, spc_det_text,
                  actual_marker_fill, actual_marker_border)],
      by.x = "x",
      by.y = "period_end",
      all.x = TRUE,
      sort = FALSE
    )

  } else {
    # Otherwise default to actual_colour preset
    hqiu_spc_df$actual_marker_fill <- actual_colour
    hqiu_spc_df$actual_marker_border <- actual_colour
  }

  # Add Target to dataframe
  hqiu_spc_df$target <- target

  # Add function to add target line to plotly object
  spc_plotly_target <- function(p) {
    output <- p |>
      plotly::add_trace(
        name = target_options$legend_name,
        x = ~x,
        y = ~target,
        type = "scatter",
        mode = "lines",
        line = list(
          color = target_options$line_colour,
          width = target_options$line_width,
          dash = target_options$line_dash
        ),
        showlegend = show_legend,
        hoverinfo = "none"
      )

    if (!is.na(target_annotation)) {
      output <- output |>
        plotly::layout(
          annotations =
            list(
              list(
                x = max(hqiu_spc_df$x), xref = "x", xanchor = "right",
                y = target, yref = "y",
                yanchor = ifelse(
                  target > hqiu_spc_df$cl[1],
                  "bottom",
                  "top"
                ),
                text = target_annotation,
                showarrow = FALSE,
                font = list(color = target_options$line_colour)
              )
            )
        )
    }

    output
  }

  # Initialise the plotly object
  spc_plotly <- plotly::plot_ly(
    data = hqiu_spc_df
  )

  # Add legend entry for Upper NHS Colour if enabled
  if (nhs_colours_enable & show_legend) {
    spc_plotly <- spc_plotly |>
      plotly::add_trace(
        name = ifelse(
          better_is == "Lower",
          "Deterioration",
          "Improvement"
        ),
        x = ~x,
        y = ~cl,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = ifelse(
            better_is == "Lower",
            nhs_colours_options$colours$deterioration,
            nhs_colours_options$colours$improvement
          ),
          size = marker_size
        ),
        showlegend = show_legend,
        visible = "legendonly"
      )
  }


  # Check if run chart. If not, add control limits
  if (data_type != "run") {
    spc_plotly <- spc_plotly |>
      # Add the upper control limit
      plotly::add_trace(
        name = "Upper Control Limit 99.8%",
        x = ~x,
        y = ~ucl,
        type = "scatter",
        mode = "lines",
        line = list(color = brand_colour, width = line_width, dash = "dash"),
        showlegend = show_legend,
        hoverinfo = "none"
      ) |>
      # Add the upper warning limit
      plotly::add_trace(
        name = "Upper Warning Limit 95%",
        x = ~x,
        y = ~ucl.95,
        type = "scatter",
        mode = "lines",
        line = list(color = brand_colour, width = line_width, dash = "dot"),
        showlegend = show_legend,
        hoverinfo = "none"
      )
  }

  # Add Target here if it is above the centerline
  if (!is.na(target) & target > hqiu_spc_df$cl[1]) {
    spc_plotly <- spc_plotly_target(spc_plotly)
  }

  # Add actual line
  spc_plotly <- spc_plotly |>
    # Add the data's line, with points added
    plotly::add_trace(
      name = "Actuals",
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "markers+lines",
      marker = ~list(color = actual_marker_fill, size = marker_size,
                     line = list(width = 2, color = actual_marker_border)),
      line = list(color = ifelse(nhs_colours_enable,
                                 nhs_colours_options$colours$common_cause,
                                 actual_colour), width = line_width),
      showlegend = show_legend,
      # Create hoverinfo (tooltip) text for this trace
      hoverinfo = "text",
      text = ~paste0(
        ifelse(inherits(x, "Date"), "<br><b>Date: </b>", ""),
        if (inherits(x, "Date")) {
          format.Date(x, format = x_format)
        } else {
          x
        },
        if (!(data_type %in% c("g", "t"))) {
          paste0(
            "<br><b>Numerator: </b>",
            formatC(y.sum, digits = y_dp, format = "f", big.mark = ",",
                    drop0trailing = TRUE),
            "<br><b>Denominator: </b>",
            formatC(n, digits = y_dp, format = "f", big.mark = ",",
                    drop0trailing = TRUE))
        },
        "<br><b>Value: </b>",
        formatC(y * hover_scaling, digits = y_dp, format = "f", big.mark = ",",
                drop0trailing = TRUE),
        ifelse(y_format == "Percentage", "%", ""),
        if (data_type != "run") {
          paste0("<br><b>Upper 99% Limit: </b>",
                 formatC(ucl * hover_scaling, digits = y_dp, format = "f",
                         big.mark = ","),
                 ifelse(y_format == "Percentage", "%", ""))
        },
        "<br><b>Centerline: </b>",
        formatC(cl * hover_scaling, digits = y_dp,
                format = "f", big.mark = ","),
        ifelse(y_format == "Percentage", "%", ""),
        if (data_type != "run") {
          paste0("<br><b>Lower 99% Limit: </b>",
                 formatC(lcl * hover_scaling, digits = y_dp,
                         format = "f", big.mark = ","),
                 ifelse(y_format == "Percentage", "%", ""))
        },
        if (nhs_colours_enable) {
          paste0(
            ifelse(!is.na(spc_imp_text),
                   paste0("<br><b>Pattern(s) (",
                          ifelse(better_is == "Neutral", "Higher", "Improvement"),
                          "): </b>", spc_imp_text),
                   ""),
            ifelse(!is.na(spc_det_text),
                   paste0("<br><b>Pattern(s) (",
                          ifelse(better_is == "Neutral", "Lower", "Deterioration"),
                          "): </b>", spc_det_text),
                   "")
          )
        }
      ),
      hoverlabel = list(bgcolor = brand_colour)
    )

  # Add the centre line
  spc_plotly <- spc_plotly |>
    plotly::add_trace(
      name = "Centreline",
      x = ~x,
      y = ~cl,
      type = "scatter",
      mode = "lines",
      line = list(color = actual_colour, width = line_width),
      showlegend = show_legend,
      hoverinfo = "none"
    )

  # Add Target here if it is below the centerline
  if (!is.na(target) & target <= hqiu_spc_df$cl[1]) {
    spc_plotly <- spc_plotly_target(spc_plotly)
  }

  # Check if run chart. If not, add control limits
  if (data_type != "run") {
    spc_plotly <- spc_plotly |>
      # Add the lower control limit
      plotly::add_trace(
        name = "Lower Warning Limit 95%",
        x = ~x,
        y = ~lcl.95,
        type = "scatter",
        mode = "lines",
        line = list(color = brand_colour, width = line_width, dash = "dot"),
        showlegend = show_legend,
        hoverinfo = "none"
      ) |>
      # Add the lower warning limit
      plotly::add_trace(
        name = "Lower Control Limit 99.8%",
        x = ~x,
        y = ~lcl,
        type = "scatter",
        mode = "lines",
        line = list(color = brand_colour, width = line_width, dash = "dash"),
        showlegend = show_legend,
        hoverinfo = "none"
      )
  }

  # Add legend entry for Lower NHS Colour if enabled
  if (nhs_colours_enable & show_legend) {
    spc_plotly <- spc_plotly |>
      plotly::add_trace(
        name = ifelse(
          better_is == "Higher",
          "Deterioration",
          "Improvement"
        ),
        x = ~x,
        y = ~cl,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = ifelse(
            better_is == "Higher",
            nhs_colours_options$colours$deterioration,
            nhs_colours_options$colours$improvement
          ),
          size = marker_size
        ),
        showlegend = show_legend,
        visible = "legendonly"
      )
  }

  #labels, see start of chunk for setup
  spc_plotly <- spc_plotly |>
    plotly::layout(
      font = list(family = "Arial", color = "black"),
      title = list(
        font = list(size = 20),
        text = paste0("<b>", spc_heading, "</b><br><sup>",
                      spc_sub_heading, "</sup>")
      ),
      xaxis = list(
        title = x_axis_label_full,
        hoverformat = x_format,
        categoryorder = "trace"
      ),
      yaxis = list(
        title = y_axis_label_full,
        hoverformat = y_format_d3,
        tickformat = y_format_d3
      ),
      showlegend = FALSE,
      hovermode = "x",
      margin = list(b = 0, t = 80)
    )

  # Add legend if enabled
  if (show_legend) {
    spc_plotly <- spc_plotly |>
      plotly::layout(
        showlegend = TRUE,
        legend = list(
          x = 1, y = 0.5,
          xanchor = "left",
          yanchor = "middle",
          orientation = "v",
          traceorder = "normal",
          itemclick = FALSE,
          itemdoubleclick = FALSE,
          font = list(size = legend_font_size)
        )
      )
  }

  # Add source text if exists
  if (source_text != "" && !is.na(source_text)) {
    spc_plotly <- spc_plotly |>
      plotly::layout(
        margin = list(b = b_padding, t = 80),
        # Add source caption in bottom right
        annotations = list(
          x = 1, y = y_padding,
          text = paste0("<i>Source: ", source_text, "</i>"),
          showarrow = FALSE, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "top", xshift = 0, yshift = 0,
          font = list(size = 15)
        )
      )
  }

  # checks if a patterns dataframe is input and if any patterns were detected
  if (is.data.frame(patterns)) {
    filt_pat <- patterns[, c("Astro", "Trend", "TwoInThree", "Shift")]
  } else if (patterns == "Yes" & data_type != "run") {
    # Create dummy denominator when only numerator exists
    if (is.null(denominator)) {
      denominator <- rep(1, length(numerator))
    }
    # Uses the runPat function from Pattern_detection_stripped
    ## Run both sides for neutral
    if (better_is == "Neutral") {
      filt_pat <- rbind(
        qiverse.qipatterns::pattern_rules(
          numerator = numerator,
          denominator = denominator,
          period_end = as.character(x),
          unique_key = NA,
          spc_chart_type = data_type,
          multiplier = multiplier,
          better_is = "Lower",
          trend_size = trend_size,
          shift_size = shift_size
        ) |>
          _[, .(
            Astro = max(spc_astro, na.rm = TRUE) |> suppressWarnings(),
            Trend = max(spc_trend, na.rm = TRUE) |> suppressWarnings(),
            TwoInThree = max(spc_twointhree, na.rm = TRUE) |> suppressWarnings(),
            Shift = max(spc_shift, na.rm = TRUE) |> suppressWarnings()
          )],
        qiverse.qipatterns::pattern_rules(
          numerator = numerator,
          denominator = denominator,
          period_end = as.character(x),
          unique_key = NA,
          spc_chart_type = data_type,
          multiplier = multiplier,
          better_is = "Higher",
          trend_size = trend_size,
          shift_size = shift_size
        ) |>
          _[, .(
            Astro = max(spc_astro, na.rm = TRUE) |> suppressWarnings(),
            Trend = max(spc_trend, na.rm = TRUE) |> suppressWarnings(),
            TwoInThree = max(spc_twointhree, na.rm = TRUE) |> suppressWarnings(),
            Shift = max(spc_shift, na.rm = TRUE) |> suppressWarnings()
          )]
      ) |>
        # Squash to the last pattern identified
        apply(2, function(x) suppressWarnings(max(x, na.rm = TRUE))) |>
        t() |>
        as.data.frame()
    } else {
      filt_pat <- qiverse.qipatterns::pattern_rules(
        numerator = numerator,
        denominator = denominator,
        period_end = as.character(x),
        unique_key = NA,
        spc_chart_type = data_type,
        multiplier = multiplier,
        better_is = better_is,
        trend_size = trend_size,
        shift_size = shift_size
      ) |>
        _[, .(
          Astro = max(spc_astro, na.rm = TRUE) |> suppressWarnings(),
          Trend = max(spc_trend, na.rm = TRUE) |> suppressWarnings(),
          TwoInThree = max(spc_twointhree, na.rm = TRUE) |> suppressWarnings(),
          Shift = max(spc_shift, na.rm = TRUE) |> suppressWarnings()
        )]
    }
  } else {
    filt_pat <- data.frame(NULL)
  }
  # check if dataframe is empty
  if (!rlang::is_empty(filt_pat) && nrow(filt_pat) != 0) {
    # Format x axis as characters to merge
    hqiu_spc_df$x <- as.character(hqiu_spc_df$x)
    #filter columns to only those that are related to patterns,
    # x and y co-ordinates
    pattern_info <- dplyr::filter(
      hqiu_spc_df,
      as.character(filt_pat$Astro) %in% as.character(hqiu_spc_df$x) |
        as.character(filt_pat$Trend) %in% as.character(hqiu_spc_df$x) |
        as.character(filt_pat$Shift) %in% as.character(hqiu_spc_df$x) |
        as.character(filt_pat$TwoInThree) %in% as.character(hqiu_spc_df$x)) |>
      base::subset(select = c("x", "y"))

    if (nrow(pattern_info)) {
      #creates a dataframe that has the dates of patterns as well as the
      # pattern name for the current spc
      pat_info <- dplyr::tibble(
        value = c(as.character(filt_pat$Astro),
                  as.character(filt_pat$Trend),
                  as.character(filt_pat$TwoInThree),
                  as.character(filt_pat$Shift)),
        Pattern = c("&#8786;", "&#9443;", "&#8532;", "&#9442;")) |>
        tidyr::drop_na()
      #joins the two dataframes to now hold the x, y and pattern identifier
      pat_agg <- dplyr::left_join(pat_info, pattern_info, by = c("value" = "x"))

      #creates annotation ax offset if a single point has multiple patterns
      pat_agg$ax <- 0
      for (pat_agg_unique in unique(pat_agg$value)) {
        n_pat_agg_unique <- dplyr::filter(pat_agg,
                                          .data$value == pat_agg_unique) |>
          nrow()
        if (n_pat_agg_unique == 2) {
          pat_agg[pat_agg$value == pat_agg_unique, "ax"] <- c(-20, 20)
        } else if (n_pat_agg_unique == 3) {
          pat_agg[pat_agg$value == pat_agg_unique, "ax"] <- c(-40, 0, 40)
        } else if (n_pat_agg_unique == 4) {
          pat_agg[pat_agg$value == pat_agg_unique, "ax"] <- c(-60, -20, 20, 60)
        }
      }

      # reformat value as date
      if (inherits(x, "Date")) {
        pat_agg$value <- as.Date(pat_agg$value)
        pat_agg$annotation_value <- as.Date(pat_agg$value)
      } else {
        # otherwise set annotation value as index - 1 to place correctly
        pat_agg$annotation_value <- match(pat_agg$value, hqiu_spc_df$x) - 1
      }

      #add to plotly object
      spc_plotly <- spc_plotly |>
        # Add circle
        plotly::add_trace(
          data = pat_agg,
          x = ~value,
          y = ~y,
          type = "scatter",
          mode = "markers",
          opacity = 0.7,
          marker = list(
            color = "rgba(0,0,0,0)",
            size = marker_size * 2,
            line = list(
              color = annotation_marker_colour,
              width = line_width
            )
          ),
          showlegend = FALSE,
          hoverinfo = "none"
        ) |>
        # Add tag around points
        plotly::add_annotations(
          data = pat_agg,
          text = ~Pattern,
          x = ~annotation_value, xref = "x", x_anchor = "auto",
          y = ~y, yref = "y", y_anchor = "auto",
          font = list(size = pattern_font_size),
          arrowcolor = "black",
          arrowwidth = pattern_arrow_width,
          standoff = marker_size,
          ax = ~ax,
          ay = ~ifelse(y > hqiu_spc_df$cl[1], -pattern_text_ay, pattern_text_ay)
        )
    }
  }

  # Add NHS Icons
  if (nhs_icons_enable & nhs_colours_enable & data_type != "run") {

    # Detect which type of icon to use, common_cause, concern or improvement
    if (nhs_icons_options$flag_last_point_only) {
      # Set the icon pattern depending on the detected pattern on the last point
      if (!is.na(nhs_pat[, .SD[.N]]$spc_det_text)) {
        nhs_icon_pattern <- "concern"
      } else if (!is.na(nhs_pat[, .SD[.N]]$spc_imp_text)) {
        nhs_icon_pattern <- "improvement"
      } else {
        nhs_icon_pattern <- "common_cause"
      }
    } else {
      # Extract the last pattern detected in the SPC
      imp_max_period_end <- nhs_pat[!is.na(spc_imp_text), .SD[.N]]
      det_max_period_end <- nhs_pat[!is.na(spc_det_text), .SD[.N]]

      # Set the icon pattern depending on the last detected pattern
      if (imp_max_period_end[,.N] == 0 & det_max_period_end[,.N] == 0) {
        nhs_icon_pattern <- "common_cause"
      } else if (imp_max_period_end[,.N] == 0) {
        nhs_icon_pattern <- "concern"
      } else if (det_max_period_end[,.N] == 0) {
        nhs_icon_pattern <- "improvement"
      } else if (det_max_period_end$period_end >=
                 imp_max_period_end$period_end) {
        nhs_icon_pattern <- "concern"
      } else {
        nhs_icon_pattern <- "improvement"
      }
    }

    # If better_is direction is neutral, set the icon pattern to neutral
    if (nhs_colours_options$improvement_direction == "Neutral" &
        nhs_icon_pattern != "common_cause") {
      if (nhs_icon_pattern == "concern") {
        nhs_icon_pattern <- "neutral_low"
      } else if (nhs_icon_pattern == "improvement") {
        nhs_icon_pattern <- "neutral_high"
      }
    } else if (nhs_colours_options$improvement_direction == "Higher") {
      # Apply correct direction for icon pattern
      if (nhs_icon_pattern == "concern") {
        nhs_icon_pattern <- paste0(nhs_icon_pattern, "_low")
      } else if (nhs_icon_pattern == "improvement") {
        nhs_icon_pattern <- paste0(nhs_icon_pattern, "_high")
      }
    } else if (nhs_colours_options$improvement_direction == "Lower") {
      if (nhs_icon_pattern == "concern") {
        nhs_icon_pattern <- paste0(nhs_icon_pattern, "_high")
      } else if (nhs_icon_pattern == "improvement") {
        nhs_icon_pattern <- paste0(nhs_icon_pattern, "_low")
      }
    }

    # Extract Icon from package
    icon_name <- paste0(nhs_icon_pattern, ".svg")
    icon_loc <- system.file("icons", "spc", "variation", icon_name, package = "qiverse.qiplotly")
    ## Base64 encode the icon as it can't be read directly into plotly
    txt <- RCurl::base64Encode(readBin(icon_loc, "raw", file.info(icon_loc)[1, "size"]), "txt")

    # Add NHS Icon
    spc_plotly <- spc_plotly  |>
      plotly::layout(
        images = list(
          list(
            source = paste("data:image/svg+xml;base64", txt, sep=","),
            layer = "above",
            sizex = nhs_icons_options$sizex,
            sizey = nhs_icons_options$sizey,
            x = 1,
            y = 1,
            xref = "paper",
            yref = "paper",
            xanchor = "right",
            yanchor = "bottom",
            opacity = 1
          )
        )
      )
  }

  # Output plotly object
  return(spc_plotly)
}
