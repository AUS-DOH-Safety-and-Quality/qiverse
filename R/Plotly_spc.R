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
#' standardised ratios such as HSMR, "p" for proportions, "r" for ratios
#' of counts, "run" for run charts, "g" for geometric charts and "t" for time
#' charts. Default is "p".
#' @param multiplier Scale relative risk and funnel by this factor. Default to
#' 1, but 100 sometime used, e.g. in some hospital mortality ratios.
#' @param betteris A string identifying the direction that is favourable for
#' the indicator. "Higher" for points below the lower control limit to be
#' unfavourable, and "Lower" for points above the upper control limit to be
#' unfavourable. Default is "Higher".
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
#' @param trend_size Set the number of points required for a trend to be
#' detected (default = 5)
#' @param shift_size Set the number of points required for a shift to be
#' detected (default = 7)
#' @param nhs_colours_enable A boolean to enable NHS colours for the SPC chart.
#' (default = TRUE)
#' @param nhs_colours_options A list of parameters to enable NHS colours for the SPC
#' chart. (default = list(improvement_direction = betteris,
#' direction_to_flag = "Both", colours = list(neutral = "#490092",
#' improvement = "#00B0F0", deterioration = "#E46C0A", common_cause = "#A6A6A6"))
#' @param source_text Set source text of the chart. If empty ("") or NA, no
#' source will be displayed (default = "Healthcare Quality Intelligence Unit")
#'
#' @import data.table
#'
#' @return Statistical process control chart in plotly for the specific
#' indicator and establishment.
#'
#'  ## Additional requirements for this function
#'  NA
#' @export
#' @examples -
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
#'   betteris = "Lower",
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
#'   source_text = 'Healthcare Quality Intelligence Unit'
#' )
#' }
spc_plotly_create <- function(
  x,
  numerator,
  denominator = NULL,
  data_type = "p",
  multiplier = 1,
  betteris = "Lower",
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
  trend_size = 5,
  shift_size = 7,
  nhs_colours_enable = TRUE,
  nhs_colours_options = list(
    improvement_direction = betteris,
    direction_to_flag = "Both", # TODO add params (Both, Improvement, Deterioration)
    colours = list(
      neutral = "#490092", # TODO Currently not used
      improvement = "#00B0F0",
      deterioration = "#E46C0A",
      common_cause = "#A6A6A6"
    )
  ),
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
  if (nhs_colours_enable) {
    # Run patterns for improvement and deterioration
    nhs_pat_improvement <- qiverse.qipatterns::pattern_rules(
      numerator = numerator,
      denominator = denominator,
      period_end = as.character(x),
      unique_key = NA,
      spccharttype = data_type,
      multiplier = multiplier,
      betteris = ifelse(nhs_colours_options$improvement_direction == "Lower", "Higher", "Lower"),
      fpl_astro = NA,
      trend_size = trend_size,
      shift_size = shift_size
    )
    nhs_pat_deterioration <- qiverse.qipatterns::pattern_rules(
      numerator = numerator,
      denominator = denominator,
      period_end = as.character(x),
      unique_key = NA,
      spccharttype = data_type,
      multiplier = multiplier,
      betteris = nhs_colours_options$improvement_direction,
      fpl_astro = NA,
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
    hqiu_spc_df[, actual_marker_fill := actual_colour]
    hqiu_spc_df[, actual_marker_border := actual_colour]
  }

  spc_plotly <- plotly::plot_ly(
    data = hqiu_spc_df
  ) |>
    # Add the centre line
    plotly::add_trace(
      name = "Mean",
      x = ~x,
      y = ~cl,
      type = "scatter",
      mode = "lines",
      line = list(color = actual_colour, width = line_width),
      showlegend = FALSE,
      hoverinfo = "none"
    )

  # Check if run chart. If not, add control limits
  if (data_type != "run") {
    spc_plotly <- spc_plotly |>
      # Add the upper control limit
      plotly::add_trace(
        name = "Upper Control Limit (95%)",
        x = ~x,
        y = ~ucl.95,
        type = "scatter",
        mode = "lines",
        line = list(color = brand_colour, width = line_width, dash = "dot"),
        showlegend = FALSE,
        hoverinfo = "none"
      ) |>
      # Add the upper warning limit
      plotly::add_trace(
        name = "Upper Warning Limit (99%)",
        x = ~x,
        y = ~ucl,
        type = "scatter",
        mode = "lines",
        line = list(color = brand_colour, width = line_width, dash = "dash"),
        showlegend = FALSE,
        hoverinfo = "none"
      ) |>
      # Add the lower warning limit
      plotly::add_trace(
        name = "Lower Warning Limit (99%)",
        x = ~x,
        y = ~lcl,
        type = "scatter",
        mode = "lines",
        line = list(color = brand_colour, width = line_width, dash = "dash"),
        showlegend = FALSE,
        hoverinfo = "none"
      ) |>
      # Add the lower control limit
      plotly::add_trace(
        name = "Lower Control Limit (95%)",
        x = ~x,
        y = ~lcl.95,
        type = "scatter",
        mode = "lines",
        line = list(color = brand_colour, width = line_width, dash = "dot"),
        showlegend = FALSE,
        hoverinfo = "none"
      )
  }

  # Add actual line
  spc_plotly <- spc_plotly |>
    # Add the data's line, with points added
    plotly::add_trace(
      name = "Actual",
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "markers+lines",
      marker = ~list(color = actual_marker_fill, size = marker_size,
                    line = list(width = 2, color = actual_marker_border)),
      line = list(color = actual_colour, width = line_width),
      showlegend = FALSE,
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
                   paste0("<br><b>Pattern(s) (Improvement) </b>", spc_imp_text),
                   ""),
            ifelse(!is.na(spc_det_text),
                   paste0("<br><b>Pattern(s) (Deterioration): </b>", spc_det_text),
                   "")
          )
        }
      ),
      hoverlabel = list(bgcolor = brand_colour)
    ) |>
    #labels, see start of chunk for setup
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
  } else if (patterns == "Yes") {
    # Create dummy denominator when only numerator exists
    if (is.null(denominator)) {
      denominator <- rep(1, length(numerator))
    }
    # Uses the runPat function from Pattern_detection_stripped
    filt_pat <- qiverse.qipatterns::runPat(numerator, denominator,
                                           as.character(x),
                                           data_type, multiplier, betteris,
                                           trend_size, shift_size)
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
          hoverinfo = "none"
        ) |>
        # Add tag around points
        plotly::add_annotations(
          data = pat_agg,
          text = ~Pattern,
          x = ~annotation_value, xref = "x", x_anchor = "auto",
          y = ~y, yref = "y", y_anchor = "auto",
          font = list(size = 25),
          arrowcolor = "black",
          standoff = marker_size,
          ax = ~ax,
          ay = ~ifelse(y > hqiu_spc_df$cl[1], -pattern_text_ay, pattern_text_ay)
        )
    }
  }

  # Output plotly object
  return(spc_plotly)
}
