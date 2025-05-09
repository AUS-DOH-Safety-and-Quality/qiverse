#' Draw a funnel plot in plotly
#'
#' @param numerator A vector of the numerator (observed events/counts) values.
#' Used as the numerator of the Y-axis
#' @param denominator A vector of denominator (predicted/population etc.) Used
#' as denominator of the Y-axis and the scale of the x-axis
#' @param group A vector of group names as character or factor. Used to
#' aggregate and group points on plots.
#' @param data_type A string identifying the type of data used for in the plot,
#' the adjustment used and the reference point. One of: "SR" for indirectly
#' standardised ratios such as HSMR, "PR" for proportions, or "RC" for ratios
#' of counts. Default is "PR".
#' @param allow_overdispersion A boolean value to allow the use of overdispersion
#' control limits. Default is FALSE.
#' @param multiplier Scale relative risk and funnel by this factor. Default to
#' 1, but 100 sometime used, e.g. in some hospital mortality ratios.
#' @param better_is A string identifying the direction that is favourable for
#' the indicator. "Higher" for points below the lower control limit to be
#' unfavourable, "Lower" for points above the upper control limit to be
#' unfavourable, and "Neutral" if the direction is not stated. Default is
#' "Higher".
#' @param title A string for the title of the plot.
#' @param group_name A vector of the group names that may differ from the
#' initial group identifiers. I.e. group is an alphanumeric code and group_name
#' is the actual name.
#' @param short_group_name A vector of shortened group names to be displayed
#' for highlighted groups and outlier points.
#' @param parent_group_name A vector of parent group names which are to be
#' displayed in the tooltip. These are the major categories for the groups.
#' @param parent_group_name_label A string for the tooltip label on the parent
#' group name.
#' @param funnel_period_start A date (or character of format #' "yyyy-mm-dd")
#' for the start date of the funnel period.
#' @param funnel_period_end A date (or character of format #' "yyyy-mm-dd")
#' for the end date of the funnel period.
#' @param y_limit_lower A value denoting the y-axis lower limit range
#' @param y_limit_upper A value denoting the y-axis upper limit range
#' @param x_axis_label A value denoting the x-axis label. Usually NA or a specified label.
#' @param y_axis_label A value denoting the y-axis label. Usually NA, a specified label or "Rate",
#' for items which are proportion based with multipliers.
#' @param highlight_group Optional group(s) to highlight individually
#' @param highlight_outlier Boolean, if FALSE do not highlight outliers
#' @param highlight_outlier_options Options to apply if highlight_outlier is TRUE.
#' Default is list(direction_to_flag = "Deterioration"), where direction_to_flag
#' can be "Deterioration", "Improvement" or "Both".
#' @param brand_colour Hex code for the colour of the funnel limits. Default is
#' "#00667b"
#' @param actual_colour Hex code for the colour actual data points. Default is
#' "black"
#' @param annotation_marker_colour Hex code for the colour actual data points.
#' Default is "grey"
#' @param line_width A value denoting the width of the line in pixels
#' (default = 3)
#' @param marker_size A value denoting the size of the markers in pixels
#' (default = 8)
#' @param marker_symbol A value denoting the symbol type. See plotly documentation for
#' further options (default = 'circle')
#' @param y_dp Number of decimal points for the y-axis ticks and hoverinfo.
#' Default is 1
#' @param y_format Set to either "Numeric" or "Percentage" to format the y-axis.
#' Default is "Percentage"
#' @param pattern_text_ay Set the y offset for pattern detection text in pixels
#' (default = 50)
#' @param pattern_arrow_width Set the width of the pattern arrow in pixels
#' (default = 2)
#' @param pattern_font_size Numeric value for the font size of the annotation.
#' Default is 15
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
#' @param show_legend A boolean to enable legend for the Funnel Plot
#' (default = FALSE)
#' @param legend_font_size A numeric value to set the font size of the legend
#' (default = 12)
#' @param source_text Set source text of the chart. If empty ("") or NA, no
#' source will be displayed (default = "Healthcare Quality Intelligence Unit")
#'
#' @return Funnel plot in plotly for the specified indicator
#'
#'  ## Additional requirements for this function
#'  The group vector must be a vector of unique values that are associated with
#'  a numerator and a denominator. Any summation of numerators and denominators
#'  by group must be done prior to input into this function.
#' @export
#' @examples
#' \dontrun{
#' # Minimal Example
#'
#' fpl_plotly_create(
#'   numerator = (sapply(1:5, function(x) rpois(5, x * 20 * 0.6)) |> c() -
#'                  (runif(25) * 10) |> round(0)) |>
#'     sapply(function(x) ifelse(x<0, 0, x)),
#'   denominator = sapply(1:5, function(x) rpois(5, x * 20)) |> c(),
#'   group = paste0('Q', 1:25),
#'   data_type = 'PR',
#'   multiplier = 1,
#'   better_is = "Higher",
#'   title = "Test Indicator",
#'   y_format = "Percentage",
#'   x_axis_label = "This is X Axis",
#'   y_axis_label = "This is Y Axis"
#' )
#'
#' # Full Example
#' fpl_plotly_create(
#'   numerator = (sapply(1:5, function(x) rpois(5, x * 20 * 0.6)) |> c() -
#'                  (runif(25) * 10) |> round(0)) |>
#'     sapply(function(x) ifelse(x<0, 0, x)),
#'   denominator = sapply(1:5, function(x) rpois(5, x * 20)) |> c(),
#'   group = paste0('Q', 1:25),
#'   data_type = 'PR',
#'   multiplier = 1,
#'   better_is = "Higher",
#'   title = "Test Indicator",
#'   group_name = c("Alfa", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot",
#'                  "Golf","Hotel", "India", "Juliett", "Kilo", "Lima", "Mike",
#'                  "November","Oscar", "Papa", "Quebec", "Romeo", "Sierra",
#'                  "Tango", "Uniform", "Victor", "Whiskey", "X-ray", "Yankee"),
#'   short_group_name = LETTERS[1:25],
#'   parent_group_name = "Code Words",
#'   parent_group_name_label = "Category",
#'   funnel_period_start = '2021-07-01',
#'   funnel_period_end = '2022-06-30',
#'   y_limit_lower = NA,
#'   y_limit_upper = 85,
#'   x_axis_label = "This is X Axis",
#'   y_axis_label = "This is Y Axis",
#'   highlight_group = c('Q2', 'Q5'),
#'   highlight_outlier = TRUE,
#'   nhs_colours_enable = TRUE,
#'   show_legend = FALSE,
#'   legend_font_size = 12,
#'   y_format = "Percentage",
#'   source_text = 'Healthcare Quality Intelligence Unit'
#' )
#' }
fpl_plotly_create <- function(
    numerator,
    denominator,
    group,
    data_type = "PR",
    allow_overdispersion = FALSE,
    multiplier = 1,
    better_is = "Higher",
    title = "",
    group_name = NULL,
    short_group_name = NULL,
    parent_group_name = NULL,
    parent_group_name_label = NA,
    funnel_period_start = NA,
    funnel_period_end = NA,
    y_limit_lower = NA,
    y_limit_upper = NA,
    x_axis_label = NA,
    y_axis_label = NA,
    highlight_group = NULL,
    highlight_outlier = TRUE,
    highlight_outlier_options = list(direction_to_flag = "Deterioration"),
    brand_colour = "#00667B",
    actual_colour = "black",
    annotation_marker_colour = "grey",
    line_width = 3,
    marker_size = 8,
    marker_symbol = "circle",
    y_dp = 1,
    y_format = "Percentage",
    pattern_text_ay = 50,
    pattern_arrow_width = 2,
    pattern_font_size = 15,
    target = NA,
    target_annotation = NA,
    target_options = list(
      legend_name = "Target",
      line_colour = "#FF0000",
      line_width = 1.5,
      line_dash = "solid"
    ),
    nhs_colours_enable = TRUE,
    nhs_colours_options = list(
      improvement_direction = better_is,
      direction_to_flag = "Both",
      colours = list(
        neutral = "#490092", # TODO Currently not used
        improvement = "#00B0F0",
        deterioration = "#E46C0A",
        common_cause = "#A6A6A6"
      )
    ),
    show_legend = FALSE,
    legend_font_size = 12,
    source_text = "Healthcare Quality Intelligence Unit"
) {
  # Dealing with undefined global functions or variables
  .data <- LCL99 <- UCL99 <- outlier_3sigma <- rr <- marker_colour <-
    OD95LCL <- OD95UCL <- OD99LCL <- OD99UCL <-
    odll95 <- odul95 <- odll998 <- odul998 <- NULL

  # Check Inputs
  len_denominator <- length(denominator)
  len_numerator <- length(numerator)
  len_group <- length(group)

  ## Check that lengths are equal
  if (len_denominator != len_numerator ||
      len_denominator != len_group ||
      len_numerator != len_group) {
    stop("denominator, numerator and group are not equal lengths")
  }

  ## Return early if 0 or 1 rows, shouldn't funnel with these
  if (len_denominator %in% c(0, 1) ||
      len_numerator %in% c(0, 1) ||
      len_group %in% c(0, 1)) {
    stop("There are only 0 or 1 data points. Unable to generate funnel plot.")
  }

  # Generate funnel plot data
  FunnelPlotR_version <-
    asNamespace("FunnelPlotR")$`.__NAMESPACE__.`$spec[["version"]]
  # For FunnelPlotR versions below 0.5.0
  if (FunnelPlotR_version < "0.5.0") {
    funnel <- FunnelPlotR::funnel_plot(
      denominator = denominator,
      numerator = numerator,
      group = group,
      limit = 99,
      data_type = data_type,
      sr_method = "CQC",
      multiplier = multiplier,
      draw_unadjusted = TRUE,
      draw_adjusted = TRUE,
      label = NA,
      x_range = c(0, max(denominator) * 1.15),
      highlight  = NA
    )
  }

  # For FunnelPlotR version 0.5.0 and above
  if (FunnelPlotR_version >= "0.5.0") {
    funnel <- FunnelPlotR::funnel_plot(
      .data = data.frame(
        denominator = denominator,
        numerator = numerator,
        group = group
      ),
      denominator = denominator,
      numerator = numerator,
      group = group,
      limit = 99,
      data_type = data_type,
      sr_method = "CQC",
      multiplier = multiplier,
      draw_unadjusted = TRUE,
      draw_adjusted = TRUE,
      label = NA,
      x_range = c(0, max(denominator) * 1.15),
      highlight  = NA
    )
  }

  # Extract funnel data
  funnel_data <- funnel$plot$data
  ## Reorder data to match initial group
  funnel_data <- funnel_data[match(group, funnel_data$group), ]
  # Pulls out the data for the limits of the plot
  lim_data <- funnel$limits_lookup

    # Check if need to use overdispersion control limits
  if (allow_overdispersion) {
    funnel_data <- funnel_data |>
      dplyr::mutate(
        LCL95 = OD95LCL,
        UCL95 = OD95UCL,
        LCL99 = OD99LCL,
        UCL99 = OD99UCL
      )
    lim_data <- lim_data |>
      dplyr::mutate(
        ll95 = odll95,
        ul95 = odul95,
        ll998 = odll998,
        ul998 = odul998
      )
  }

  # Create the labels for our funnel plot
  if (!is.na(funnel_period_start) && !is.na(funnel_period_end)) {
    date_range <- paste(format(funnel_period_start |> as.Date(),
                               format = "%b-%y"),
                        format(funnel_period_end |> as.Date(),
                               format = "%b-%y"),
                        sep = " to ")
  } else {
    date_range <- ""
  }

  #checks chart type and sets values for center line per chart
  if (data_type == "PR") {
    centre_line <- sum(numerator) / sum(denominator)
  } else if (data_type == "SR") {
    centre_line <- 1
  } else {
    centre_line <- sum(numerator) / sum(denominator)
  }
  centre_line <- centre_line * multiplier

  # Outlier flagging
  ## Flag 3 sigma outliers
  funnel_data <- funnel_data |>
    dplyr::mutate(outlier_3sigma = ifelse(
      rr * multiplier > UCL99 | rr * multiplier < LCL99,
      1, 0
    ))

  # NHS Colours
  if (nhs_colours_enable == TRUE) {
    if (nhs_colours_options$direction_to_flag == "Both") {
      if (nhs_colours_options$improvement_direction %in% c("Higher", "Neutral")) {
        funnel_data <- funnel_data |>
          dplyr::mutate(marker_colour = dplyr::if_else(
            outlier_3sigma == 1 & rr * multiplier > UCL99,
            nhs_colours_options$colours$improvement,
            dplyr::if_else(outlier_3sigma == 1 & rr * multiplier < LCL99,
                           nhs_colours_options$colours$deterioration,
                           actual_colour)))
      } else if (nhs_colours_options$improvement_direction %in%
                 c("Lower", "Neutral")) {
        funnel_data <- funnel_data |>
          dplyr::mutate(marker_colour = dplyr::if_else(
            outlier_3sigma == 1 & rr * multiplier < LCL99,
            nhs_colours_options$colours$improvement,
            dplyr::if_else(outlier_3sigma == 1 & rr * multiplier > UCL99,
                           nhs_colours_options$colours$deterioration,
                           actual_colour)))
      }
    } else if (nhs_colours_options$direction_to_flag == "Improvement") {
      if (nhs_colours_options$improvement_direction == "Higher") {
        funnel_data <- funnel_data |>
          dplyr::mutate(marker_colour = dplyr::if_else(
            outlier_3sigma == 1 & rr * multiplier > UCL99,
            nhs_colours_options$colours$improvement,
            actual_colour))
      } else if (nhs_colours_options$improvement_direction == "Lower") {
        funnel_data <- funnel_data |>
          dplyr::mutate(marker_colour = dplyr::if_else(
            outlier_3sigma == 1 & rr * multiplier < LCL99,
            nhs_colours_options$colours$improvement,
            actual_colour))
      }
    } else if (nhs_colours_options$direction_to_flag == "Deterioration") {
      if (nhs_colours_options$improvement_direction == "Higher") {
        funnel_data <- funnel_data |>
          dplyr::mutate(marker_colour = dplyr::if_else(
            outlier_3sigma == 1 & rr * multiplier < LCL99,
            nhs_colours_options$colours$deterioration,
            actual_colour))
      } else if (nhs_colours_options$improvement_direction == "Lower") {
        funnel_data <- funnel_data |>
          dplyr::mutate(marker_colour = dplyr::if_else(
            outlier_3sigma == 1 & rr * multiplier > UCL99,
            nhs_colours_options$colours$deterioration,
            actual_colour))
      }
    }

    # Update Colours if better_is was set to Neutral
    actual_colour_original <- actual_colour
    if (better_is == "Neutral") {
      funnel_data <- funnel_data |>
        dplyr::mutate(marker_colour = dplyr::if_else(
          marker_colour != actual_colour_original,
          nhs_colours_options$colours$neutral,
          marker_colour))
    }

  } else {
    # Set to default actual colour is NHS colours is not enabled
    funnel_data <- funnel_data |>
      dplyr::mutate(marker_colour = actual_colour)
  }

  # Marker size and symbols
  funnel_data$marker_sizes <- marker_size
  funnel_data$marker_symbols <- marker_symbol

  #change x axis label if not NA
  if (is.na(x_axis_label)) {
    x_axis_label_full <- ""
    margin_b_padding <- 80
    source_y <- -0.1
  } else {
    x_axis_label_full <- x_axis_label
    margin_b_padding <- 100
    source_y <- -0.2
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

  # adjust hover scaling for percentages
  hover_scaling <- ifelse(y_format == "Percentage", 100, 1) # nolint

  # hover label text
  hover_label_text <- paste0(
    ## Group Name
    "<b>",
    if (is.null(group_name)) {
      group
    } else {
      group_name
    },
    "</b>",
    ## Parent Group
    ifelse(is.na(parent_group_name), "",
           paste0("<br><b>", parent_group_name_label, ": </b>")),
    if (is.null(parent_group_name)) {
      ""
    } else {
      parent_group_name
    },
    ## Numerator
    "<br><b>Numerator: </b>",
    formatC(numerator, digits = y_dp, format = "f", big.mark = ",",
            drop0trailing = TRUE),
    ## Denominator
    "<br><b>Denominator: </b>",
    formatC(denominator, digits = y_dp, format = "f", big.mark = ",",
            drop0trailing = TRUE),
    ## Value
    "<br><b>Value: </b>",
    formatC(funnel_data$rr * multiplier * hover_scaling,
            digits = y_dp, format = "f", big.mark = ","),
    ifelse(y_format == "Percentage", "%", ""),
    ## Upper 99.8% Limit
    "<br><b>Upper 99% Limit: </b>",
    formatC(funnel_data$UCL99 * hover_scaling, digits = y_dp,
            format = "f", big.mark = ","),
    ifelse(y_format == "Percentage", "%", ""),
    ## Centerline
    "<br><b>Centerline: </b>",
    formatC(centre_line * hover_scaling, digits = y_dp, format = "f",
            big.mark = ","), ifelse(y_format == "Percentage", "%", ""),
    ## Lower 99.8% Limit
    "<br><b>Lower 99% Limit: </b>",
    formatC(funnel_data$LCL99 * hover_scaling, digits = y_dp,
            format = "f", big.mark = ","),
    ifelse(y_format == "Percentage", "%", ""),
    ## Outlier
    ifelse(funnel_data$outlier_3sigma == 1,
           "<br><b>Pattern(s): </b>Three Sigma Outlier",
           "")
  )

  # Add function to add target line to plotly object
  fpl_plotly_target <- function(p) {
    output <- p |>
      plotly::add_trace(
        name = target_options$legend_name,
        data = lim_data,
        x = ~number.seq,
        y = target,
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
                x = 1, xref = "paper", xanchor = "right",
                y = target, yref = "y",
                yanchor = ifelse(
                  target > centre_line,
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

  # create funnel plotly
  fpl_plotly <- plotly::plot_ly()

  # Add legend entry for Upper NHS Colour if enabled
  if (nhs_colours_enable & show_legend) {
    fpl_plotly <- fpl_plotly |>
      plotly::add_trace(
        name = ifelse(
          better_is == "Lower",
          "Unfavourable",
          "Favourable"
        ),
        x = 0,
        y = centre_line,
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

  # Add the Upper Limits
  fpl_plotly <- fpl_plotly |>
    # Add the upper control limit
    plotly::add_trace(
      name = "Upper Control Limit 99.8%",
      data = lim_data,
      x = ~number.seq,
      y = ~ul998,
      type = "scatter",
      mode = "lines",
      line = list(color = brand_colour, width = line_width, dash = "dash"),
      showlegend = show_legend,
      hoverinfo = "none"
    ) |>
    # Add the upper warning limit
    plotly::add_trace(
      name = "Upper Warning Limit 95%",
      data = lim_data,
      x = ~number.seq,
      y = ~ul95,
      type = "scatter",
      mode = "lines",
      line = list(color = brand_colour, width = line_width, dash = "dot"),
      showlegend = show_legend,
      hoverinfo = "none"
    )

  # Add Target here if it is above the centerline
  if (!is.na(target) & target > centre_line) {
    fpl_plotly <- fpl_plotly_target(fpl_plotly)
  }

  # Add the points line
  fpl_plotly <- fpl_plotly |>
    plotly::add_trace(
      name = "Sites",
      # Exclude those without a denominator
      data = funnel_data[funnel_data$denominator != 0,],
      x = ~denominator,
      y = ~(rr * multiplier),
      type = "scatter",
      mode = "markers",
      opacity = 1,
      marker = ~list(color = marker_colour, line = list(color = marker_colour, width = 0), size = marker_sizes, symbol = marker_symbols, opacity = 1),
      showlegend = show_legend,
      # Create hoverinfo (tooltip) text for this trace
      hoverinfo = "text",
      text = hover_label_text[funnel_data$denominator != 0],
      hoverlabel = list(bgcolor = brand_colour)
    )

  # Add the centerline
  fpl_plotly <- fpl_plotly |>
    plotly::add_trace(
      name = "Centerline",
      data = lim_data,
      x = ~number.seq,
      y = centre_line,
      type = "scatter",
      mode = "lines",
      line = list(color = actual_colour, width = line_width / 2),
      showlegend = show_legend,
      hoverinfo = "none"
    )

  # Add Target here if it is above the centerline
  if (!is.na(target) & target <= centre_line) {
    fpl_plotly <- fpl_plotly_target(fpl_plotly)
  }

  # Add the lower limits
  fpl_plotly <- fpl_plotly |>
    # Add the lower warning limit
    plotly::add_trace(
      name = "Lower Warning Limit 95%",
      data = lim_data,
      x = ~number.seq,
      y = ~ll95,
      type = "scatter",
      mode = "lines",
      line = list(color = brand_colour, width = line_width, dash = "dot"),
      showlegend = show_legend,
      hoverinfo = "none"
    ) |>
    # Add the lower control limit
    plotly::add_trace(
      name = "Lower Control Limit 99.8%",
      data = lim_data,
      x = ~number.seq,
      y = ~ll998,
      type = "scatter",
      mode = "lines",
      line = list(color = brand_colour, width = line_width, dash = "dash"),
      showlegend = show_legend,
      hoverinfo = "none"
    )

  # Add legend entry for Upper NHS Colour if enabled
  if (nhs_colours_enable & show_legend) {
    fpl_plotly <- fpl_plotly |>
      plotly::add_trace(
        name = ifelse(
          better_is == "Higher",
          "Unfavourable",
          "Favourable"
        ),
        x = 0,
        y = centre_line,
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

  # add layout options for titles
  fpl_plotly <- fpl_plotly |>
    plotly::layout(
      font = list(family = "Arial", color = "black"),
      title = list(
        font = list(size = 20),
        text = paste0("<b>", title,
                      "</b><br><sup>", date_range, "</sup>")
      ),
      xaxis = list(
        title = x_axis_label_full,
        hoverformat = ".0f"
      ),
      yaxis = list(
        title = y_axis_label_full,
        hoverformat = y_format_d3,
        tickformat = y_format_d3
      ),
      hovermode = "closest",
      margin = list(b = 0, t = 80)
    )

  # Add legend if enabled
  if (show_legend) {
    fpl_plotly <- fpl_plotly |>
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
    fpl_plotly <- fpl_plotly |>
      plotly::layout(
        margin = list(b = margin_b_padding, t = 80),
        # Add source caption in bottom right
        annotations = list(
          x = 1, y = source_y,
          text = paste0("<i>Source: ", source_text, "</i>"),
          showarrow = FALSE, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "top", xshift = 0, yshift = 0,
          font = list(size = 15)
        )
      )
  }

  # Set highlight points to those that are NHS coloured
  if (nhs_colours_enable) {
    highlight_points <- ifelse(
      if (highlight_outlier_options$direction_to_flag == "Both") {
        funnel_data$marker_colour != actual_colour
      } else if (highlight_outlier_options$direction_to_flag == "Improvement") {
        funnel_data$marker_colour == nhs_colours_options$colours$improvement
      } else if (highlight_outlier_options$direction_to_flag ==
                 "Deterioration") {
        funnel_data$marker_colour == nhs_colours_options$colours$deterioration
      },
      funnel_data$rr*multiplier,
      NA
    )
  } else {
    # Otherwise set highlight points to those that are outliers
    highlight_points <- ifelse(
      if (highlight_outlier_options == "Both") {
        funnel_data$outlier_3sigma == 1
      } else if (highlight_outlier_options$direction_to_flag == "Improvement") {
        if (better_is == "Lower") {
          funnel_data$outlier_3sigma == 1 &
            funnel_data$rr * multiplier < funnel_data$LCL99
        } else if (better_is == "Higher") {
          funnel_data$outlier_3sigma == 1 &
            funnel_data$rr * multiplier > funnel_data$UCL99
        }
      } else if (highlight_outlier_options$direction_to_flag ==
                 "Deterioration") {
        if (better_is == "Lower") {
          funnel_data$outlier_3sigma == 1 &
            funnel_data$rr * multiplier > funnel_data$UCL99
        } else if (better_is == "Higher") {
          funnel_data$outlier_3sigma == 1 &
            funnel_data$rr * multiplier < funnel_data$LCL99
        }
      },
      funnel_data$rr * multiplier,
      NA
    )
  }

  # create a dataframe that holds the group names to serve as a lookup table
  # We only have the group in the plot output so we need this to
  # find the group_name
  outlier_label <- if (!is.null(short_group_name)) {
    short_group_name
  } else if (!is.null(group_name)) {
    group_name
  } else {
    group
  }
  # create a dataframe that holds the points on the graph, so that we can
  # overlay them with a circle and group name
  outlier_lookup <- data.frame(x = denominator,
                               y = highlight_points,
                               outlier_label = outlier_label,
                               marker_size = marker_size,
                               group = group) |>
    #drop values that aren't outliers
    tidyr::drop_na(.data$y)

  if (highlight_outlier == TRUE && (nrow(outlier_lookup) > 0)) {
    # add to plotly object
    fpl_plotly <- fpl_plotly |>
      # Add marker, over the top of outliers, that is a grey hollow circle
      plotly::add_trace(
        data = outlier_lookup,
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = "markers",
        opacity = 0.7,
        marker = list(
          color = "rgba(0,0,0,0)",
          size = ~marker_size * 2.5,
          line = list(
            color = "grey",
            width = line_width
          )
        ),
        showlegend = FALSE,
        hoverinfo = "none"
      ) |>
      # Add tag around points, over the top of outliers, that is text relaying
      # the name of the group for that outlier
      plotly::add_annotations(
        data = outlier_lookup,
        text = ~outlier_label,
        x = ~x, xref = "x", x_anchor = "auto",
        y = ~y, yref = "y", y_anchor = "auto",
        font = list(size = pattern_font_size),
        arrowcolor = "black",
        arrowwidth = pattern_arrow_width,
        standoff = ~marker_size + 2,
        ax = 30,
        ay = ~ifelse(y > centre_line, -pattern_text_ay, pattern_text_ay)
      )

  }
  # check if the group input is in the data provided
  if (nrow(outlier_lookup) > 0 & highlight_outlier == TRUE) {
    highlight_group <- highlight_group[!(highlight_group %in% outlier_lookup$group)]
  }

  if (length(highlight_group) > 0) {
    # define highlighted groups and set labels
    highlight <- data.frame(
      group = highlight_group,
      highlight_label = outlier_label[match(highlight_group, group)]
    )
    # join by groups to find highlighted point's x and y values
    highlight_point <- dplyr::inner_join(highlight, funnel_data,
                                         by = "group")
    # if point exists in funnel data
    if (nrow(highlight_point) > 0) {
      fpl_plotly <- fpl_plotly |>
        # Add marker, over the top of outliers, that is a grey hollow circle
        plotly::add_trace(
          data = highlight_point,
          x = ~denominator,
          y = ~rr * multiplier,
          type = "scatter",
          mode = "markers",
          opacity = 0.7,
          marker = list(
            color = "rgba(0,0,0,0)",
            size = ~marker_sizes * 2.5,
            line = list(
              color = annotation_marker_colour,
              width = line_width
            )
          ),
          showlegend = FALSE,
          hoverinfo = "none"
        ) |>
        # Add tag around points with group names, over the top of
        # outliers, that is text relaying the name of the group for that
        # outlier
        plotly::add_annotations(
          data = highlight_point,
          text = ~highlight_label,
          x = ~denominator, xref = "x", x_anchor = "auto",
          y = ~rr * multiplier,
          yref = "y",
          y_anchor = "auto",
          font = list(size = pattern_font_size),
          arrowcolor = "black",
          arrowwidth = pattern_arrow_width,
          standoff = ~marker_sizes + 2,
          ax = 30,
          ay = ~ifelse((rr * multiplier) > centre_line, -pattern_text_ay, pattern_text_ay)
        )
    }
  }

  y_limit_scaling <- ifelse(data_type == "PR" & multiplier == 1, 100, 1)

  #cut off limits
  #if a y minimum AND y maximum is supplied use both for y limits
  if (!is.na(y_limit_lower) && !is.na(y_limit_upper)) {
    return(
      fpl_plotly |>
        plotly::layout(
          yaxis = list(range = list(y_limit_lower / y_limit_scaling,
                                    y_limit_upper / y_limit_scaling)))
    )
  }
  # If only y minimum is supplied use from it to 1 - only works for proportions
  if (!is.na(y_limit_lower)) {
    return(
      fpl_plotly |>
        plotly::layout(
          yaxis = list(range = list(y_limit_lower / y_limit_scaling, 1)))
    )
  }
  # if only y maximum is supplied use from 0 to it - can work for all measures
  if (!is.na(y_limit_upper)) {
    return(
      fpl_plotly |>
        plotly::layout(
          yaxis = list(range = list(0, y_limit_upper / y_limit_scaling)))
    )
  }

  # Otherwise return base chart
  return(fpl_plotly)
}
