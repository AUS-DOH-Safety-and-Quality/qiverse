#' Chart the Multiple Indicator Sigma Chart in Plotly
#'
#' @param data A data.frame/data.table of the funnel data with the
#' columns indicator, group, numerator and denominator. See ?example_funnel_data
#' for an example and more information on the fields required.
#' @param hover_colour The background colour for the hoverinfo (default =
#' "#00667B")
#' @param control_colour The colour of the 95% and 99% control limits for the plot
#' (default = "#00667B")
#' @param worse_colour The colour of the bar in the unfavourable direction
#' (default = "#E46C0A")
#' @param neutral_colour The colour of the bar in the favourable direction
#' (default = "#A6A6A6")
#' @param better_colour The colour of the bar in the favourable direction
#' (default = "#00B0F0")
#' @param y_dp The number of decimal places displayed on the chart (default = 2)
#'
#' @return A MISC plotly output.
#'
#' @export
#' @examples -
#' \dontrun{
#'   library(qiverse.qimisc)
#'   library(qiverse.data)
#'   misc_data <- misc_prep_data(example_funnel_data, example_indicator_data)
#'   misc_plotly(misc_data[group == "A"])
#' }

# Plotting function ####
misc_plotly <- function(data,
                        hover_colour = "#00667B",
                        control_colour = "#00667B",
                        worse_colour = "#E46C0A",
                        neutral_colour = "#A6A6A6",
                        better_colour = "#00B0F0",
                        y_dp = 2) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  suffix <- .N <- group <- `:=` <- value <- numerator <- denominator <- #nolint
    data_type <- multiplier <- betteris <- uzscore_betteris <- uzscore <-
    hovertext <- indicator_theme <- indicator <- ucl99 <- cl <- lcl99 <- NULL

  # Copy data to load standalone dataset in memory ####
  data <- data.table::copy(data) |>
    data.table::as.data.table()

  # Add columns for plotting ####
  ## Calculate value and apply multiplier to value, to make consistency between
  ## value and limits
  data[, value := numerator / denominator *
         ifelse(data_type == "PR" & multiplier == 1, 100, multiplier)]

  ## Apply betteris to Uzscore for consistency in reporting
  data[betteris == "Lower", uzscore_betteris := uzscore * (-1)]
  data[betteris == "Higher", uzscore_betteris := uzscore * (1)]

  ## Set suffix for hovertext label
  ### Set percentage
  data[data_type == "PR" & multiplier == 1, suffix := "%"]
  ### Set rates per x
  data[data_type == "PR" & multiplier != 1,
       suffix := paste0(" per ", formatC(multiplier, digits = 0,
                                         format = "f", big.mark = ","))]
  ### Set blank for others
  data[is.na(suffix), suffix := ""]

  #see my comment on the hovertext below
  data[, hovertext := paste0(
    "<br><b>", indicator_theme, ": ", indicator, "</b>",
    "<br><b>Z-score: </b>", formatC(uzscore_betteris, digits = 3,
                                    format = "f", big.mark = ","),
    "<br><b>Numerator: </b>", formatC(numerator, digits = y_dp,
                                      format = "f", big.mark = ",",
                                      drop0trailing = TRUE),
    "<br><b>Denominator: </b>", formatC(denominator, digits = y_dp,
                                        format = "f", big.mark = ",",
                                        drop0trailing = TRUE),
    "<br><b>Actual Value: </b>", formatC(value, digits = y_dp,
                                         format = "f", big.mark = ",",
                                         drop0trailing = TRUE), suffix,
    "<br><b>Upper 99% Limit: </b>", formatC(ucl99, digits = y_dp,
                                            format = "f", big.mark = ",",
                                            drop0trailing = TRUE), suffix,
    "<br><b>Centerline: </b>", formatC(cl, digits = y_dp, format = "f",
                                       big.mark = ",",
                                       drop0trailing = TRUE), suffix,
    "<br><b>Lower 99% Limit: </b>", formatC(lcl99, digits = y_dp,
                                            format = "f", big.mark = ",",
                                            drop0trailing = TRUE), suffix
  )]

  # Plotly Function ####
  # Helper Function to Create vertical line
  vline <- function(x = 0, color = "#00667B", dash = "dot") {
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = x,
      x1 = x,
      line = list(color = color, dash = dash)
    )
  }

  # Create Plotly Chart
  plot_data <- plotly::plot_ly(data = data[data[, .N]:1]) |>
    # Add z-scores as a bar chart
    plotly::add_trace(
      name = "Z-score",
      x = ~ifelse(!is.na(uzscore_betteris), uzscore_betteris, 0),
      y = ~list(paste0("<b>", indicator_theme, "</b>"), indicator),
      type = "bar",
      orientation = "h",
      marker = ~list(color =
                       ifelse(
                         uzscore_betteris < stats::qnorm(0.001),
                         worse_colour,
                         ifelse(
                           uzscore_betteris > stats::qnorm(0.999),
                           better_colour,
                           neutral_colour
                         )
                       )
      ),
      textposition = "none",
      hoverinfo = "text",
      text = ~hovertext,
      hoverlabel = list(bgcolor = hover_colour),
      showlegend = FALSE
    ) |>
    # Add 95% control limits to legend only
    plotly::add_trace(
      name = "95% Control Limits",
      x = NULL,
      y = 0,
      type = "scatter",
      mode = "lines",
      line = list(color = control_colour, dash = "dot"),
      showlegend = TRUE
    ) |>
    # Add 99% control limits to legend only
    plotly::add_trace(
      name = "99% Control Limits",
      x = NULL,
      y = 0,
      type = "scatter",
      mode = "lines",
      line = list(color = control_colour, dash = "dash"),
      showlegend = TRUE
    ) |>
    # Format layout options
    plotly::layout(
      ## Set font options
      font = list(family = "Arial", color = "black"),
      ## Create Chart Title
      title = list(
        font = list(size = 20),
        text = paste0("<b>Multiple Indicator Sigma Chart for ", data[1, group],
                      "</b>")
      ),
      ## Set x-axis options
      xaxis = list(title = "", range = list(-5, 5), dtick = 1),
      ## Set y-axis options
      yaxis = list(title = "", categoryorder = "trace"),
      ## Set hovermode of cursor
      hovermode = "closest",
      ## Set margin of chart (important for captions, and title/legend spacing)
      margin = list(b = 80, t = 80),
      ## Add annotation text
      annotations = list(
        ## Add source caption in bottom right
        list(
          x = 1, y = -0.05,
          text = "<i>Source: Healthcare Quality Intelligence Unit</i>",
          showarrow = FALSE, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "top", xshift = 0, yshift = 0,
          font = list(size = 15)
        ),
        ## Add Worse caption for LHS
        list(x = 0.05, y = 1, text = "<i><b>Unfavourable</b></i>",
             showarrow = FALSE, xref = "paper", yref = "paper",
             xanchor = "left", yanchor = "bottom", xshift = 0, yshift = 0,
             font = list(size = 15, color = worse_colour)),
        ## Add Better caption for RHS
        list(x = 0.95, y = 1, text = "<i><b>Favourable</b></i>",
             showarrow = FALSE, xref = "paper", yref = "paper",
             xanchor = "right", yanchor = "bottom", xshift = 0, yshift = 0,
             font = list(size = 15, color = better_colour))
      ),
      ## Add vertical lines for control limits
      shapes = list(
        ## 99.9% Lower Control Limit
        vline(stats::qnorm(0.001), color = control_colour, dash = "dash"),
        ## 95% Lower Control Limit
        vline(stats::qnorm(0.025), color = control_colour, dash = "dot"),
        ## 95% Upper Control Limit
        vline(stats::qnorm(0.975), color = control_colour, dash = "dot"),
        ## 99.9% Upper Control Limit
        vline(stats::qnorm(0.999), color = control_colour, dash = "dash")
      ),
      ## Set legend options
      legend = list(
        orientation = "h",
        x = 0.5,
        y = 1,
        xanchor = "center",
        yanchor = "bottom",
        itemclick = FALSE
      )
    ) |>
    # Remove modebar buttons to simplify options
    plotly::config(modeBarButtonsToRemove = list(
      "select", "lasso", "zoomIn", "zoomOut", "hoverClosestCartesian",
      "hoverCompareCartesian", "zoom", "pan"
    ), displaylogo = FALSE)

  # Return
  return(plot_data)
}
