# Initialise Libraries ####
library(data.table)
library(magrittr)
library(plotly)

options(scipen = 9)

# Plotting function ####
misc_plotly <- function(data,
                        brand_colour = "#00667B",
                        worse_colour = 'black',
                        better_colour = 'grey',
                        y_dp = 2) {
  
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
      line = list(color = color, dash=dash)
    )
  }
  
  # Copy data to load standalone dataset in memory
  data <- copy(data) %>% as.data.table()
  
  ## Fix suffix per 100
  data[SUFFIX == 'per 100', SUFFIX := ' per 100']
  
  # Create Plotly Chart
  plot_data <- plot_ly(data = data[data[,.N]:1]) %>%
    # Add z-scores as a bar chart
    plotly::add_trace(
      name = 'Z-score',
      x = ~ifelse(!is.na(UZSCORE_BETTERIS), UZSCORE_BETTERIS, 0),
      y = ~list(paste0('<b>', INDICATOR_THEME, '</b>'), INDICATOR),
      type = 'bar',
      orientation = 'h',
      marker = ~list(color = ifelse(UZSCORE_BETTERIS < 0, worse_colour, better_colour)),
      textposition = 'none',
      hoverinfo = 'text',
      ## Implement hovertext tooltip
      text = ~paste0(
        '<br><b>', INDICATOR_THEME, ': ', INDICATOR, '</b>',
        '<br><b>Z-score: </b>', formatC(UZSCORE_BETTERIS, digits = 3, format = 'f', big.mark = ','),
        '<br><b>Numerator: </b>', formatC(NUMERATOR, digits = y_dp, format = 'f', big.mark = ',', drop0trailing = TRUE),
        '<br><b>Denominator: </b>', formatC(DENOMINATOR, digits = y_dp, format = 'f', big.mark = ',', drop0trailing = TRUE),
        '<br><b>Actual Value: </b>', formatC(VALUE, digits = y_dp, format = 'f', big.mark = ',', drop0trailing = TRUE), SUFFIX,
        '<br><b>Upper 99% Limit: </b>', formatC(UCL99, digits = y_dp, format = 'f', big.mark = ',', drop0trailing = TRUE), SUFFIX,
        '<br><b>Centerline: </b>', formatC(CL, digits = y_dp, format = 'f', big.mark = ',', drop0trailing = TRUE), SUFFIX,
        '<br><b>Lower 99% Limit: </b>', formatC(LCL99, digits = y_dp, format = 'f', big.mark = ',', drop0trailing = TRUE), SUFFIX
      ),
      hoverlabel = list(bgcolor = brand_colour),
      showlegend = FALSE
    ) %>%
    # Add 95% control limits to legend only
    plotly::add_trace(
      name = '95% Control Limits',
      x = NULL,
      y = 0,
      type = 'scatter',
      mode = 'lines',
      line = list(color = brand_colour, dash = "dot"),
      showlegend = TRUE
    ) %>%
    # Add 99% control limits to legend only
    plotly::add_trace(
      name = '99% Control Limits',
      x = NULL,
      y = 0,
      type = 'scatter',
      mode = 'lines',
      line = list(color = brand_colour, dash = "dash"),
      showlegend = TRUE
    ) %>%
    # Format layout options
    plotly::layout(
      ## Set font options
      font = list(family = 'Arial', color = 'black'),
      ## Create Chart Title
      title = list(
        font = list(size = 20),
        text = paste0('<b>Multiple Indicator Sigma Chart for ', data[1,GROUP], '</b>')
      ),
      ## Set x-axis options
      xaxis = list(title = '', range = list(-5, 5), dtick = 1),
      ## Set y-axis options
      yaxis = list(title = '', categoryorder = 'trace'),
      ## Set hovermode of cursor
      hovermode = 'closest',
      ## Set margin of chart (important for captions, and title/legend spacing)
      margin = list(b = 80, t = 80),
      ## Add annotation text
      annotations = list(
        ## Add source caption in bottom right
        list(
          x = 1, y = -0.05, text = "<i>Source: Healthcare Quality Intelligence Unit</i>",
          showarrow = F, xref = 'paper', yref = 'paper',
          xanchor = 'right', yanchor = 'top', xshift = 0, yshift = 0,
          font = list(size = 15)
        ),
        ## Add Worse caption for LHS
        list(x = 0.05, y = 1, text = '<i><b>Unfavourable</b></i>',
             showarrow = F, xref = 'paper', yref = 'paper',
             xanchor = 'left', yanchor = 'bottom', xshift = 0, yshift = 0,
             font = list(size = 15, color = worse_colour)),
        ## Add Better caption for RHS
        list(x = 0.95, y = 1, text = '<i><b>Favourable</b></i>',
             showarrow = F, xref = 'paper', yref = 'paper',
             xanchor = 'right', yanchor = 'bottom', xshift = 0, yshift = 0,
             font = list(size = 15, color = better_colour))
      ),
      ## Add vertical lines for control limits
      shapes = list(
        ## 99.9% Lower Control Limit
        vline(qnorm(0.001), color = brand_colour, dash = "dash"),
        ## 95% Lower Control Limit
        vline(qnorm(0.025), color = brand_colour, dash = "dot"),
        ## 95% Upper Control Limit
        vline(qnorm(0.975), color = brand_colour, dash = "dot"),
        ## 99.9% Upper Control Limit
        vline(qnorm(0.999), color = brand_colour, dash = "dash")
      ),
      ## Set legend options
      legend = list(
        orientation = 'h',
        x = 0.5,
        y = 1,
        xanchor = 'center',
        yanchor = 'bottom',
        itemclick = FALSE
      )
    ) %>%
    # Remove modebar buttons to simplify options
    plotly::config(modeBarButtonsToRemove = list('select', 'lasso', 'zoomIn', 'zoomOut', 'hoverClosestCartesian', 'hoverCompareCartesian', 'zoom', 'pan'), displaylogo = FALSE)
  
  # Return
  return(plot_data)
}