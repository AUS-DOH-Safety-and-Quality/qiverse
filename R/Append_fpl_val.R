#' Generate funnel plot values and output as data table
#'
#' @param numerator A numeric vector
#' @param denominator A numeric vector
#' @param establishment A vector of establishment ids
#' @param funnelcharttype A string identifying the type of funnel plot.
#' Default "PR"
#' @param multiplier A string identifying the multiplication factor. Default 1
#' @param betteris A character string, shows direction of positive change.
#' @param period_end A string of the maximum date of the funnel period for use
#'  as the identifier for funnel astronomical points
#'  type Date format: %d/%m/%Y:15/12/2022
#'
#' @return The input data with appended funnel plot values in a data table
#'
#' @family pattern detection functions
#' @export
#' @examples -
append_fpl_val <- function(
    numerator,
    denominator,
    establishment,
    funnelcharttype = "PR",
    multiplier = 1,
    betteris,
    period_end
) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  . <- group <- rr <- LCL95 <- UCL95 <- LCL99 <- UCL99 <- fpl_rr <-
    fpl_row_value <- fpl_ul99 <- fpl_astro <- fpl_ll99 <- `:=` <- NULL

  #Take the input data and generate a data table
  funnel_input <- data.table::data.table(numerator, denominator,
                                         establishment = as.character(establishment),
                                         multiplier = multiplier,
                                         funnelcharttype = funnelcharttype,
                                         betteris = betteris)

  #if the data input cannot make a funnel skip and output 0 for all values
  if (sum(denominator) == 0 | length(unique(funnel_input$establishment)) <= 1) { #nolint
    funnel_input <- data.table::data.table(
      establishment = unique(funnel_input$establishment)
    )
    return(funnel_input[, c("fpl_rr", "fpl_ll95", "fpl_ll99",
                            "fpl_ul95", "fpl_ul99", "fpl_row_value",
                            "fpl_astro") := 0])
  }

  # Create Funnel
  FunnelPlotR_version <-
    asNamespace("FunnelPlotR")$`.__NAMESPACE__.`$spec[["version"]]
  # For FunnelPlotR versions below 0.5.0
  if (FunnelPlotR_version < "0.5.0") {
    ind_funnel <- FunnelPlotR::funnel_plot(
      denominator = denominator,
      numerator = numerator,
      group = establishment,
      limit = 99,
      data_type = funnelcharttype[1],
      sr_method = "CQC",
      multiplier = multiplier[1],
      draw_adjusted = TRUE,
      label = NA,
      highlight  = NA
    ) |>
      suppressWarnings()
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
      group = group,
      limit = 99,
      data_type = funnelcharttype[1],
      sr_method = "CQC",
      multiplier = multiplier[1],
      draw_adjusted = TRUE,
      label = NA,
      highlight  = NA
    ) |>
      suppressWarnings()
  }

  #extract data from funnel object as data table
  funnel_data <- ind_funnel$plot$data |>
    data.table::as.data.table()

  # change group from factor to character
  funnel_data[, group := as.character(group)]
  # select only the columns we want to keep from funnel data
  funnel_data <- funnel_data[, .(establishment = group, fpl_rr = rr,
                                 fpl_ll95 = LCL95, fpl_ul95 = UCL95,
                                 fpl_ll99 = LCL99, fpl_ul99 = UCL99)]

  # Generate fpl row values (y axis value) from funnel data
  # add betteris column to table
  funnel_data[, `:=`(fpl_row_value = fpl_rr * multiplier[1],
                     betteris = betteris[1])]

  #Check which points are outliers
  funnel_data[betteris == "Lower" & fpl_row_value >= fpl_ul99,
              fpl_astro := period_end[1]][
                betteris == "Higher" & fpl_row_value <= fpl_ll99,
                fpl_astro := period_end[1]]
  #remove betteris field
  funnel_data[, betteris := NULL]

  return(funnel_data)
}
