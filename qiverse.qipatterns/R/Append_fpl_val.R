#' Generate funnel plot values and output as data table
#'
#' @param numerator A numeric vector
#' @param denominator A numeric vector
#' @param group A vector of group ids
#' @param funnel_chart_type A string identifying the type of funnel plot.
#' Default "PR"
#' @param multiplier A string identifying the multiplication factor. Default 1
#' @param better_is A character string, shows direction of positive change.
#' @param overdispersion A boolean flag for whether to adjust for overdispersion
#' @param period_end A string of the maximum date of the funnel period for use
#'  as the identifier for funnel astronomical points
#'  type Date format: %d/%m/%Y:15/12/2022
#'
#' @return The input data with appended funnel plot values in a data table
#'
#' @family pattern detection functions
#' @export
append_fpl_val <- function(
    numerator,
    denominator,
    group,
    funnel_chart_type = "PR",
    multiplier = 1,
    better_is,
    overdispersion = FALSE,
    period_end
) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  . <- rr <- LCL95 <- UCL95 <- LCL99 <- UCL99 <- OD99LCL <- OD99UCL <- s <- ODUzscore  <- fpl_rr <-
    fpl_row_value <- fpl_ul99 <- fpl_astro <- fpl_ll99 <- Uzscore <- tau2 <- `:=` <- NULL

  #Take the input data and generate a data table
  funnel_input <- data.table::data.table(numerator, denominator,
                                         group = as.character(group),
                                         multiplier = multiplier,
                                         funnel_chart_type = funnel_chart_type,
                                         better_is = better_is,
                                         overdispersion = overdispersion)

  # Create Funnel
  FunnelPlotR_version <-
    asNamespace("FunnelPlotR")$`.__NAMESPACE__.`$spec[["version"]]
  # For FunnelPlotR versions below 0.5.0
  if (FunnelPlotR_version < "0.5.0") {
    ind_funnel <- FunnelPlotR::funnel_plot(
      denominator = denominator,
      numerator = numerator,
      group = group,
      limit = 99,
      data_type = funnel_chart_type[1],
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
    message("data_type: ", funnel_chart_type)
    ind_funnel <- FunnelPlotR::funnel_plot(
      .data = data.frame(
        denominator = denominator,
        numerator = numerator,
        group = group
      ),
      denominator = denominator,
      numerator = numerator,
      group = group,
      limit = 99,
      data_type = funnel_chart_type[1],
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
 funnel_data$tau2 <- ind_funnel$tau2

  # Apply overdispersion
  # calculating Z score for over dispersion
  # brought it from Power BI visual for funnel chart since it need to be align to it (by Andrew Johnson)
  funnel_data[, ODUzscore := (Uzscore * s) / sqrt(s^2 + tau2)]

  #overwrite the values when over dispersion flag is true
  if(overdispersion == TRUE){
    funnel_data[,
                `:=` (UCL99 = OD99UCL,
                      LCL99 = OD99LCL,
                      Uzscore = ODUzscore)]
  }


 # change group from factor to character
  funnel_data[, group := as.character(group)]
  # select only the columns we want to keep from funnel data
  funnel_data <- funnel_data[, .(group = group, fpl_rr = rr,
                                 fpl_ll95 = LCL95, fpl_ul95 = UCL95,
                                 fpl_ll99 = LCL99, fpl_ul99 = UCL99)]

  # Generate fpl row values (y axis value) from funnel data
  # add better_is column to table
  funnel_data[, `:=`(fpl_row_value = fpl_rr * multiplier[1],
                     better_is = better_is[1])]

  #Check which points are outliers
  funnel_data[better_is == "Lower" & fpl_row_value > fpl_ul99,
              fpl_astro := period_end[1]][
                better_is == "Higher" & fpl_row_value < fpl_ll99,
                fpl_astro := period_end[1]]
  #remove better_is field
  funnel_data[, better_is := NULL]

  return(funnel_data)
}
