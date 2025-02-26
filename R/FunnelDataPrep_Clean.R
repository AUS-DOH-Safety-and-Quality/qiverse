# Initialise Libraries ####
library(data.table)
library(magrittr)
library(stringr)
library(qiverse)
library(plotly)

options(scipen = 9)

# Extract data from PowerBI Dataflows ####
## Grab the authentication token for azure to connect to powerbi dataflows
tk <- get_az_tk('pbi_df')

## Squis core wide is the main data source that is used to calculate plots
squis_core_wide <- download_dataflow_table(workspace_name = "DOH - SQuIS [Test]",
                                           dataflow_name = "squis_ultimate",
                                           table_name = "squis_core_wide",
                                           access_token = tk$credentials$access_token) |>
  as.data.table() |>
  set_attr('spec', NULL)
squis_core_wide[, funnel_period_start := as.Date(funnel_period_start, format = '%d/%m/%Y')]
squis_core_wide[, funnel_period_end := as.Date(funnel_period_end, format = '%d/%m/%Y')]
squis_core_wide[, period_start := as.Date(period_start, format = '%d/%m/%Y')]
squis_core_wide[, period_end := as.Date(period_end, format = '%d/%m/%Y')]

## Get indicator list for qsgsortorder
indicator_list <- download_dataflow_table(workspace_name = "DOH - SQuIS [Test]",
                                          dataflow_name = "squis_ultimate",
                                          table_name = "indicator_list",
                                          access_token = tk$credentials$access_token) |>
  as.data.table() |>
  set_attr('spec', NULL)

# Filter, group and summarised data to be relevant to funnel period and exclusions ####
scw_filt <- squis_core_wide %>%
  ## Only include dates within funnel period
  .[period_start >= funnel_period_start & period_end <= funnel_period_end] %>%
  ## Remove any establishment codes from exclusion funnels
  .[!(str_detect(exclusions_funnel, establishment)) | is.na(exclusions_funnel)] %>%
  ## Summate numerator and denominator
  .[, .(numerator = sum(numerator), denominator = sum(denominator)),
    keyby = .(indicator_theme = indicatorgroupvalue, indicator = descriptionshort, group = shorthospitalname, multiplier, data_type = funnelcharttype, betteris)]

# Set up data for import ####
funnel_data <- scw_filt[denominator != 0, .(indicator, group, numerator, denominator)]

indicator_data <- indicator_list[liveinsquis == TRUE] %>%
  setorder(qsgsortorder) %>%
  .[, .(indicator_theme = indicatorgroupvalue, indicator = descriptionshort, multiplier, data_type = funnelcharttype, betteris)]

# Format Data ####
misc_data <- function(funnel_data, indicator_data) {
  # Extract funnel data for all unique indicators ####
  ## Merge the data into a data.table
  data <- merge(funnel_data, indicator_data, by = "indicator", all.x = TRUE) %>%
    as.data.table()
  ## Loop through each unique indicator to generate funnel limits and Z-scores
  data.funnel <- lapply(indicator_data$indicator, function(ind) {
    print(ind)
    # Create subset of data based on chosen indicator
    data.ind <- data[indicator == ind]

    # Do not generate funnel data for indicator with only one group
    if (data.ind[, .N] > 1) {
      # Generate funnel plot data
      funnel <- FunnelPlotR::funnel_plot(
        denominator=data.ind$denominator,
        numerator=data.ind$numerator,
        group = data.ind$group,
        limit=99,
        data_type = data.ind[1, data_type],
        sr_method = "CQC",
        multiplier = data.ind[1, multiplier],
        draw_unadjusted = TRUE,
        draw_adjusted = TRUE,
        label = NA,
        highlight  = NA
      )

      # Extract out values , Uzscore, control limits
      funnel_data <- funnel$aggregated_data[, c('group', 'rr', 'Uzscore', 'LCL95', 'UCL95', 'LCL99', 'UCL99')] %>%
        as.data.table %>%
        .[, group := as.character(group)] %>%
        setnames('rr', 'value')

      # Merge back in funnel data
      data.ind <- merge(data.ind, funnel_data, by = 'group', all.x = TRUE, sort = FALSE)

      # Find centreline
      data.ind[, cl := sum(numerator)/sum(denominator) * multiplier]

      # Output
      data.ind
    }
  }) %>%
    # Stack list of data.tables into single table
    rbindlist

  # Apply a series of transformation steps to transform data directly into plotly function formats ####
  ## Apply multiplier to value, to make consistency between value and limits
  data.funnel[, value := value*multiplier]

  ## Apply betteris to Uzscore for consistency in reporting
  data.funnel[betteris == 'Lower', Uzscore_betteris := Uzscore * (-1)]
  data.funnel[betteris == 'Higher', Uzscore_betteris := Uzscore * (1)]

  ## Set suffix for hovertext label
  ### Set percentage
  data.funnel[data_type == 'PR' & multiplier == 1, suffix := '%']
  ### Set rates per x
  data.funnel[data_type == 'PR' & multiplier != 1, suffix := paste0(' per ', formatC(multiplier, digits = 0, format = 'f', big.mark = ','))]
  ### Set blank for others
  data.funnel[is.na(suffix), suffix := '']

  ## Multiply by 100 for those that are proportions and multiplier 1
  data.funnel[data_type == 'PR' & multiplier == 1,
              ':=' (value = value * 100,
                    UCL99 = UCL99 * 100,
                    cl = cl * 100,
                    LCL99 = LCL99 * 100)]

  # Change names of columns to uppercase
  setnames(data.funnel, names(data.funnel), toupper(names(data.funnel)))

  # Return output ####
  return(data.funnel)
}

## Write Output
data <- misc_data(funnel_data, indicator_data)

##fwrite(data, 'Outputs/msc_data.csv')
