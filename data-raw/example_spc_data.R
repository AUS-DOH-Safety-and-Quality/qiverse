# Indicator Metadata ####
library(data.table)

indicator_1_data <- data.table(
  indicator_theme = "Mortality",
  indicator = "Standardised Mortality Ratio",
  multiplier = 100,
  funnel_data_type = "SR",
  spc_data_type = "i",
  betteris = "Lower"
)
indicator_2_data <- data.table(
  indicator_theme = "Mortality",
  indicator = "Crude Mortality",
  multiplier = 10000,
  funnel_data_type = "PR",
  spc_data_type = "p",
  betteris = "Lower"
)
indicator_3_data <- data.table(
  indicator_theme = "Maternity",
  indicator = "Caesarean Section",
  multiplier = 100,
  funnel_data_type = "PR",
  spc_data_type = "p",
  betteris = "Lower"
)
indicator_4_data <- data.table(
  indicator_theme = "Hospital Acquired Complications",
  indicator = "Pressure Injury",
  multiplier = 1000,
  funnel_data_type = "PR",
  spc_data_type = "p",
  betteris = "Lower"
)
indicator_5_data <- data.table(
  indicator_theme = "Hospital Acquired Complications",
  indicator = "Falls",
  multiplier = 1000,
  funnel_data_type = "PR",
  spc_data_type = "p",
  betteris = "Lower"
)
indicator_6_data <- data.table(
  indicator_theme = "Staff Experience",
  indicator = "Staff Feel Safe To Speak Up",
  multiplier = 1,
  funnel_data_type = "PR",
  spc_data_type = "p",
  betteris = "Higher"
)
example_indicator_data <- rbind(
  indicator_1_data,
  indicator_2_data,
  indicator_3_data,
  indicator_4_data,
  indicator_5_data,
  indicator_6_data
)

# Funnel Data ####
library(data.table)
set.seed(7287)
## Set Hospital Sizes
### Hospitals B, C and F are larger
large_hosp_multiplier <- data.table(
  group = c("B", "C", "F"),
  hosp_multiplier = runif(3, min = 40, max = 50)
)

### Hospitals A, D and I are medium sized
medium_hosp_multiplier <- data.table(
  group = c("A", "D", "I"),
  hosp_multiplier = runif(3, min = 10, max = 20)
)

### Hospitals E, G, H, J and L are smaller
small_hosp_multiplier <- data.table(
  group = c("E", "G", "H", "J", "L"),
  hosp_multiplier = runif(5, min = 1, max = 5)
)

### Combine sizes
hosp_sizes <- rbind(
  large_hosp_multiplier,
  medium_hosp_multiplier,
  small_hosp_multiplier
) |>
  setorder(group)

## Generate Funnel Plot Data
gen_fpl_data <- function(
    seed,
    input_hosp_sizes,
    indicator_name,
    funnel_data_type,
    target = 1,
    base_denominator,
    tau2
) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  indicator <- denominator <- hosp_multiplier <- numerator <- NULL

  # Set seed
  set.seed(seed)

  # Copy data for usage
  fpl_data <- copy(input_hosp_sizes)

  # Calculate Numerator and Denominator
  fpl_data[, indicator := indicator_name]
  fpl_data[, denominator := base_denominator * hosp_multiplier]
  if (funnel_data_type == "SR") {
    fpl_data[, numerator :=
               # SR SHMI Method for Limits
               exp(qnorm(runif(fpl_data[, .N])) *
                     sqrt((1 / denominator) + tau2)) *
               # Multiply by denominator to get numerator
               denominator
    ]
  } else if (funnel_data_type == "PR") {
    fpl_data[, denominator := round(denominator, 0)]
    fpl_data[, numerator :=
               # PR Method for Limits
               sin(asin(sqrt(target)) +
                     qnorm(runif(fpl_data[, .N])) *
                     sqrt((1 / (2 * sqrt(denominator)))^2 + tau2))^2 *
               # Multiply by denominator to get numerator
               denominator
    ]
    fpl_data[, numerator := round(numerator, 0)]
  }
  fpl_data
}

funnel_1_data <- gen_fpl_data(
  seed = 123,
  input_hosp_sizes = hosp_sizes,
  indicator_name = example_indicator_data[1, indicator],
  funnel_data_type = example_indicator_data[1, funnel_data_type],
  base_denominator = 10,
  tau2 = 0.2
)
funnel_1_data[, numerator := round(numerator)]
funnel_2_data <- gen_fpl_data(
  seed = 412,
  input_hosp_sizes = hosp_sizes,
  indicator_name = example_indicator_data[2, indicator],
  funnel_data_type = example_indicator_data[2, funnel_data_type],
  target = 0.03,
  base_denominator = 10000,
  tau2 = 0.001
)
funnel_3_data <- gen_fpl_data(
  seed = 823,
  input_hosp_sizes = hosp_sizes,
  indicator_name = example_indicator_data[3, indicator],
  funnel_data_type = example_indicator_data[3, funnel_data_type],
  target = 0.32,
  base_denominator = 50,
  tau2 = 0.001
)
funnel_4_data <- gen_fpl_data(
  seed = 483,
  input_hosp_sizes = hosp_sizes,
  indicator_name = example_indicator_data[4, indicator],
  funnel_data_type = example_indicator_data[4, funnel_data_type],
  target = 0.00025,
  base_denominator = 10000,
  tau2 = 0.00001
)
funnel_5_data <- gen_fpl_data(
  seed = 500,
  input_hosp_sizes = hosp_sizes,
  indicator_name = example_indicator_data[5, indicator],
  funnel_data_type = example_indicator_data[5, funnel_data_type],
  target = 0.00015,
  base_denominator = 10000,
  tau2 = 0.00001
)
funnel_6_data <- gen_fpl_data(
  seed = 673,
  input_hosp_sizes = hosp_sizes,
  indicator_name = example_indicator_data[6, indicator],
  funnel_data_type = example_indicator_data[6, funnel_data_type],
  target = 0.5,
  base_denominator = 50,
  tau2 = 0.004
)
example_funnel_data <- rbind(
  funnel_1_data,
  funnel_2_data,
  funnel_3_data,
  funnel_4_data,
  funnel_5_data,
  funnel_6_data
)[, .(indicator, group, numerator, denominator)]

# Generate SPC data
gen_spc_data <- function(
  seed,
  indicator_name,
  group_name,
  fpl_numerator,
  fpl_denominator
) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  indicator <- denominator <- hosp_multiplier <- numerator <- NULL

  # Set seed
  set.seed(seed)

  # Establish period start date
  period_start <- seq.Date(
    from = as.Date('2021-01-01'),
    by = 'month',
    length.out = 24
  )
  # Establish period end date
  period_end <- sapply(
    period_start,
    function(x) seq.Date(x, by = 'month', length.out = 2)[2] - 1
  ) |>
    as.Date()

  # Determine spc_data_type
  spc_data_type <- example_indicator_data[indicator == indicator_name, spc_data_type]

  # calculate centreline
  centreline <- fpl_numerator/fpl_denominator

  if (spc_data_type == "i") {
    # set numerator for 24 months by distributing it using Poisson
    spc_numerator <- rpois(24, fpl_numerator/24)
    # set denominator for 24 months by distributing it using normal distribution
    spc_denominator <- rnorm(24, fpl_denominator/24, 1)
  } else if (spc_data_type == "p") {
    # set denominator for 24 months by distributing it using Poisson
    spc_denominator <- rpois(24, fpl_denominator/24)
    # Set numerator by using a binomial distribution
    spc_numerator <- sapply(spc_denominator, function(x) rbinom(1, x, centreline))
  }

  # Output table with required fields for SPC Funnel Matrix
  spc_data <- data.table(
    indicator = indicator_name,
    group = group_name,
    period_end = period_end,
    period_start = period_start,
    numerator = spc_numerator,
    denominator = spc_denominator,
    multiplier = example_indicator_data[indicator == indicator_name, multiplier],
    better_is = example_indicator_data[indicator == indicator_name, betteris],
    spc_chart_type = spc_data_type,
    funnel_chart_type = example_indicator_data[indicator == indicator_name, funnel_data_type]
  )

  return(spc_data)
}

example_spc_data <- lapply(1:example_funnel_data[,.N], function(i) {
  gen_spc_data(
    seed = 2000 + i,
    indicator = example_funnel_data[i, indicator],
    group = example_funnel_data[i, group],
    fpl_numerator = example_funnel_data[i, numerator],
    fpl_denominator = example_funnel_data[i, denominator]
  )
}) |>
  rbindlist()

usethis::use_data(example_spc_data, overwrite = TRUE)
