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
  data_type,
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
  if (data_type == "SR") {
    fpl_data[, numerator :=
        # SR SHMI Method for Limits
        exp(qnorm(runif(fpl_data[, .N])) *
              sqrt((1 / denominator) + tau2)) *
        # Multiply by denominator to get numerator
        denominator
    ]
  } else if (data_type == "PR") {
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
  data_type = example_indicator_data[1, funnel_data_type],
  base_denominator = 10,
  tau2 = 0.2
)
funnel_2_data <- gen_fpl_data(
  seed = 412,
  input_hosp_sizes = hosp_sizes,
  indicator_name = example_indicator_data[2, indicator],
  data_type = example_indicator_data[2, funnel_data_type],
  target = 0.03,
  base_denominator = 10000,
  tau2 = 0.001
)
funnel_3_data <- gen_fpl_data(
  seed = 823,
  input_hosp_sizes = hosp_sizes,
  indicator_name = example_indicator_data[3, indicator],
  data_type = example_indicator_data[3, funnel_data_type],
  target = 0.32,
  base_denominator = 50,
  tau2 = 0.001
)
funnel_4_data <- gen_fpl_data(
  seed = 483,
  input_hosp_sizes = hosp_sizes,
  indicator_name = example_indicator_data[4, indicator],
  data_type = example_indicator_data[4, funnel_data_type],
  target = 0.00025,
  base_denominator = 10000,
  tau2 = 0.00001
)
funnel_5_data <- gen_fpl_data(
  seed = 500,
  input_hosp_sizes = hosp_sizes,
  indicator_name = example_indicator_data[5, indicator],
  data_type = example_indicator_data[5, funnel_data_type],
  target = 0.00015,
  base_denominator = 10000,
  tau2 = 0.00001
)
funnel_6_data <- gen_fpl_data(
  seed = 673,
  input_hosp_sizes = hosp_sizes,
  indicator_name = example_indicator_data[6, indicator],
  data_type = example_indicator_data[6, funnel_data_type],
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

usethis::use_data(example_funnel_data, overwrite = TRUE)
