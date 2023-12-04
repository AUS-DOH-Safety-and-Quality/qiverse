# Indicator Metadata ####
library(data.table)

indicator_1_data <- data.table(
  indicator_theme = "Mortality",
  indicator = "Standardised Mortality Ratio",
  multiplier = 100,
  data_type = "SR",
  betteris = "Lower"
)
indicator_2_data <- data.table(
  indicator_theme = "Mortality",
  indicator = "Crude Mortality",
  multiplier = 10000,
  data_type = "PR",
  betteris = "Lower"
)
indicator_3_data <- data.table(
  indicator_theme = "Maternity",
  indicator = "Caesarean Section",
  multiplier = 100,
  data_type = "PR",
  betteris = "Lower"
)
indicator_4_data <- data.table(
  indicator_theme = "Hospital Acquired Complications",
  indicator = "Pressure Injury",
  multiplier = 1000,
  data_type = "PR",
  betteris = "Lower"
)
indicator_5_data <- data.table(
  indicator_theme = "Hospital Acquired Complications",
  indicator = "Falls",
  multiplier = 1000,
  data_type = "PR",
  betteris = "Lower"
)
indicator_6_data <- data.table(
  indicator_theme = "Staff Experience",
  indicator = "Staff Feel Safe To Speak Up",
  multiplier = 100,
  data_type = "PR",
  betteris = "Higher"
)
indicator_data <- rbind(
  indicator_1_data,
  indicator_2_data,
  indicator_3_data,
  indicator_4_data,
  indicator_5_data,
  indicator_6_data
)

usethis::use_data(indicator_data, overwrite = TRUE)
