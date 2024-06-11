#' Construct Flextable from SPC Funnel Matrix Data
#'
#' Construct a Flextable from the SPC Funnel Matrix Data
#'
#' @param data A data.table output from the spc_funnel_matrix_prep_data
#' function
#' @param filter_indicatorgroup A vector of indicatorgroup to filter the data by. Data
#' will not be filtered by default (NA)
#' @param filter_indicator A vector of indicator to filter the data by. Data
#' will not be filtered by default (NA)
#' @param filter_parent_group A vector of parent_group to filter the data by. Data
#' will not be filtered by default (NA)
#' @param filter_hospital A vector of hospital to filter the data by. Data
#' will not be filtered by default (NA)
#'
#' @return A flextable visualisation for the SPC Funnel Matrix.
#'
#' @import data.table
#'
#' @export
#' @examples -
#' \dontrun{
#'   library(qiverse.qimatrix)
#'   spc_funnel_matrix_data <- spc_funnel_matrix_prep_data(
#'     indicator,
#'     establishment,
#'     period_end,
#'     period_start,
#'     numerator,
#'     denominator,
#'     multiplier,
#'     betteris,
#'     parent_group_name,
#'     indicatorgroup,
#'     spccharttype = "p",
#'     funnelcharttype = "PR",
#'     descriptionshort = indicator,
#'     shorthospitalname = establishment,
#'     funneldatapoints = "Yes"
#'   )
#'   spc_funnel_matrix_flextable(data = spc_funnel_matrix_data)
#' }

spc_funnel_matrix_flextable <- function(
    data,
    filter_indicatorgroup = NULL,
    filter_indicator = NULL,
    filter_parent_group = NULL,
    filter_hospital = NULL
) {

  # Dealing with undefined global functions or variables
  . <- position <- spc_flag <- fpl_flag <- spc_pattern <- unique_id <-
    patterns <- indicator <- hospital <- num <- indicator_group <-
    parent_group <- NULL

  # Load the data
  spc_funnel_matrix <- data$spc_funnel_matrix
  spc_patterns_long <- data$spc_patterns_long

  # Apply optional pre-filtering
  ## Filter by indicatorgroup
  if (length(filter_indicatorgroup) > 0) {
    spc_funnel_matrix <- spc_funnel_matrix[indicatorgroup %in% filter_indicatorgroup]
    spc_patterns_long <-
      spc_patterns_long[indicator %in% spc_funnel_matrix[, unique(indicator)]]
  }

  ## Filter by indicator
  if (length(filter_indicator) > 0) {
    spc_funnel_matrix <- spc_funnel_matrix[indicator %in% filter_indicator]
    spc_patterns_long <-
      spc_patterns_long[indicator %in% spc_funnel_matrix[, unique(indicator)]]
  }

  ## Filter by parent_group
  if (length(filter_parent_group) > 0) {
    spc_funnel_matrix <- spc_funnel_matrix[parent_group %in% filter_parent_group]
    spc_patterns_long <-
      spc_patterns_long[hospital %in% spc_funnel_matrix[, unique(hospital)]]
  }

  ## Filter by hospital
  if (length(filter_hospital) > 0) {
    spc_funnel_matrix <- spc_funnel_matrix[hospital %in% filter_hospital]
    spc_patterns_long <-
      spc_patterns_long[hospital %in% spc_funnel_matrix[, unique(hospital)]]
  }

  # Case statement to identify where on the 3x3 grid each indicator lies
  spc_funnel_matrix[,position := fcase(
    spc_flag == "Favourable SPC Patterns" & fpl_flag == "Favourable Funnel Plot Outlier", 1,
    spc_flag == "Favourable SPC Patterns" & fpl_flag == "Neutral", 2,
    spc_flag == "Favourable SPC Patterns" & fpl_flag == "Unfavourable Funnel Plot Outlier", 3,
    spc_flag == "Neutral" & fpl_flag == "Favourable Funnel Plot Outlier", 4,
    spc_flag == "Neutral" & fpl_flag == "Neutral", 5,
    spc_flag == "Neutral" & fpl_flag == "Unfavourable Funnel Plot Outlier", 6,
    spc_flag == "Unfavourable SPC Patterns" & fpl_flag == "Favourable Funnel Plot Outlier", 7,
    spc_flag == "Unfavourable SPC Patterns" & fpl_flag == "Neutral", 8,
    spc_flag == "Unfavourable SPC Patterns" & fpl_flag == "Unfavourable Funnel Plot Outlier", 9
  )]

  # Collapse long patterns to each unique_id
  spc_patterns_long[, spc_pattern_utf8 :=
                      sapply(spc_patterns_long$spc_pattern,
                             function(x) intToUtf8(x))]
  spc_patterns_long_collapsed <-
    spc_patterns_long[,
                      .(patterns = paste0(spc_pattern_utf8, collapse = ",")),
                      by = unique_id]
  spc_patterns_long_collapsed[, patterns := paste0("(", patterns, ")")]

  # Sort groups so they appear alphabetically
  data.table::setorder(spc_funnel_matrix, indicator)
  # Group all of the indicators in one cell into a single vector
  matrix <- copy(spc_funnel_matrix) |>
    # Merge in Patterns
    merge(spc_patterns_long_collapsed, by = "unique_id",
          all.x = TRUE, sort = FALSE) |>
    # Collapse by position
    _[, paste(indicator, "-", hospital,
               fifelse(!is.na(patterns), patterns, ""),
               collapse = "\n"), by = position]

  # Fill in the potential missing cells with na
  count <- data.table::data.table(num = 1:9)
  matrix <- matrix[count, on = .(position = num)]
  # Replace the middle cell with the count of values as it is sometimes large
  matrix$V1[5] <- paste(
    "There are",
    spc_funnel_matrix[spc_flag == "Neutral" & fpl_flag == "Neutral", .N],
    "indicator hospital combinations that have not displayed any SPC patterns",
    "or funnel plot outliers in the last twelve months")
  # Create a vector of row names
  colZero <- data.table::data.table(
    " " = c("\nImproving\n(Favourable SPC Patterns)",
            "\nStaying the Same\n(No SPC Patterns)",
            "\nDeteriorating\n(Unfavourable SPC Patterns")
  )
  # Pull out each column individually from the main table
  colOne <- matrix[position %in% c(1,4,7)][,position := NULL]
  # Name the columns
  data.table::setnames(colOne, "V1",
                       "Better than Expected\n(Favourable Funnel Plot Outlier)")
  colTwo <- matrix[position %in% c(2,5,8)][,position := NULL]
  data.table::setnames(colTwo, "V1",
                       "Within Expectations\n(Within Funnel Limits)")
  colThree <- matrix[position %in% c(3,6,9)][,position := NULL]
  data.table::setnames(colThree, "V1",
                       "Worse than Expected\n(Unfavourable Funnel Plot Outlier")
  #Bind the columns together
  matrix <- cbind(colZero, colOne, colTwo, colThree)
  blankRow <- data.table::data.table(
    " " = " ",
    "Better than Expected\n(Favourable Funnel Plot Outlier)" = " ",
    "Within Expectations\n(Within Funnel Limits)" = " ",
    "Worse than Expected\n(Unfavourable Funnel Plot Outlier" = " "
  )
  matrix <- rbind(blankRow, matrix)

  # Format the table
  matrix |>
    flextable::flextable() |>
    flextable::border_remove() |>
    flextable::set_table_properties(layout = "autofit") |>
    flextable::set_table_properties(width = 1) |>
    flextable::width(j = 2:4, width = 4) |>
    # Set background colours
    flextable::bg(i=2,j=2, bg = "#5A7AB5", part = "body") |>
    flextable::bg(i=3,j=2, bg = "#70BBFF", part = "body") |>
    flextable::bg(i=2,j=3, bg = "#70BBFF", part = "body") |>
    flextable::bg(i=2,j=4, bg = "#F5C4AF", part = "body") |>
    flextable::bg(i=3,j=4, bg = "#EB895F", part = "body") |>
    flextable::bg(i=4,j=2, bg = "#A0D1FF", part = "body") |>
    flextable::bg(i=4,j=3, bg = "#F5C4AF", part = "body") |>
    flextable::bg(i=4,j=4, bg = "#E46C0A", part = "body") |>
    flextable::align(align = "center", part = "all") |>
    flextable::prepend_chunks(i = 2, j = 1, value = flextable::as_image(system.file("icons", "spc", "variation", "VariationIconImprovementHigh.png", package = "qiverse.qimatrix"), width = 0.8, height = 0.8)) |>
    flextable::prepend_chunks(i = 3, j = 1, value = flextable::as_image(system.file("icons", "spc", "variation", "VariationIconCommonCause.png", package = "qiverse.qimatrix"), width = 0.8, height = 0.8)) |>
    flextable::prepend_chunks(i = 4, j = 1, value = flextable::as_image(system.file("icons", "spc", "variation", "VariationIconConcernLow.png", package = "qiverse.qimatrix"), width = 0.8, height = 0.8)) |>
    flextable::append_chunks(i = 1, j = 2, value = flextable::as_image(system.file("icons", "funnel", "FunnelIconBetter.png", package = "qiverse.qimatrix"), width = 1, height = 1)) |>
    flextable::append_chunks(i = 1, j = 3, value = flextable::as_image(system.file("icons", "funnel", "FunnelIconNeutral.png", package = "qiverse.qimatrix"), width = 1, height = 1)) |>
    flextable::append_chunks(i = 1, j = 4, value = flextable::as_image(system.file("icons", "funnel", "FunnelIconWorse.png", package = "qiverse.qimatrix"), width = 1, height = 1))

}

