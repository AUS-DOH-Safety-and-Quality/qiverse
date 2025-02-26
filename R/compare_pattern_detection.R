# Parameters ####
## Set the parameters for the "old patterns" and "new patterns".


#' Compare Pattern Detection
#'
#' @param pattern_output A data table produced by [qiverse.qipatterns::pattern_detection()]
#' @param filepath_patterns_old The SharePoint file path of the last pattern run
#' @param tk_sp The SharePoint connection token from [qiverse.azure::get_az_tk()]
#' @param sheet_name The excel sheet name to input
#' @return A combined data table of the old and new patterns
#' @export
#'
#' @examples -
compare_pattern_detection <- function(
    pattern_output,
    filepath_patterns_old = paste0("https://wahealthdept.sharepoint.com/:x:/r/",
    "sites/SafetyandQualityIndicatorSetSQuIS/internal/",
    "SQuIS%20O365%20Confidential%20Documents/Patterns/",
    "MostRecentPatternRecognition/",
    "LastPatternRun.xlsx",
    "?d=w600d2bd92db349bea66c22dd81b10c74&csf=1&web=1&e=ax7H92"),
    tk_sp,
    sheet_name = "Data"
    ) {
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  . <- Key_To_Match <- Indicator <- Hospital <- Astro <- Trend <- TwoInThree <-
    Shift <- Funnel <- LastReviewed <- Manual_Review <- Sequence <-
    UniquePatternKey <- `QSG Recommendation` <- Manual_Review_Text <-
    `:=` <- .SD <- .N <- NULL

  # Assign data object
  squis_data <- pattern_output

  ## SQUIS Data Old
  #download sharepoint file
  squis_data_old <- qiverse.sharepoint::download_sharepoint_file(
    site_url = "https://wahealthdept.sharepoint.com/sites/SafetyandQualityIndicatorSetSQuIS/internal/", #nolint
    file_url = filepath_patterns_old,
    token = tk_sp
  ) |>
    #read the excel output
    readxl::read_xlsx(sheet = sheet_name) |>
    data.table::as.data.table()

  # Format Data ####

  ### Add in new columns to be filled out
  squis_data[, `:=`(LastReviewed = as.Date(NA), QSGFlag = as.character(NA),
                    `Funnel Text` = as.character(NA),
                    `Escalation Notes` = as.character(NA),
                    UniquePatternKey = as.character(NA),
                    Manual_Review = as.character(NA),
                    Sequence = 9999)]

  ### Add Key to Match
  squis_data[, Key_To_Match := paste0(Indicator, "_", Hospital, "_", Astro, "_",
                                      Trend, "_", TwoInThree, "_", Shift, "_",
                                      ifelse(is.na(Funnel), "N", "Y"))]
  ### Reorder columns
  data.table::setcolorder(squis_data,
                          c("Key_To_Match", "UniquePatternKey", "Indicator",
                            "Hospital", "Sequence", "QSG Theme", "Astro",
                            "Trend", "TwoInThree", "Shift", "Funnel",
                            "Numerator", "Denominator", "Reason",
                            "QSG Recommendation", "LastReviewed",
                            "Manual_Review", "QSGFlag", "Funnel Text",
                            "Escalation Notes"))

  ## SQuIS Data Old
  #need to format the dates as dates
  squis_data_old[, Astro := as.Date(Astro, tryFormats =
                                      c("%Y-%m-%d", "%d/%m/%Y"))]
  squis_data_old[, Trend := as.Date(Trend, tryFormats =
                                      c("%Y-%m-%d", "%d/%m/%Y"))]
  squis_data_old[, TwoInThree := as.Date(TwoInThree, tryFormats =
                                      c("%Y-%m-%d", "%d/%m/%Y"))]
  squis_data_old[, Shift := as.Date(Shift, tryFormats =
                                      c("%Y-%m-%d", "%d/%m/%Y"))]
  squis_data_old[, Funnel := as.Date(Funnel, tryFormats =
                                      c("%Y-%m-%d", "%d/%m/%Y"))]
  squis_data_old[, LastReviewed := as.Date(LastReviewed, tryFormats =
                                      c("%Y-%m-%d", "%d/%m/%Y"))]
  squis_data_old[, Manual_Review := as.character(NA)]
  ### Add Key to Match
  squis_data_old[, Key_To_Match := paste0(Indicator, "_", Hospital, "_",
                                          Astro, "_", Trend, "_",
                                          TwoInThree, "_", Shift, "_",
                                          ifelse(is.na(Funnel), "N", "Y"))]
  ### Reorder columns
  data.table::setcolorder(squis_data_old, "Key_To_Match")

  ### Remove any patterns not previously reviewed
  squis_data_old <- squis_data_old[!is.na(LastReviewed)]

  # Combine old and new items ####
  ## Reviewed patterns that are still up to date
  ### Find the latest key for each Indicator/Hospital combination
  # and see if there are any new items
  current <- squis_data[Key_To_Match %in% squis_data_old[, .SD[.N],
                        by = .(Indicator, Hospital)][, Key_To_Match]]
  current[, c("UniquePatternKey", "Sequence", "Reason", "QSG Recommendation",
              "LastReviewed", "Manual_Review", "QSGFlag", "Funnel Text",
              "Escalation Notes") := NULL]
  current <- merge(current,
                   squis_data_old[, c("Key_To_Match", "UniquePatternKey",
                                      "Sequence", "Reason",
                                      "QSG Recommendation", "LastReviewed",
                                      "Manual_Review", "QSGFlag", "Funnel Text",
                                      "Escalation Notes"), with = FALSE],
                   by = "Key_To_Match", all.x = TRUE, sort = FALSE)
  data.table::setcolorder(current, names(squis_data_old))

  ## Old reviewed patterns that are no longer in new items
  old <- squis_data_old[!(Key_To_Match %in% squis_data$Key_To_Match)]

  ## Newest items
  new <- squis_data[!(Key_To_Match %in% squis_data_old$Key_To_Match)]

  # Combine items ####
  comb <- rbind(current, old, new)

  ## Find max date of pattern detected, and sort by this
  data.table::setorder(comb, "QSG Theme", Indicator, Hospital, Sequence)
  comb[Sequence == 9999, Sequence := NA]

  ## Apply sequence number to those with missing sequence numbers
  comb_na <- comb[is.na(Sequence)]
  for (i in 1:comb_na[, .N]) {
    comb[Key_To_Match == comb_na[i, Key_To_Match] & is.na(Sequence),
         # Set Sequence number to max number plus one
         Sequence := comb[Indicator == comb_na[i, Indicator] &
                            Hospital == comb_na[i, Hospital],
                          max(cbind(0, Sequence), na.rm = TRUE)] + 1]
  }

  ## Apply UniquePatternKey to those with missing keys
  comb[is.na(UniquePatternKey), UniquePatternKey := paste0(Indicator, "_",
                                                           Hospital, "_",
                                                           Sequence)]

  ## Remove Key_To_Match
  comb[, Key_To_Match := NULL]

  # Add Flag for manual reviews
  manual_review_text <- rbind(
    ## Flag anything that needs analyst review
    comb[paste0(Indicator, "_", Hospital) %in%
           comb[is.na(LastReviewed),
                paste0(Indicator, "_", Hospital)] &
           `QSG Recommendation` != "Standing down",
         .SD[.N], by = .(Indicator, Hospital)][, .(
           Indicator, Hospital, Manual_Review_Text =
             paste0("Yes, ", "HQIU analyst review"))],
    ## Flag all new funnel plot outliers as manual reviews
    comb[paste0(Indicator, "_", Hospital) %in%
           squis_data[!is.na(Funnel), paste0(Indicator, "_", Hospital)],
         .SD[.N], by = .(Indicator, Hospital)][, .(
           Indicator, Hospital, Manual_Review_Text =
             paste0("Yes, ", "HQIU analyst review"))]
  ) |>
    unique()
  ## Add manual review text back to comb
  comb <- comb[manual_review_text, on = .(Indicator, Hospital),
               Manual_Review := Manual_Review_Text]

  ## For those without any manual review text
  manual_review_text <- comb[paste0(Indicator, "_", Hospital) %in%
                               comb[is.na(LastReviewed),
                               paste0(Indicator, "_", Hospital)] &
                               is.na(Manual_Review),
                             .SD[.N],
                             by = .(Indicator, Hospital)][,
                             .(Indicator, Hospital, Manual_Review_Text =
                             paste0("Yes, ", `QSG Recommendation`))]
  comb <- comb[manual_review_text, on = .(Indicator, Hospital),
               Manual_Review := Manual_Review_Text]

  return(comb)

}
