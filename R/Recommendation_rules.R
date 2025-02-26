#' Recommendation rules
#' @param indicator A character vector
#' @param hospital A character vector
#' @param numerator A numeric vector, sum of the numerator field
#' @param denominator A numeric vector, sum of the denominator field
#' @param betteris A character string, shows direction of positive change.
#' "Higher" or "Lower"
#' @param astro Date of latest instance of Astronomical Point pattern
#' @param shift Date of latest instance of Shift pattern
#' @param trend Date of latest instance of Trend pattern
#' @param twointhree Date of latest instance of Two in Three pattern
#' @param fpl_astro Date of max period end of funnel plot data if outlier
#' @param indicatorgroup A character vector, grouping for indicators.
#' Dropped by default
#'
#' @return A data table with added qsg recommendation and reason
#'
#'  ## Additional requirements for this function
#'  The input dataframe must have been passed through [rSQuIS::pattern_rules()]
#' @export
#' @family Pattern detection functions
#' @examples -

recommendation_rules <- function(indicator, hospital, numerator, denominator,
                                 betteris, astro, shift, trend, twointhree,
                                 fpl_astro, indicatorgroup = NA) {
  # Dealing with undefined global functions or variables
  `:=` <- NULL
  # Dealing with undefined global functions or variables (see datatable-import
  # vignette)
  min_numerator <- min_denominator <- Reason <- NULL

  #create data table
  input_data <- data.table::data.table(indicator, hospital, indicatorgroup,
                           numerator = as.numeric(numerator),
                           denominator = as.numeric(denominator),
                           betteris, fpl_astro,
                           astro, shift, trend, twointhree)

  # REC001 For all indicators: create columns called qsg_recommendation
  #and Reason and add a default value of "Pending HQIU analyst review".
  input_data[, `:=`(Reason = "Pending HQIU analyst review",
                    `QSG Recommendation` = "Pending HQIU analyst review")]
  #if no patterns are detected, stand down
  input_data[is.na(astro) & is.na(shift) & is.na(trend) & is.na(twointhree) &
  is.na(fpl_astro), `:=`(Reason = "No patterns identified",
                  `QSG Recommendation` = "Standing down")]

  #create a table of lookup values, the cut off's for minimum numerators and
  #denominators for each indicator
  rec_values <- data.table::data.table(indicator =
      c("Incidents Open Disclosure", "Complaints Response 30 Day",
      "Apgar", "In Hospital Mortality AMI", "In Hospital Mortality Stroke",
      "In Hospital Mortality Pneumonia", "HSMR", "C Section",
      "Stillbirth", "Perinatal Mortality", "Incidents SAC1 Evaluations 6m",
      "Staff Safe To Speak Up", "Survival 30-day Hip Fracture", "Hand Hygiene",
      "Incident Reporting Rate", "Post-partum haemorrhage",
      "Incident Patient Outcome (Harm)", "Induction Rate", "Incidents SAC128",
      "Vaginal Birth (2) After C Section", "Staff Treated Fairly",
      "Staff Committed To Safety", "Staff Feel Empowered",
      "Staff Friends And Family", "Restraint Rates", "Seclusion Rates",
      "MH Consumer Experience", "MH Outcomes (Consumer Rated - IP)",
      "MH Outcomes (Clinician Rated - IP)", "MH % ED Admitted <4hrs with MHU",
      "MH % ED Admitted <4hrs w/o MHU"),
      min_numerator = c(24, 50, 10, 2, 5, 5, 24, 25, 5, 5, 4, 10, 24, 24,
                        30, 24, 50, 10, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 30,
                        30, 0),
      min_denominator = c(24, 50, 100, 0.1, 2, 2, 24, 24, 100, 100, 4, 250,
                          250, 100, 5, 24, 12, 24, 0, 0, 0, 0, 0, 0, 365,
                          0, 0, 0, 0, 0, 0))

  #merge the lookup table and data
  input_data <- merge(input_data, rec_values, by = "indicator", all = TRUE)

  #if the sum of numerator is less than required minimum,
  #and the sum of the denominator is less than the required minimum, stand down
  input_data[numerator < min_numerator & denominator < min_denominator &
               Reason == "Pending HQIU analyst review", `:=`(
    Reason = "Small numerator and denominator",
    `QSG Recommendation` = "Standing down")]
  #if the sum of numerator is less than required minimum, stand down
  input_data[numerator < min_numerator &
               Reason == "Pending HQIU analyst review", `:=`(
    Reason = "Small numerator",
    `QSG Recommendation` = "Standing down")]
  #if the sum of denominator is less than required minimum, stand down
  input_data[denominator < min_denominator &
               Reason == "Pending HQIU analyst review", `:=`(
    Reason = "Small denominator",
    `QSG Recommendation` = "Standing down")]
  #if the sum of numerator and sum of denominator are equal and betteris higher
  #the rate is 1 so patterns are not useful
  input_data[numerator == denominator & betteris == "Higher"
             & Reason == "Pending HQIU analyst review", `:=`(
    Reason = "Favourable maximum value sustained over entire spc period",
    `QSG Recommendation` = "Standing down")]
  #if numerator is 0 and betteris lower, patterns are not useful
  input_data[numerator == 0 & betteris == "Lower" &
               Reason == "Pending HQIU analyst review", `:=`(
    Reason = "Favourable minimum value sustained over entire spc period",
    `QSG Recommendation` = "Standing down")]
  #clean up
  input_data[, `:=`(betteris = NULL,
                    min_numerator = NULL,
                    min_denominator = NULL)]

  #if indicator group is not needed, remove it
  if (length(indicatorgroup) == 1) input_data[, indicatorgroup := NULL]

  return(input_data)
}
