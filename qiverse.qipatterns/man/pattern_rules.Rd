% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pattern_rules.R
\name{pattern_rules}
\alias{pattern_rules}
\title{Apply pattern rules}
\usage{
pattern_rules(
  numerator,
  denominator,
  period_end,
  unique_key,
  spc_chart_type = "p",
  multiplier = 1,
  better_is = "Lower",
  trend_size = 5,
  shift_size = 7
)
}
\arguments{
\item{numerator}{A numeric vector}

\item{denominator}{A numeric vector. Set as NA for g and t spc_chart_type.}

\item{period_end}{A vector of dates of type Date format: \%d/\%m/\%Y:15/12/2022}

\item{unique_key}{A key to identify each indicator group combination}

\item{spc_chart_type}{A string identifying the type of spc chart. Default "p"}

\item{multiplier}{A string identifying the multiplication factor. Default 1}

\item{better_is}{A character string, shows direction of positive change.
"Higher" or "Lower"}

\item{trend_size}{The number of points in a trend pattern}

\item{shift_size}{The number of points in a shift pattern}
}
\value{
The input data with appended identified patterns
}
\description{
Apply pattern rules
}
\seealso{
Other Pattern detection functions: 
\code{\link{pattern_detection}()},
\code{\link{valid_spc}()}
}
\concept{Pattern detection functions}
