init_access_token <- function(access_token = NULL) {
  if (is.null(access_token)) {
    if (!requireNamespace("qiverse.azure", quietly = TRUE)) {
      stop("Package 'qiverse.azure' is required but not installed. ", call. = FALSE)
    }
    access_token <- qiverse.azure::get_az_tk("sp")
  } else {
    access_token
  }
}
