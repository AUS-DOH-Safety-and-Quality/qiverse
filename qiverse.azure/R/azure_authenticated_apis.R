#' Make a call to a web API which requires an Azure access token for
#' authentication
#'
#' @param method The HTTP method to call (e.g., "POST", "GET", etc.)
#' @param url The web URL of the API to be called
#' @param access_token The azure access token string which will be used for
#' authentication
#' @param ... Any additional arguments to be passed to the method call. See the
#' httr documentation for the intended method for valid arguments
#'
#' @return The result of the API call
#'
#' @family Azure methods
#' @export
az_authenticated_api_query <- function(method, url, access_token, ...) {
  httr_func <- utils::getFromNamespace(method, "httr")

  # Convert http:// addresses to https://
  url <- gsub("^http://", "https://", url)

  # Prepend https:// if missing
  url <- ifelse(grepl("^https://", url), url, paste0("https://", url))

  args <- list(...)
  args$url <- url
  args$config <- httr::add_headers("Authorization" = paste("Bearer",
                                                           access_token))
  args[[length(args) + 1]] <- httr::content_type_json()

  if (!is.null(args$body)) {
    args$body <- jsonlite::toJSON(args$body, auto_unbox = TRUE)
  }

  query_result <- do.call(httr_func, args)
  query_content <- httr::content(query_result)

  if (length(query_content) > 0) {
    return(query_content)
  } else {
    return(query_result)
  }
}
