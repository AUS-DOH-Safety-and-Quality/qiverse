#' Download SharePoint list
#'
#' @param list_url The URL of the list when accessing the list via your web
#' browser.
#' @param token The token generated with the correct SharePoint permissions.
#' Use get_az_tk('sp") to create this token.
#'
#' @return A data.table of the SharePoint list
#' @export
#' @examples
#'  \dontrun{
#' # Get the token for SharePoint
#' tk <- get_az_tk('sp')
#'
#' # Download a particular view for the SharePoint list
#' list_data <- download_sharepoint_list(
#' paste0("https://myexample.sharepoint.com/sites/",
#'        "Indicator%20Metadata/Lists/Indicators/",
#'        "Latest.aspx"),
#'   token = tk
#' )
#'
#' # Download the default view for the SharePoint list
#' list_data <- download_sharepoint_list(
#' paste0("https://myexample.sharepoint.com/sites/",
#'        "Indicator%20Metadata/Lists/Indicators/"),
#'   token = tk
#' )
#'}
download_sharepoint_list <- function(
    list_url,
    token
) {
  # Initialise objects ####
  ## Check if the string 'Lists' exists in the url, and return the position
  list_url_start <- gregexpr("Lists", list_url)[[1]][1]
  ## If 'List' does not exist then stop the function, otherwise continue to
  # create objects
  if (list_url_start == -1) {
    stop("The URL provided is not a SharePoint list")
  } else {
    # Extract the sharepoint url ####
    site_url <- substr(
      x = list_url,
      start = 1,
      stop = list_url_start - 1
    )
    # Extract the list name ####
    ## Extract string after list
    list_display_name <- substr(
      x = list_url,
      start = list_url_start + 6,
      stop = nchar(list_url)
    )
    ## Remove the page name if it exists(*.aspx)
    if (gregexpr("/", list_display_name)[[1]][1] != -1) {
      list_display_name <- substr(
        x = list_display_name,
        start = 1,
        stop = gregexpr("/", list_display_name)[[1]][1] - 1
      )
    }
    ## Convert URL to plain text
    list_display_name <- utils::URLdecode(list_display_name)
  }

  # Connect to sharepoint site object ####
  site <- Microsoft365R::get_sharepoint_site(
    site_url = site_url,
    token = token
  )

  # Extract table of metadata for all lists in sharepoint site ####
  ## Loop function over all lists to extract metadata
  all_lists <- lapply(site$get_lists(), function(x) {
    # Extract properties of this list
    output <- x$properties |>
      # Convert to data.frame for easy manipulation/appending
      as.data.frame()
    # Choose only these select columns, and first row as they are duplicated
    output[1, c("displayName", "name", "id", "createdDateTime", "description",
                "webUrl")]
  }) |>
    # Combine the list of lists into a single data.table
    dplyr::bind_rows()

  # Check if list_display_name exists
  # If empty list_display_name, stop due to error
  if (!(list_display_name %in% all_lists[, "name"])) {
    stop("The URL provided is not a SharePoint list")
  }

  # Extract list for chosen sharepoint list ####
  list_data <- site$get_list(
    list_id = all_lists[all_lists$name == list_display_name, "id"]
  )$list_items() |>
    as.data.frame()

  # Output dataset ####
  return(list_data)
}
