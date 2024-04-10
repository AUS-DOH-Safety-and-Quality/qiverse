#' Download PowerBI Dataflow Table
#'
#' This function provides the ability to download tables from within PowerBI
#' dataflow themselves, rather than requiring that they first be exposed via a
#' PowerBI dataset.
#'
#' @param workspace_name The PowerBI workspace name.
#' @param dataflow_name The name of the PowerBI Dataflow within the workspace.
#' @param table_name The name of the table within the PowerBI Dataflow to be
#' accessed.
#' @param access_token The token generated with the correct PowerBI Dataflow
#' permissions. Use get_az_tk('pbi_df') to create this token.
#' @param verbose Whether to print status messages while the function is
#' running. Default is TRUE.
#'
#' @return A tibble of the PowerBI table from the PowerBI dataflow.
#' @export
#' @examples
#'  \dontrun{
#' # Create PowerBI Dataflow azure token
#' tk <- get_az_tk('pbi_df')
#'
#' # Load AD dummy file from PowerBI dataflow
#' ad_dummy <- download_dataflow_table(
#'   workspace_name = "DOH - Reference Data",
#'   dataflow_name = "calendar",
#'   table_name = "CalendarToToday",
#'   access_token = tk$credentials$access_token
#' )
#'}
download_dataflow_table <- function(workspace_name, dataflow_name,
                                    table_name, access_token,
                                    verbose = TRUE) {
  # Create the authorisation header that will be needed for all queries
  auth_header <- get_auth_header(access_token)

  if (interactive() && isTRUE(verbose)) {
    message("Fetching dataflow metadata...")
  }

  # Using the cluster URL instead of the generic API redirect (api.powerbi.com)
  # allows for additional API endpoints
  cluster_url <- get_cluster_url(access_token)

  # We need the unique identifier for the dataflow of interest, but we only
  # have its name. As such, we request the metadata of all dataflows that we
  # have access to and filter the results
  target_dataflow <- httr::GET(url = paste0(cluster_url, "/metadata/v201901/gallery/dataflows"),
                              config = auth_header,
                              httr::content_type_json()) |>
    httr::content() |>
    purrr::keep(\(x) {
      if (is.null(x$cdsaModel$displayName)) {
        FALSE
      } else {
        x$workspaceName == workspace_name && x$cdsaModel$displayName == dataflow_name
      }
    })

  # Keep last edited target dataflow
  if (length(target_dataflow) > 1) {
    # Find max last edited date
    max_last_edit_date <- purrr::map(target_dataflow, \(x) {
      x$cdsaModel$lastEditedTimeUTC})|>
      # extract max of list
      purrr::reduce(max)
    # Choose last edited date dataflow
    target_dataflow <- target_dataflow |>
      purrr::keep(\(x) {x$cdsaModel$lastEditedTimeUTC == max_last_edit_date})
  }

  # Extract
  dataflow_id <- target_dataflow[[1]]$cdsaModel$objectId

  # Now we can request detailed metadata for the dataflow, including column names
  # and types, as well the storage location of the actual CSV file
  all_tables <- httr::GET(url = paste0(cluster_url, "/metadata/v201606/cdsa/dataflows/", dataflow_id, "/contentandcache"),
                          config = auth_header,
                          httr::content_type_json()) |>
    httr::content()

  target_table <- purrr::keep(all_tables$content$entities, \(x) {
    x$name == table_name
  })


  # Extract the column names and types
  table_colnames <- purrr::map_chr(target_table[[1]]$attributes, "name")

  pbi_to_readr_type_map <- list(
    "string" = readr::col_character(),
    "date" = readr::col_date(format = "%d/%m/%Y"),
    "double" = readr::col_double(),
    "int64" = readr::col_integer(),
    "boolean" = readr::col_logical(),
    "dateTime" = readr::col_datetime(format = "%d/%m/%Y %H:%M:%S %p"),
    "time" = readr::col_time(format = "%H:%M:%S")
  )
  table_coltypes <- purrr::map(target_table[[1]]$attributes, \(x) {
    pbi_to_readr_type_map[[x$dataType]]
  })

  # While we have the URL for the table, we don't have 'permission' to
  # download it using our current access token. Instead, we need to use the
  # access token to request a Shared Access Signature (SAS). This SAS summarises
  # the extent of data that we are allowed to access, and the length of time
  # that it is valid for use.
  sas_query <- httr::POST(
      url = paste0(cluster_url, "/metadata/v201606/cdsa/dataflows/", dataflow_id, "/storageAccess"),
      body = jsonlite::toJSON(
        list("TokenLifetimeInMinutes" = 10,
             "Permissions" = "Read",
             "EntityName" = table_name),
        auto_unbox = TRUE
      ),
      config = auth_header,
      httr::content_type_json()
    ) |>
    httr::content()

  sas_key <- sas_query$accessDetails[[1]]$blobContainerSas

  if (interactive() && isTRUE(verbose)) {
    message("Downloading dataflow table...")
  }

  # Now we can simply append the generated SAS to the blob storage download URL
  # and pass the result to the read_csv function. This will handle downloading
  # the table to R and adding the extracted column names
  readr::read_csv(paste0(target_table[[1]]$partitions[[1]]$location, "&", sas_key),
                  col_names = table_colnames,
                  col_types = table_coltypes)
}
