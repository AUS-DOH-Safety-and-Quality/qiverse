get_dataflow_metadata <- function(workspace_name, dataflow_name, access_token,
                                  verbose = TRUE) {
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
                               config = get_auth_header(access_token),
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
  target_dataflow[[1]]
}

get_table_metadata <- function(dataflow_id, table_name, access_token) {
  # Now we can request detailed metadata for the dataflow, including column names
  # and types, as well the storage location of the actual CSV file
  all_tables <- httr::GET(url = paste0(get_cluster_url(access_token), "/metadata/v201606/cdsa/dataflows/", dataflow_id, "/contentandcache"),
                          config = get_auth_header(access_token),
                          httr::content_type_json()) |>
    httr::content()

  rtn <- purrr::keep(all_tables$content$entities, \(x) {
    x$name == table_name
  })[[1]]
  # Locale info (e.g. en-GB) is stored in the content object
  # but not in the table object
  rtn$locale <- all_tables$content$culture
  rtn
}

get_sas_key <- function(dataflow_id, table_name, access_token) {
  # While we have the URL for the table, we don't have 'permission' to
  # download it using our current access token. Instead, we need to use the
  # access token to request a Shared Access Signature (SAS). This SAS summarises
  # the extent of data that we are allowed to access, and the length of time
  # that it is valid for use.
  sas_query <- httr::POST(
    url = paste0(get_cluster_url(access_token), "/metadata/v201606/cdsa/dataflows/", dataflow_id, "/storageAccess"),
    body = jsonlite::toJSON(
      list("TokenLifetimeInMinutes" = 360,
           "Permissions" = "Read",
           "EntityName" = table_name),
      auto_unbox = TRUE
    ),
    config = get_auth_header(access_token),
    httr::content_type_json()
  ) |>
    httr::content()

  sas_query$accessDetails[[1]]$blobContainerSas
}

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
#'   workspace_name = "My Workspace Name",
#'   dataflow_name = "My Dataflow Name",
#'   table_name = "My Table Name",
#'   access_token = tk$credentials$access_token
#' )
#'}
download_dataflow_table <- function(workspace_name, dataflow_name,
                                    table_name, access_token,
                                    verbose = TRUE) {

  target_dataflow <- get_dataflow_metadata(workspace_name, dataflow_name,
                                           access_token, verbose)
  # Extract
  dataflow_id <- target_dataflow$cdsaModel$objectId

  target_table <- get_table_metadata(dataflow_id, table_name, access_token)

  # Extract the column names and types
  table_colnames <- purrr::map_chr(target_table$attributes, "name")

  pbi_to_readr_type_map <- list(
    "string" = readr::col_character(),
    "date" = readr::col_date(format = "%d/%m/%Y"),
    "double" = readr::col_double(),
    "int64" = readr::col_integer(),
    "boolean" = readr::col_logical(),
    "dateTime" = readr::col_datetime(format = "%d/%m/%Y %H:%M:%S %p"),
    "time" = readr::col_time(format = "%H:%M:%S")
  )
  table_coltypes <- purrr::map(target_table$attributes, \(x) {
    pbi_to_readr_type_map[[x$dataType]]
  })

  sas_key <- get_sas_key(dataflow_id, table_name, access_token)


  if (interactive() && isTRUE(verbose)) {
    message("Downloading dataflow table...")
  }

  # Now we can simply append the generated SAS to the blob storage download URL
  # and pass the result to the read_csv function. This will handle downloading
  # the table to R and adding the extracted column names
  readr::read_csv(paste0(target_table$partitions[[1]]$location, "&", sas_key),
                  col_names = table_colnames,
                  col_types = table_coltypes)
}
