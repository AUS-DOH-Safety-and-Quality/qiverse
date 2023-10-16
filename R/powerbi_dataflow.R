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
#'   workspace_name = "DOH - Safety and Quality",
#'   dataflow_name = "active_directory",
#'   table_name = "dummy",
#'   access_token = tk$credentials$access_token)
#'}
download_dataflow_table <- function(workspace_name, dataflow_name,
                                    table_name, access_token) {
  # Create the authorisation header that will be needed for all queries
  auth_header <- httr::add_headers(Authorization = paste("Bearer",
                                                         access_token))

  # This is the primary API endpoint which all queries will be based on
  base_url <- "https://api.powerbi.com/v1.0/myorg/groups/"

  # We need the unique identifier for the workspace of interest, but we only
  # have its name. As such, we request the metadata of all workspaces that we
  # have access to, and filter the results for the workspace of interest
  all_workspaces <- httr::content(
    httr::GET(url = base_url,
              config = auth_header,
              httr::content_type_json())
  )

  target_workspace <- purrr::keep(all_workspaces$value, \(x) {
    x$name == workspace_name
  })

  workspace_id <- target_workspace[[1]]$id

  # Next we follow the same procedure for obtaining the unique ID of the target
  # dataflow, but instead request the metadata for all dataflows only within
  # the workspace of interest
  all_dataflows <- httr::content(
    httr::GET(url = paste0(base_url, workspace_id, "/dataflows"),
              config = auth_header,
              httr::content_type_json())
  )

  target_dataflow <- purrr::keep(all_dataflows$value, \(x) {
    if (is.null(x$name)) {
      FALSE
    } else {
      x$name == dataflow_name
    }
  })

  dataflow_id <- target_dataflow[[1]]$objectId

  # Now that we have the unique identifiers for both the workspace and dataflow
  # of interest, we can request the full metadata for all tables within the
  # targeted dataflow and then filter to retain only the metadata for the
  # targeted table
  all_tables <- httr::content(
    httr::GET(url = paste0(base_url, workspace_id, "/dataflows/", dataflow_id),
              config = auth_header,
              httr::content_type_json())
  )

  target_table <- purrr::keep(all_tables$entities, \(x) {
    x$name == table_name
  })

  # Extract the Azure Blob Storage download URL for the most recent
  # refresh of the selected table
  location <- target_table[[1]]$partitions[[1]]$location

  # Extract the column names
  table_colnames <- purrr::map_chr(target_table[[1]]$attributes, "name")

  # While we have the URL for the table, we don't have 'permission' to
  # download it using our current access token. Instead, we need to use the
  # access token to request a Shared Access Signature (SAS). This SAS summarises
  # the extent of data that we are allowed to access, and the length of time
  # that it is valid for use.
  pbi_url_base <- all_workspaces$`@odata.context`
  pbi_url <- strsplit(pbi_url_base, split = "v1.0/", fixed = TRUE)[[1]][1]
  pbi_url_https <- gsub(pattern = "http:", replacement = "https:", x = pbi_url)
  url <- paste0(pbi_url_https, "metadata/v201606/cdsa/dataflows/",
                dataflow_id, "/storageAccess")

  body_list <- list(
    "TokenLifetimeInMinutes" = 10,
    "Permissions" = "Read",
    "EntityName" = table_name
  )

  sas_query <- httr::content(
    httr::POST(url = url,
               body = jsonlite::toJSON(body_list, auto_unbox = TRUE),
               config = auth_header,
               httr::content_type_json())
  )

  sas_key <- sas_query$accessDetails[[1]]$blobContainerSas

  # Now we can simply append the generated SAS to the blob storage download URL
  # and pass the result to the read_csv function. This will handle downloading
  # the table to R and adding the extracted column names
  readr::read_csv(paste0(location, "&", sas_key),
                  col_names = table_colnames,
                  show_col_types = FALSE)
}
