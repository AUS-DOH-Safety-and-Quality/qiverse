#' Request metadata for all Workspaces in Tenant
#'
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_ds') to create this token.
#'
#' @return DataFrame containing the names, GUIDs, and capacity GUIDs for all workspaces in tenant.
#' @export
list_workspaces <- function(access_token) {
  base_url <- "https://api.powerbi.com/v1.0/myorg/groups"
  metadata_request <- httr::GET(url = base_url,
                                config = get_auth_header(access_token),
                                httr::content_type_json())

  if (metadata_request$status_code != 200) {
    stop("API request returned status code: ", metadata_request$status_code, "!",
         call. = TRUE)
  }

  metadata_content <- httr::content(metadata_request)

  content_to_dataframe <- metadata_content$value |>
    dplyr::bind_rows()
  content_to_dataframe <- content_to_dataframe[,c("name", "id", "capacityId")]
  names(content_to_dataframe) <- c("Workspace", "WorkspaceId", "CapacityID")
  content_to_dataframe
}


list_dataflows <- function(workspace_id, access_token) {
  base_url <- paste0("https://api.powerbi.com/v1.0/myorg/groups/", workspace_id, "/dataflows")
  metadata_request <- httr::GET(url = base_url,
                                config = get_auth_header(access_token),
                                httr::content_type_json())

  if (metadata_request$status_code != 200) {
    stop("API request returned status code: ", metadata_request$status_code, "!",
         call. = TRUE)
  }

  metadata_content <- httr::content(metadata_request)

  content_to_dataframe <- metadata_content$value |>
    purrr::keep(\(x) !is.null(x$name)) |>
    purrr::map_dfr(\(x) {
      x[c("name", "objectId")]
    })

  names(content_to_dataframe) <- c("Dataflow", "DataflowId")
  content_to_dataframe
}
