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



#' Request metadata for all dataflows in specified workspace
#'
#' @param workspace Name of the workspace containing dataflows
#' @param access_token The token generated with the correct PowerBI Dataflow
#' permissions. Use get_az_tk('pbi_df') to create this token.
#'
#' @return DataFrame containing the names, GUIDs, and descriptions for all dataflows in workspace
#' @export
list_dataflows <- function(workspace, access_token) {
  workspace_metadata <- qiverse.powerbi::list_workspaces(access_token)
  if (!(workspace %in% workspace_metadata$Workspace)) {
    stop("No workspace called: ", workspace, " in tenant!", call. = FALSE)
  }

  workspace_id <- workspace_metadata[workspace_metadata$Workspace == workspace,]$WorkspaceId
  base_url <- paste0("https://api.powerbi.com/v1.0/myorg/groups/", workspace_id, "/dataflows")
  metadata_request <- httr::GET(url = base_url,
                                config = get_auth_header(access_token),
                                httr::content_type_json())

  if (metadata_request$status_code != 200) {
    stop("API request returned status code: ", metadata_request$status_code, "!",
         call. = TRUE)
  }

  metadata_content <- httr::content(metadata_request)$value

  if (length(metadata_content) == 0) {
    return(NULL)
  }

  content_to_dataframe <- purrr::map_dfr(metadata_content, \(metadata){
    do.call(data.frame, purrr::keep(metadata, \(x){length(x) > 0}))
  })
  content_to_dataframe$Workspace <- workspace
  content_to_dataframe$WorkspaceId <- workspace_id
  if (!("configuredBy" %in% colnames(content_to_dataframe))) {
    content_to_dataframe$configuredBy <- ""
  }
  if (!("name" %in% colnames(content_to_dataframe))) {
    content_to_dataframe$name <- ""
  }
  if (!("objectId" %in% colnames(content_to_dataframe))) {
    content_to_dataframe$name <- ""
  }

  content_to_dataframe <- content_to_dataframe[,c("Workspace", "WorkspaceId", "name", "objectId", "configuredBy")]
  names(content_to_dataframe) <- c("Workspace", "WorkspaceId", "Dataflow", "DataflowId", "DatasetOwner")
  content_to_dataframe
}


list_reports <- function(workspace_id, access_token) {
  base_url <- paste0("https://api.powerbi.com/v1.0/myorg/groups/", workspace_id, "/reports")
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
      x[c("name", "id", "webUrl", "embedUrl")]
    })
  names(content_to_dataframe) <- c("Report", "ReportId", "WebUrl", "EmbedUrl")
  content_to_dataframe
}
