#' Request metadata for all Workspaces in Tenant
#'
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_ds') to create this token.
#'
#' @return DataFrame containing the names, GUIDs, and capacity GUIDs for all workspaces in tenant.
#' @export
list_workspaces <- function(access_token) {
  base_url <- "https://api.powerbi.com/powerbi/databases/v201606/workspaces?PreferClientRouting=true"
  metadata_request <- httr::GET(url = base_url,
                                config = get_auth_header(access_token),
                                httr::content_type_json())

  if (metadata_request$status_code != 200) {
    stop("API request returned status code: ", metadata_request$status_code, "!",
         call. = TRUE)
  }

  metadata_content <- httr::content(metadata_request)
  content_to_dataframe <- purrr::map_dfr(metadata_content, \(metadata){
    do.call(data.frame, purrr::keep(metadata, \(elem) { !is.null(elem) }))
  })
  content_to_dataframe <- content_to_dataframe[,c("name", "id", "capacityObjectId", "capacityUri")]
  names(content_to_dataframe) <- c("Workspace", "WorkspaceId", "CapacityID", "CapacityUri")
  content_to_dataframe
}
