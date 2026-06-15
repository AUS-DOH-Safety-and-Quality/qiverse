#' Request metadata for all deployment pipelines in Tenant
#'
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_df') to create this token.
#'
#' @return DataFrame containing the names, GUIDs, and descriptions for all
#' deployment pipelines in tenant you have access to.
#' @examples
#'  \dontrun{
#' # example code to get access token and list pipelines
#' token <- qiverse.azure::get_az_tk('pbi_df')
#' list_pipelines(token$credentials$access_token)
#' }
#' @export
list_pipelines <- function(access_token) {
  # List all Deployment Pipelines that the user can access
  metadata_request <- httr::GET(
    url = "https://api.powerbi.com/v1.0/myorg/pipelines",
    config = get_auth_header(access_token),
    httr::content_type_json()
  )

  if (metadata_request$status_code != 200) {
    stop("API request returned status code: ", metadata_request$status_code, "!",
         call. = TRUE)
  }

  metadata_content <- httr::content(metadata_request)

  content_to_dataframe <- .bind_rows_base(metadata_content$value)
  content_to_dataframe <- content_to_dataframe[,c("id",
                                                  "displayName",
                                                  "description")]
  names(content_to_dataframe) <- c("deploymentPipelineId",
                                   "deploymentPipelineName",
                                   "deploymentPipelineDescription")
  content_to_dataframe
}

#' Request metadata for selected deployment pipeline name
#'
#' @param deployment_pipeline_name Name of the deployment pipeline you want to
#' get metadata for
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_df') to create this token.
#'
#' @return DataFrame containing the names, GUIDs, and descriptions for the
#' selected deployment pipeline
#' @examples
#'  \dontrun{
#' # example code to get access token and list pipelines
#' token <- qiverse.azure::get_az_tk('pbi_df')
#' get_pipeline_metadata("my-pipeline", token$credentials$access_token)
#' }
#' @export
get_pipeline_metadata <- function(deployment_pipeline_name, access_token) {
  deployment_pipelines <- list_pipelines(access_token)

  if (!(deployment_pipeline_name %in% deployment_pipelines$deploymentPipelineName)) {
    stop("No deployment pipeline called: ", deployment_pipeline_name, " in tenant!", call. = FALSE)
  }

  deployment_pipeline_data <- deployment_pipelines[deployment_pipelines$deploymentPipelineName == deployment_pipeline_name,]

  deployment_pipeline_data
}

#' Request users for selected deployment pipeline name
#'
#' @param deployment_pipeline_name Name of the deployment pipeline you want to
#' get user access details for
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_df') to create this token.
#'
#' @return DataFrame containing the access right, identifier, and principal type
#'  for the selected deployment pipeline
#' @examples
#'  \dontrun{
#' # example code to get access token and list pipelines
#' token <- qiverse.azure::get_az_tk('pbi_df')
#' get_pipeline_users("my-pipeline", token$credentials$access_token)
#' }
#' @export
get_pipeline_users <- function(deployment_pipeline_name, access_token) {
  # Get deployment pipeline id
  pipeline_metadata <- get_pipeline_metadata(deployment_pipeline_name, access_token)
  deployment_pipeline_id <- pipeline_metadata$deploymentPipelineId

  # Get users of deployment pipeline
  request <- httr::GET(
    url = paste0(
      "https://api.powerbi.com/v1.0/myorg/pipelines/",
      deployment_pipeline_id,
      "/users"
    ),
    config = get_auth_header(access_token),
    httr::content_type_json()
  )

  if (request$status_code != 200) {
    stop("API request returned status code: ", request$status_code, "!",
         call. = TRUE)
  }

  content <- httr::content(request)

  content_to_dataframe <- .bind_rows_base(content$value)
  content_to_dataframe <- content_to_dataframe[,c("accessRight",
                                                  "identifier",
                                                  "principalType")]
  content_to_dataframe
}

#' Request stages for selected deployment pipeline name
#'
#' @param deployment_pipeline_name Name of the deployment pipeline you want to
#' get stage details for
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_df') to create this token.
#'
#' @return DataFrame containing the order, workspace id and workspace name
#' for the stages in the selected deployment pipeline
#' @examples
#'  \dontrun{
#' # example code to get access token and list pipelines
#' token <- qiverse.azure::get_az_tk('pbi_df')
#' get_pipeline_stages("my-pipeline", token$credentials$access_token)
#' }
#' @export
get_pipeline_stages <- function(deployment_pipeline_name, access_token) {
  # Get deployment pipeline id
  pipeline_metadata <- get_pipeline_metadata(deployment_pipeline_name, access_token)
  deployment_pipeline_id <- pipeline_metadata$deploymentPipelineId

  # Get stages of deployment pipeline
  request <- httr::GET(
    url = paste0(
      "https://api.powerbi.com/v1.0/myorg/pipelines/",
      deployment_pipeline_id,
      "/stages"
    ),
    config = get_auth_header(access_token),
    httr::content_type_json()
  )

  if (request$status_code != 200) {
    stop("API request returned status code: ", request$status_code, "!",
         call. = TRUE)
  }

  content <- httr::content(request)

  content_to_dataframe <- .bind_rows_base(content$value)
  content_to_dataframe <- content_to_dataframe[,c("order",
                                                  "workspaceId",
                                                  "workspaceName")]
  content_to_dataframe
}
