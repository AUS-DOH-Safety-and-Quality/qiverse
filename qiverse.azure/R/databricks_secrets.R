#' Interact with the Databricks API for managing secret scopes
#' (collections of secrets). The Databricks API allows for listing, creating,
#' and deleting secret scopes.
#'
#' @param operation The scopes operation to apply
#' ("list", "create", or "delete")
#' @param workspace_url The url of the target databricks workspace
#' @param access_token The azure access token string which will be used for
#' authentication
#' @param scope_name If creating or deleting a secret scope, the name of the
#' target scope
#'
#' @return The result of the API call
#'
#' @family Azure methods
#' @export
db_secret_scopes_api <- function(operation, workspace_url, access_token,
                                 scope_name = NULL) {
  if (!(operation %in% c("list", "create", "delete"))) {
    stop("Specified operation: ", operation, " is not valid!",
         "Valid operations are: \"list\", \"create\", and \"delete\"",
         call. = FALSE)
  }

  if (operation == "list" && !is.null(scope_name)) {
    warning("\"scope_name\" argument not used with \"list\" operation, and ",
            "will be ignored",
            call. = FALSE)
  }

  args <- list(
    method = ifelse(operation == "list", "GET", "POST"),
    url = paste0(workspace_url, "/api/2.0/secrets/scopes/", operation),
    access_token = access_token
  )

  if (operation != "list") {
    args$body <- list("scope" = scope_name)
  }

  do.call(qiverse.azure::az_authenticated_api_query, args)
}

#' Interact with the Databricks API for managing individual secrets.
#' The Databricks API allows for listing, creating,
#' and deleting secret scopes.
#'
#' @param operation The scopes operation to apply
#' ("list", "put", or "delete")
#' @param workspace_url The url of the target databricks workspace
#' @param access_token The azure access token string which will be used for
#' authentication
#' @param scope_name The name of the scope containing the target secret
#' @param secret_name The name of the target secret
#' @param secret_value The value to assign to the target secret
#' @param bytestring Whether the target secret is storing a string of bytes or
#' characters
#'
#' @return The result of the API call
#'
#' @family Azure methods
#' @export
db_secrets_api <- function(operation, workspace_url, access_token,
                           scope_name, secret_name = NULL, secret_value = NULL,
                           bytestring = FALSE) {
  if (!(operation %in% c("list", "put", "delete"))) {
    stop("Specified operation: ", operation, " is not valid!",
         "Valid operations are: \"list\", \"put\", and \"delete\"",
         call. = FALSE)
  }

  args <- list(
    method = ifelse(operation == "list", "GET", "POST"),
    url = paste0(workspace_url, "/api/2.0/secrets/", operation),
    access_token = access_token
  )


  if (operation == "list") {
    args$url <- paste0(args$url, "?scope=", scope_name)
  }

  if (operation != "list") {
    args$body <- list(
      "scope" = scope_name,
      "key" = secret_name
    )
    if (operation == "put") {
      if (bytestring) {
        args$body$bytes_value <- secret_value
      } else {
        args$body$string_value <- secret_value
      }
    }
  }

  do.call(qiverse.azure::az_authenticated_api_query, args)
}

#' Create an Azure authentication token and store it as a Databricks secret
#' when run on a databricks cluster
#'
#' @param token The Azure access token to access the scopes API, and to be
#' stored as a secret
#' @param url The workspace URL of the databricks instance
#' @param username The current user's username, for the secret scope
#'
#' @return NULL
#'
#' @family Azure methods
#' @export
#' @examples
#' \dontrun{
#' # Set with your tenant_id and app_id. Ensure that this has it's own command
#' # chunk, so the command will complete after authentication
#' token <- qiverse.azure::get_az_tk(
#'   "pbi_df",
#'   tenant_id = tenant_id,
#'   app_id_pbi_df = app_id,
#'   auth_type = "device_code"
#' )
#'
#' # Store token as databricks secret
#' update_secret <- qiverse.azure::store_databricks_access_token(
#'   token = token,
#'   url = paste0("https://",
#'     SparkR::sparkR.conf("spark.databricks.workspaceUrl")),
#'   user_name =
#'     SparkR::first(SparkR::sql("SELECT current_user() AS username"))$username
#' )
#'
#' # Check whether the HTTP request returned a success code
#' if(update_secret$status_code == 200) {
#'   "Token successfully updated"
#' } else {
#'   "Error occurred"
#' }
#'}
store_databricks_access_token <- function(token, url, username) {
  # Check if username scope exists, otherwise create
  qiverse.azure::db_secret_scopes_api(
    operation = "create",
    scope_name = username,
    workspace_url = url,
    access_token = token$credentials$access_token
  )

  # Convert token into bytes
  token_as_bytes <- serialize(token, NULL)

  # Put token into databricks secrets
  qiverse.azure::db_secrets_api(operation = "put",
                                workspace_url = url,
                                access_token = token$credentials$access_token,
                                scope_name = username,
                                secret_name = "azure_token",
                                secret_value = token_as_bytes,
                                bytestring = TRUE)
}

#' Refresh Databricks Azure Token Secret
#'
#' @description Generates a new Azure access token and stores it as a Databricks
#' secret in the specified workspace under your username.
#'
#' @param workspace_url The workspace URL of the databricks instance
#'
#' @return NULL
#'
#' @family Azure methods
#' @export
update_databricks_token <- function(workspace_url) {
  db_token <- AzureAuth::get_azure_token(
    # Default resource ID for Azure Databricks
    resource = "2ff814a6-3304-4ab8-85cb-cd0e6f879c1d",
    tenant = "common",
    # App ID for Excel for PowerQuery
    app = "a672d62c-fc7b-4e81-a576-e60dc46e951d",
    # Ensure that token has MFA claim for usage outside of corporate network
    token_args = list(claims = '{"access_token":{"amr":{"values":["mfa"]}}}'),
    use_cache = FALSE
  )
  db_token$refresh()

  if (!("mfa" %in% AzureAuth::decode_jwt(db_token)$payload$amr)) {
    stop("MFA was not used when authenticating! ",
          "Try disconnecting from the corporate network and calling again.")
  }

  user_details <- az_authenticated_api_query(
    "GET",
    paste0(workspace_url,"/api/2.0/preview/scim/v2/Me"),
    db_token$credentials$access_token
  )

  username <- user_details$userName

  initialise_scope <- db_secret_scopes_api(
    operation = "create",
    scope_name = username,
    workspace_url = workspace_url,
    access_token = db_token$credentials$access_token
  )

  # Convert token into bytes
  token_as_bytes <- serialize(db_token, NULL)

  upload_token <- db_secrets_api(operation = "put",
                                workspace_url = workspace_url,
                                access_token = db_token$credentials$access_token,
                                scope_name = username,
                                secret_name = "azure_token",
                                secret_value = token_as_bytes,
                                bytestring = TRUE)
  if (upload_token$status_code == 200) {
    "Secret updated successfully!"
  } else {
    "Error updating secret!"
  }
}
