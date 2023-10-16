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
#' @examples -
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

  do.call(rSQuIS::az_authenticated_api_query, args)
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
#' @examples -
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
    if (operation == "create") {
      if (bytestring) {
        args$body$bytes_value <- secret_value
      } else {
        args$body$string_value <- secret_value
      }
    }
  }

  do.call(rSQuIS::az_authenticated_api_query, args)
}

#' Create an Azure authentication token and store it as a Databricks secret
#' when run on a databricks cluster
#'
#' @param url The workspace URL of the databricks instance
#' @param username The current user's username, for the secret scope
#'
#' @return NULL
#'
#' @family Azure methods
#' @export
#' @examples -
init_databricks_access_token <- function(url, username) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop(
      "Package \"callr\" must be installed to use this function.\n",
      call. = FALSE
    )
  }
  args <- list(
    username = username,
    url = url
  )
  token_function <- function(url, username) {
    token <- rSQuIS::get_az_tk("databricks", auth_type = "device_code")
    db_secret_scopes_api(
      operation = "create",
      scope_name = username,
      workspace_url = url,
      access_token = token$credentials$access_token)

    token_as_bytes <- serialize(token, NULL)
    db_secrets_api(operation = "put",
                    workspace_url = url,
                    access_token = token$credentials$access_token,
                    scope_name = username,
                    secret_value = token_as_bytes,
                    bytestring = TRUE)
  }
  bg_r_process <- callr::r_bg(token_function, args)

  # Wait until an output has been printed (the ID code)
  bg_r_process$poll_io(10000)

  # Return the printed output (ID code for authentication)
  message(bg_r_process$read_output())
}
