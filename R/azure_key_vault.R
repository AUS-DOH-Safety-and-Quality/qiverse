#' Initialise an AzureKeyVault object for interacting with stored secrets. If
#' an AzureAuth access token object is not provided then the authentication
#' process is also initiated
#'
#' @param vault_name The name of the target Azure Key Vault
#' @param token_object A previously-generated AzureAuth token object
#' created for the Key Vault resource
#'
#' @return An AzureKeyVault::key_vault() object
#'
#' @family Key Vault methods
#' @examples -
.init_key_vault <- function(vault_name, token_object) {
  if (is.null(token_object)) {
    token_object <- rSQuIS::get_az_tk("key_vault")
  }
  AzureKeyVault::key_vault(url = vault_name, token = token_object)
}

#' A generic implementation function for interacting with Azure Key Vault
#' secrets. Handles the connection and authentication (if necessary) to the
#' Key Vault service
#'
#' @param vault_name The name of the target Azure Key Vault
#' @param operation The name of the operation to be applied, see the
#' AzureKeyVault::secrets documentation for a full list of possible operations
#' @param token_object (optional) A previously-generated AzureAuth token object
#' created for the Key Vault resource
#' @param ... Any arguments to be passed to the Key Vault operation, see the
#' AzureKeyVault::secrets documentation for the possible arguments for each
#' operation
#'
#' @return An AzureKeyVault::key_vault() object
#'
#' @family Key Vault methods
#' @examples -
.secrets_operation <- function(vault_name, operation, token_object, ...) {
  kv <- .init_key_vault(vault_name = vault_name, token_object = token_object)
  kv$secrets[[operation]](...)
}

#' List the names of secrets stored in a given Azure Key Vault
#'
#' @param vault_name The name of the target Azure Key Vault
#' @param token_object (optional) A previously-generated AzureAuth token object
#' created for the Key Vault resource
#'
#' @return A list of the (names of) secrets stored in the given Key Vault
#'
#' @family Key Vault methods
#' @export
#' @examples -
kv_list_secrets <- function(vault_name, token_object = NULL) {
  .secrets_operation(vault_name = vault_name, operation = "list",
                      token_object = token_object)
}

#' Retrieve the value of a specified secret from a given Key Vault
#'
#' @param vault_name The name of the target Azure Key Vault
#' @param secret_name The name of the target secret to retrieve
#' @param token_object (optional) A previously-generated AzureAuth token object
#' created for the Key Vault resource
#'
#' @return The value of a secret stored in the given Key Vault
#'
#' @family Key Vault methods
#' @export
#' @examples -
kv_get_secret <- function(vault_name, secret_name, token_object = NULL) {
  secret <- .secrets_operation(vault_name = vault_name, operation = "get",
                                token_object = token_object, name = secret_name)
  # The secret is returned as "hidden" and won't print, but simply need to
  # remove the class attribute to change this
  attr(secret$value, "class") <- NULL
  secret$value
}

#' Create or overwrite a secret in an Azure Key Vault
#'
#' @param vault_name The name of the target Azure Key Vault
#' @param secret_name The name of the target secret to update
#' @param secret_value The value to set the target secret to
#' @param bytestring Whether the secret value is a string of bytes, for storing
#' serialised objects
#' @param token_object (optional) A previously-generated AzureAuth token object
#' created for the Key Vault resource
#'
#' @return NULL
#'
#' @family Key Vault methods
#' @export
#' @examples -
kv_write_secret <- function(vault_name, secret_name, secret_value,
                            bytestring = FALSE, token_object = NULL) {
  content_type <- NULL
  if (bytestring) {
    content_type <- "application/octet-stream"
  }
  .secrets_operation(vault_name = vault_name,
                     operation = "create",
                     token_object = token_object,
                     name = secret_name,
                     value = secret_value,
                     content_type = content_type)
}

#' Delete a specified secret from a given Key Vault
#'
#' @param vault_name The name of the target Azure Key Vault
#' @param secret_name The name of the target secret to delete
#' @param confirm Whether to display an interactive prompt to confirm deletion
#' of stored secret
#' @param token_object (optional) A previously-generated AzureAuth token object
#' created for the Key Vault resource
#'
#' @return NULL
#'
#' @family Key Vault methods
#' @export
#' @examples -
kv_delete_secret <- function(vault_name, secret_name, confirm = TRUE,
                              token_object = NULL) {
  .secrets_operation(vault_name = vault_name,
                     operation = "delete",
                     token_object = token_object,
                     name = secret_name,
                     confirm = confirm)
}
