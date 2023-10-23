#' Generate an Azure authentication token
#'
#' @param token_type The type of token to be generated.
#' This can be either: "pbi_df" for PowerBI Dataflows, "pbi_ds"
#' for PowerBI datasets, "sp" for SharePoint, "sf" for Snowflake,
#' "databricks" for the Databricks API, "key_vault" for Azure Key Vault
#' @param tenant_id Your organisation's tenant identifier
#' @param app_id_pbi_df The application identifier with access to PowerBI
#' dataflows
#' @param app_id_pbi_ds The application identifier with access to PowerBI
#' datasets
#' @param graph_api_shp The Graph API created for access to SharePoint
#' @param app_id_shp The application identifier with access to SharePoint
#' @param cli_sec_shp The client secret with access to SharePoint
#' @param system_type The system where the package is being called from. Can be
#' either "local" if running on your local machine or virtual machine, or
#' "databricks" if running on a databricks instance. The "databricks" option
#' will pull the token from your secret created using
#' qiverse.azure::store_databricks_access_token
#' @param db_scope Optional parameter for the databricks scope to pull the
#' Azure access token from
#' @param ... Additional arguments to be passed to [AzureAuth::get_azure_token()], #nolint
#' such as 'use_cache' or 'auth_type'
#'
#' @return An Azure token to authenticate connections.
#' @export
#' @examples
#' \dontrun{
#' tk <- get_az_tk('pbi_df')
#' tk <- get_az_tk('pbi_ds')
#' tk <- get_az_tk('sp')
#' tk <- get_az_tk('pbi_df', auth_type = 'device_code')
#'}
get_az_tk <- function(
    token_type,
    tenant_id = Sys.getenv("az_tenant_id"),
    app_id_pbi_df = Sys.getenv("az_app_id_pbi_dataflow"),
    app_id_pbi_ds = Sys.getenv("az_app_id_pbi_dataset"),
    graph_api_shp = Sys.getenv("az_graph_api_sharepoint"),
    app_id_shp = Sys.getenv("az_app_id_sharepoint"),
    cli_sec_shp = Sys.getenv("az_cli_secret_id_sharepoint"),
    system_type = 'local',
    db_scope = '',
    ...
) {
  resource <- list(
    "pbi_df" = "https://analysis.windows.net/powerbi/api",
    "pbi_ds" = "https://analysis.windows.net/powerbi/api",
    "sp" = "https://graph.microsoft.com",
    "sf" = "https://analysis.windows.net/powerbi/connector/Snowflake",
    "key_vault" = "https://vault.azure.net"
  )

  app <- list(
    "pbi_df" = app_id_pbi_df,
    "pbi_ds" = app_id_pbi_ds,
    "sp" = app_id_shp,
    "sf" = app_id_pbi_df,
    "key_vault" = app_id_pbi_df
  )

  # Running on databricks requires extraction of secret
  if (system_type == "databricks") {
    # Extract the byte-string representation of the AzureAuth token object for the current user
    token_bytes <- dbutils.secrets.getBytes(
      scope = db_scope, key = "azure_token"
    )

    # Convert the byte-string to an actual R object
    token_db <- unserialize(token_bytes)

    # Update the token with PowerBI API
    token_db$resource <- "https://analysis.windows.net/powerbi/api"
  }

  # Sharepoint requires a passthrough of token
  if (token_type == "sp") {
    if (system_type == "local") {
      token <- AzureAuth::get_azure_token(
        resource = resource[["pbi_df"]],
        tenant = tenant_id,
        app = app[["pbi_df"]]
      )
    } else if (system_type == "databricks") {
      token <- token_db
    }

    # Specify the resource URI for the service principal
    # This is found under the 'Expose an API' menu
    token$resource <- graph_api_shp
    token$refresh()

    # Exchange service principal access token for Graph API access token
    token <- AzureAuth::get_azure_token(
      resource = "https://graph.microsoft.com",
      tenant = tenant_id,
      app = app[["sp"]],
      password = cli_sec_shp,
      on_behalf_of = token
    )
  } else {
    if (system_type == "local") {
      token <- AzureAuth::get_azure_token(
        resource = resource[[token_type]],
        tenant = tenant_id,
        app = app[[token_type]],
        ...
      )
    } else if (system_type == "databricks") {
      token <- token_db
    }
    token$refresh()
  }


  # Output token ####
  return(token)
}
