#' Generate an Azure authentication token
#'
#' @param token_type The type of token to be generated.
#' This can be either: "pbi_df" for PowerBI Dataflows, "pbi_ds"
#' for PowerBI datasets, "sp" for SharePoint, "databricks" for the Databricks
#' API, "key_vault" for Azure Key Vault
#' @param tenant_id Your organisation's tenant identifier
#' @param app_id_pbi_df The application identifier with access to PowerBI
#' dataflows
#' @param app_id_pbi_ds The application identifier with access to PowerBI
#' datasets
#' @param graph_api_shp The Graph API created for access to SharePoint
#' @param app_id_shp The application identifier with access to SharePoint
#' @param cli_sec_shp The client secret with access to SharePoint
#' @param ... Additional arguments to be passed to [AzureAuth::get_azure_token()], #nolint
#' such as 'use_cache' or 'auth_type'
#'
#' @return An Azure token to authenticate connections.
#' @import AzureAuth
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
    ...
) {
  resource <- list(
    "pbi_df" = "https://analysis.windows.net/powerbi/api",
    "pbi_ds" = "https://analysis.windows.net/powerbi/api",
    "sp" = "https://graph.microsoft.com",
    "sf" = "https://analysis.windows.net/powerbi/connector/Snowflake",
    "databricks" = "2ff814a6-3304-4ab8-85cb-cd0e6f879c1d",
    "key_vault" = "https://vault.azure.net"
  )

  app <- list(
    "pbi_df" = app_id_pbi_df,
    "pbi_ds" = app_id_pbi_ds,
    "sp" = app_id_shp,
    "sf" = app_id_pbi_df,
    "databricks" = app_id_pbi_df,
    "key_vault" = app_id_pbi_df
  )

  # Sharepoint requires a passthrough of token
  if (token_type == "sp") {
    token <- AzureAuth::get_azure_token(
      resource = resource[["pbi_df"]],
      tenant = tenant_id,
      app = app[["pbi_df"]]
    )

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
    token <- AzureAuth::get_azure_token(
      resource = resource[[token_type]],
      tenant = tenant_id,
      app = app[[token_type]],
      ...
    )
    token$refresh()
  }

  # Output token ####
  return(token)
}
