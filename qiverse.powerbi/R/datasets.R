#' Request metadata for all datasets in specified workspace
#'
#' @param workspace Name of the workspace containing datasets
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_ds') to create this token.
#'
#' @return DataFrame containing the names, GUIDs, and descriptions for all datasets in workspace
#' @export
list_datasets <- function(workspace, access_token) {
  workspace_metadata <- list_workspaces(access_token)
  if (!(workspace %in% workspace_metadata$Workspace)) {
    stop("No workspace called: ", workspace, " in tenant!", call. = FALSE)
  }

  base_url <- "https://api.powerbi.com/v1.0/myorg/groups/"
  workspace_id <- workspace_metadata[workspace_metadata$Workspace == workspace,]$WorkspaceId
  workspace_request <-  httr::GET(url = paste0(base_url, workspace_id, "/datasets"),
                                  config = get_auth_header(access_token),
                                  httr::content_type_json())

  if (workspace_request$status_code != 200) {
    stop("API request returned status code: ", workspace_request$status_code, "!",
         call. = TRUE)
  }

  metadata_content <- httr::content(workspace_request)$value
  
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
  if (!("id" %in% colnames(content_to_dataframe))) {
    content_to_dataframe$name <- ""
  }

  content_to_dataframe <- content_to_dataframe[,c("Workspace", "WorkspaceId", "name", "id", "configuredBy")]
  names(content_to_dataframe) <- c("Workspace", "WorkspaceId", "Dataset", "DatasetId", "DatasetOwner")
  content_to_dataframe
}

#' Download a specified dataset table into R using either the XMLA or REST API
#' endpoints. This is a convenience wrapper around the `execute_xmla_query()`
#' and `execute_rest_query()` functions.
#'
#' @param workspace Name of the workspace containing dataflow
#' @param dataset Name of the dataset containing table
#' @param table Name of the table to download
#' @param method The API to use for downloading the table. Valid values are "XMLA" (the default) and "REST"
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_ds') to create this token.
#'
#' @return DataFrame containing downloaded table
#' @export
download_dataset_table <- function(workspace, dataset, table,
                              method = "XMLA",
                              access_token) {
  query <- paste0("EVALUATE('", table, "')")
  if (method == "XMLA") {
    table_query <- execute_xmla_query(workspace, dataset, query, access_token)
  } else if (method == "REST") {
    table_query <- execute_rest_query(workspace, dataset, query, access_token)
  } else {
    stop("Invalid method: ", method, "! Valid values are \"XMLA\" or \"REST\".",
         call. = FALSE)
  }
  names(table_query) <- gsub(paste0(table, "\\[|\\]"), "", names(table_query))
  table_query
}

#' Execute a DAX query against a specified PowerBI Dataset using the REST API
#' endpoint.
#'
#' @param workspace Name of the workspace containing dataflow
#' @param dataset Name of the dataset to execute query against
#' @param query DAX query to execute
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_ds') to create this token.
#'
#' @return DataFrame containing results of query
#' @export
execute_rest_query <- function(workspace, dataset, query, access_token) {
  dataset_metadata <- list_datasets(workspace, access_token)
  if (!(dataset %in% dataset_metadata$Dataset)) {
    stop("No dataset called: ", dataset, "in workspace: ", workspace, "!", call. = FALSE)
  }

  target_dataset <- dataset_metadata[dataset_metadata$Dataset == dataset, ]
  dataset_id <- target_dataset$DatasetId

  execute_rest_query_impl(dataset_id, query, access_token)
}

execute_rest_query_impl <- function(dataset_id, query, access_token) {
  query_url <- paste0("https://api.powerbi.com/v1.0/myorg/datasets/", dataset_id, "/executeQueries")
  
  rest_query <- httr::POST(url = query_url,
                           body = construct_rest_query(query),
                           config = get_auth_header(access_token),
                           httr::content_type_json())
  
  query_content <- httr::content(rest_query)
  
  if ("error" %in% names(query_content)) {
    stop("Query returned error: ", query_content$error$pbi.error$details[[1]]$detail$value,
         call. = FALSE)
  }
  
  if (("error" %in% names(query_content$results[[1]]))) {
    error_code <- query_content$results[[1]]$error$code
    if (error_code == "DaxByteCountNotSupported") {
      stop("The query result is too large for the REST API! Try the XMLA API instead.\n",
           "See: https://learn.microsoft.com/en-us/rest/api/power-bi/datasets/execute-queries#limitations",
           call. = FALSE)
    }
  }
  
  output <- query_content$results[[1]]$tables[[1]]$rows |>
    dplyr::bind_rows()
  
  names(output) <- gsub("\\[|\\]", "", names(output))
  
  return(output)
}


#' Execute a DAX query against a specified PowerBI Dataset using the XMLA API
#' endpoint.
#'
#' @param workspace Name of the workspace containing dataflow
#' @param dataset Name of the dataset to execute query against
#' @param query DAX query to execute
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_df') or get_az_tk('pbi_df') to create
#' this token.
#'
#' @return DataFrame containing results of query
#' @export
execute_xmla_query <- function(workspace, dataset, query, access_token) {
  auth_header <- get_auth_header(access_token)
  
  # Lookup GUIDs for the Workspace and overall capacity
  all_workspaces <- httr::GET("https://api.powerbi.com/powerbi/databases/v201606/workspaces",
                              config = auth_header,
                              httr::content_type_json()) |>
    httr::content()
  target_workspace <- purrr::keep(all_workspaces, \(x) x$name == workspace)[[1]]
  workspace_id <- target_workspace$id
  capacity_id <- target_workspace$capacityObjectId
  
  cluster_details <- httr::POST("https://australiasoutheast.pbidedicated.windows.net/webapi/clusterResolve",
                                config = auth_header,
                                body = jsonlite::toJSON(list(
                                  databaseName = NA,
                                  premiumPublicXmlaEndpoint = TRUE,
                                  serverName = capacity_id
                                ), auto_unbox = TRUE),
                                httr::content_type_json()) |>
    httr::content()
  
  astoken <- httr::POST("https://api.powerbi.com/metadata/v201606/generateastoken",
                        config = auth_header,
                        body = jsonlite::toJSON(list(
                          applyAuxiliaryPermission = FALSE,
                          auxiliaryPermissionOwner = NA,
                          capacityObjectId = capacity_id,
                          datasetName = dataset,
                          intendedUsage = 0,
                          sourceCapacityObjectId = NA,
                          workspaceObjectId = workspace_id
                        ), auto_unbox = TRUE),
                        httr::content_type_json()) |>
    httr::content()
  
  xmla_request <-
    httr::POST(
      url = paste0('https://', cluster_details$clusterFQDN,'/webapi/xmla'),
      config = httr::add_headers(Authorization = paste("MwcToken", astoken$Token)),
      body = construct_xmla_query(dataset, query),
      httr::progress(),
      httr::add_headers(.headers = c(
        "x-ms-xmlacaps-negotiation-flags" = "0,0,0,1,1",
        "Content-Type" = "text/xml",
        "x-ms-xmlaserver" = cluster_details$coreServerName
      ))
    )
  request_results <- httr::content(xmla_request, encoding = "UTF-8")
  rowset_to_df(request_results)
}
