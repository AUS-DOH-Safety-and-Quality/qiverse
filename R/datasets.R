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

  content_to_dataframe <- purrr::map_dfr(metadata_content, \(metadata){
    do.call(data.frame, purrr::keep(metadata, \(x){length(x) > 0}))
  })
  content_to_dataframe$Workspace <- workspace
  content_to_dataframe$WorkspaceId <- workspace_id

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

  query_content$results[[1]]$tables[[1]]$rows |>
    purrr::map_dfr(~purrr::modify_if(.x, is.null, ~as.numeric(NA)))
}


#' Execute a DAX query against a specified PowerBI Dataset using the XMLA API
#' endpoint.
#'
#' @param workspace Name of the workspace containing dataflow
#' @param dataset Name of the dataset to execute query against
#' @param query DAX query to execute
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_df') to create this token.
#'
#' @return DataFrame containing results of query
#' @export
execute_xmla_query <- function(workspace, dataset, query, access_token) {
  auth_header <- get_auth_header(access_token)
  cluster_details <- httr::PUT(url = "https://api.powerbi.com/spglobalservice/GetOrInsertClusterUrisByTenantlocation",
                               config = auth_header,
                               httr::content_type_json()) |>
    httr::content()

  cluster_uri <- cluster_details$DynamicClusterUri

  all_datasets <- httr::GET(paste0(cluster_uri,"/metadata/v202303/gallery/sharedDatasets"),
                            config = auth_header,
                            httr::content_type_json()) |>
    httr::content()

  target_dataset <- purrr::keep(all_datasets, function(x) {
    x$workspaceName == workspace && x$model$displayName == dataset
  })[[1]]

  query_url <- paste0(cluster_uri, "/xmla?vs=", target_dataset$model$vsName,
                                    "&db=", target_dataset$model$dbName)

  xmla_request <-
    httr::POST(
      url = query_url,
      config = auth_header,
      body = construct_xmla_query(target_dataset$model$dbName, query),
      httr::progress(),
      httr::add_headers(.headers = c(
        "X-Transport-Caps-Negotiation-Flags" = "0,0,0,1,1",
        "Content-Type" = "text/xml"
      ))
    )
  request_results <- httr::content(xmla_request, encoding = "UTF-8")
  rowset_to_df(request_results)
}
