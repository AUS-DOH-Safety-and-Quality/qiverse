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

  content_to_dataframe <- .bind_rows_base(lapply(metadata_content, function(metadata) {
    do.call(data.frame, Filter(function(x) length(x) > 0, metadata))
  }))
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

#' Execute a DAX query against a specified PowerBI Dataset using the REST
#' Arrow API endpoint.
#'
#' This endpoint has no restrictions on the size of the result and will
#' transfer data in a compressed format (Apache Arrow), and should be
#' preferred.
#'
#' @param workspace Name of the workspace containing dataflow
#' @param dataset Name of the dataset to execute query against
#' @param query DAX query to execute
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_ds') to create this token.
#'
#' @return DataFrame containing results of query
#' @export
execute_arrow_query <- function(workspace, dataset, query, access_token) {
  if (!("arrow" %in% utils::installed.packages()[,"Package"])) {
    stop("This function requires the 'arrow' package, but it is not installed!")
  }
  dataset_metadata <- list_datasets(workspace, access_token)
  if (!(dataset %in% dataset_metadata$Dataset)) {
    stop("No dataset called: ", dataset, "in workspace: ", workspace, "!", call. = FALSE)
  }

  target_dataset <- dataset_metadata[dataset_metadata$Dataset == dataset, ]
  workspace_id <- target_dataset$WorkspaceId
  dataset_id <- target_dataset$DatasetId

  execute_arrow_query_impl(workspace_id, dataset_id, query, access_token)
}

execute_arrow_query_impl <- function(workspace_id, dataset_id, query, access_token) {
  query_url <- paste0("https://api.powerbi.com/v1.0/myorg/groups/", workspace_id, "/datasets/", dataset_id, "/executeDaxQueries")

  arrow_query <- httr::POST(
    url = query_url,
    body = jsonlite::toJSON(list(query = query), auto_unbox = TRUE),
    config = httr::add_headers(Authorization = paste("Bearer", access_token)),
    httr::content_type_json()
  ) |>
    httr::content(as = "raw") |>
    arrow::read_ipc_stream(as_data_frame = FALSE)

  # PBI returns data as 'dictionary' types (e.g., dictionary<double>), which get converted to 'factor' types by R
  # Need to extract the target type and update the schema before converting to R
  schema_types <- strsplit(utils::capture.output(arrow_query$schema)[-1], ":")
  new_schema <- lapply(schema_types, \(comps) {
    type <- gsub(".*values=([a-zA-Z0-9]+)(\\[[a-z]+\\])?,.*", "\\1", comps[2])
    # The 'double' alias isn't exported by the arrow package, so replace with 'float64'
    type <- gsub("double", "float64", type)
    # Extract and call the appropriate 'type' function (e.g., `arrow::float64()`)
    getExportedValue("arrow", type)()
  }) |>
    stats::setNames(sapply(schema_types, \(x) trimws(x[1])))

  arrow_query$cast(target_schema = arrow::schema(new_schema))$to_data_frame()
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

  result_rows <- lapply(query_content$results[[1]]$tables[[1]]$rows, \(rowset) {
    rowset[sapply(rowset, is.null)] <- NA
    as.data.frame(rowset)
  })

  output <- do.call(rbind, result_rows)

  names(output) <- gsub("(.*)?\\[|\\]", "", names(query_content$results[[1]]$tables[[1]]$rows[[1]]))

  return(output)
}

execute_xmla_query_impl <- function(cluster_url, xmla_server, dataset, query,
                                    astoken) {
  xmla_request <-
    httr::POST(
      url = paste0('https://', cluster_url,'/webapi/xmla'),
      config = httr::add_headers(Authorization = paste("MwcToken", astoken)),
      body = construct_xmla_query(dataset, query),
      httr::progress(),
      httr::add_headers(.headers = c(
        "x-ms-xmlacaps-negotiation-flags" = "0,0,0,1,1",
        "Content-Type" = "text/xml",
        "x-ms-xmlaserver" = xmla_server
      ))
    )
  rtn <- httr::content(xmla_request, encoding = "UTF-8", as = "raw") |>
          xml2::read_xml(options = c("NOBLANKS", "HUGE")) |>
          rowset_to_df()
  names(rtn) <- gsub("(.*)?\\[|\\]", "", names(rtn))
  rtn
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

  target_workspace <- Filter(function(x) {
    !is.null(x$name) && identical(x$name, workspace)
  }, all_workspaces)[[1]]
  workspace_id <- target_workspace$id
  capacity_id <- target_workspace$capacityObjectId
  region_name <- gsub("pbidedicated://(.*).pbidedicated.windows.net/.*", "\\1", target_workspace$capacityUri)

  cluster_details <- get_pbi_cluster_details(region_name, capacity_id, access_token)

  astoken <- get_dataset_access_token(capacity_id, workspace_id, access_token)

  execute_xmla_query_impl(cluster_details$clusterFQDN,
                          cluster_details$coreServerName,
                          dataset,
                          query,
                          astoken)
}

#' Update PowerBI Semantic Model (Dataset) storage mode
#'
#' This function provides the ability to change the storage mode for a selected
#' PowerBI dataset, via an R wrapper using the PowerBI REST API.
#'
#' You do not have to be the owner of the semantic model to use this function.
#'
#' @param workspace_name The PowerBI workspace name.
#' @param dataset_name The name of the PowerBI Dataflow within the workspace.
#' @param storage_mode The compute engine behaviour setting to be
#' applied. Must be one of "Abf" (small semantic model format) or "PremiumFiles"
#'  (large semantic model format).
#' @param access_token The token generated with the correct PowerBI Dataflow
#' permissions. Use get_az_tk('pbi_df') to create this token.
#'
#' @return A response object from the PATCH request.
#' @export
#' @examples
#'  \dontrun{
#' # Create PowerBI Dataflow azure token
#' tk <- get_az_tk('pbi_df')
#'
#' response <- update_dataset_storage_mode(
#'   workspace_name = "My Workspace Name",
#'   dataset_name = "My Dataset Name",
#'   storage_mode = "PremiumFiles",
#'   access_token = tk$credentials$access_token
#' )
#'}
update_dataset_storage_mode <- function(
    workspace_name,
    dataset_name,
    storage_mode,
    access_token
) {
  # Get dataset metadata
  dataset_metadata <- list_datasets(workspace_name, access_token)
  if (!(dataset_name %in% dataset_metadata$Dataset)) {
    stop("No dataset called: ", dataset_name, "in workspace: ", workspace_name, "!", call. = FALSE)
  }

  target_dataset <- dataset_metadata[dataset_metadata$Dataset == dataset_name, ]
  dataset_id <- target_dataset$DatasetId

  # Check if compute engine behaviour is a valid option
  valid_storage_mode <- c("PremiumFiles", "Abf")
  if (!(storage_mode %in% valid_storage_mode)) {
    stop(paste0(
      "target_storage_mode setting of '", storage_mode, "' ",
      "is not recognised. Must be one of: ",
      paste0(valid_storage_mode, collapse = ", "), ". \n",
      "See https://learn.microsoft.com/en-us/rest/api/power-bi/datasets/update-dataset#examples")
    )
  }

  # Update dataset dataset storage mode using PATCH
  update_response <- httr::PATCH(
    url = paste0(
      "https://api.powerbi.com/v1.0/myorg/datasets/",
      dataset_id
    ),
    config = get_auth_header(access_token),
    httr::content_type_json(),
    body = sprintf('{
      "targetStorageMode": "%s"
    }', storage_mode
    )
  )

  # Return response
  return(update_response)
}
