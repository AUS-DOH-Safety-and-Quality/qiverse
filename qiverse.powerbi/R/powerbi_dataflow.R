get_dataflow_metadata <- function(workspace_name, dataflow_name, access_token,
                                  verbose = TRUE) {
  if (interactive() && isTRUE(verbose)) {
    message("Fetching dataflow metadata...")
  }

  # Using the cluster URL instead of the generic API redirect (api.powerbi.com)
  # allows for additional API endpoints
  cluster_url <- get_cluster_url(access_token)

  # We need the unique identifier for the dataflow of interest, but we only
  # have its name. As such, we request the metadata of all dataflows that we
  # have access to and filter the results
  all_dataflows <- httr::GET(url = paste0(cluster_url, "/metadata/v201901/gallery/dataflows"),
                             config = get_auth_header(access_token),
                             httr::content_type_json()) |>
    httr::content()

  target_dataflow <- Filter(function(x) {
    if (is.null(x$cdsaModel$displayName)) {
      FALSE
    } else {
      x$workspaceName == workspace_name && x$cdsaModel$displayName == dataflow_name
    }
  }, all_dataflows)

  # Keep last edited target dataflow
  if (length(target_dataflow) > 1) {
    # Find max last edited date
    last_edit_times <- vapply(target_dataflow, function(x) {
      t <- x$cdsaModel$lastEditedTimeUTC
      if (is.null(t)) NA_character_ else t
    }, character(1))
    max_last_edit_date <- max(last_edit_times, na.rm = TRUE)
    # Choose last edited date dataflow
    target_dataflow <- Filter(function(x) {
      t <- x$cdsaModel$lastEditedTimeUTC
      !is.null(t) && identical(t, max_last_edit_date)
    }, target_dataflow)
  }
  target_dataflow[[1]]
}

get_table_metadata <- function(dataflow_id, table_name, access_token) {
  # Now we can request detailed metadata for the dataflow, including column names
  # and types, as well the storage location of the actual CSV file
  all_tables <- httr::GET(url = paste0(get_cluster_url(access_token), "/metadata/v201606/cdsa/dataflows/", dataflow_id, "/contentandcache"),
                          config = get_auth_header(access_token),
                          httr::content_type_json()) |>
    httr::content()

  rtn <- Filter(function(x) {
    !is.null(x$name) && identical(x$name, table_name)
  }, all_tables$content$entities)[[1]]
  # Locale info (e.g. en-GB) is stored in the content object
  # but not in the table object
  rtn$locale <- all_tables$content$culture
  rtn
}

get_sas_key <- function(dataflow_id, table_name, access_token) {
  # While we have the URL for the table, we don't have 'permission' to
  # download it using our current access token. Instead, we need to use the
  # access token to request a Shared Access Signature (SAS). This SAS summarises
  # the extent of data that we are allowed to access, and the length of time
  # that it is valid for use.
  sas_query <- httr::POST(
    url = paste0(get_cluster_url(access_token), "/metadata/v201606/cdsa/dataflows/", dataflow_id, "/storageAccess"),
    body = jsonlite::toJSON(
      list("TokenLifetimeInMinutes" = 360,
           "Permissions" = "Read",
           "EntityName" = table_name),
      auto_unbox = TRUE
    ),
    config = get_auth_header(access_token),
    httr::content_type_json()
  ) |>
    httr::content()

  sas_query$accessDetails[[1]]$blobContainerSas
}

#' Download PowerBI Dataflow Table
#'
#' This function provides the ability to download tables from within PowerBI
#' dataflow themselves, rather than requiring that they first be exposed via a
#' PowerBI dataset.
#'
#' @param workspace_name The PowerBI workspace name.
#' @param dataflow_name The name of the PowerBI Dataflow within the workspace.
#' @param table_name The name of the table within the PowerBI Dataflow to be
#' accessed.
#' @param access_token The token generated with the correct PowerBI Dataflow
#' permissions. Use get_az_tk('pbi_df') to create this token.
#' @param destfile Optional output filepath to save raw dataflow CSV file to.
#' If `NULL` (default) the CSV is read into an R `data.frame`. If non-null,
#' the CSV file is downloaded to specified filepath and the function has no return.
#' @param verbose Whether to print status messages while the function is
#' running. Default is TRUE.
#'
#' @return A data.frame of the PowerBI table from the PowerBI dataflow if
#' `destfile = NULL`, otherwise none
#' @export
#' @examples
#'  \dontrun{
#' # Create PowerBI Dataflow azure token
#' tk <- get_az_tk('pbi_df')
#'
#' # Load AD dummy file from PowerBI dataflow
#' ad_dummy <- download_dataflow_table(
#'   workspace_name = "My Workspace Name",
#'   dataflow_name = "My Dataflow Name",
#'   table_name = "My Table Name",
#'   access_token = tk$credentials$access_token
#' )
#'
#' # Download AD dummy file from PowerBI dataflow to local .csv file
#' download_dataflow_table(
#'   workspace_name = "My Workspace Name",
#'   dataflow_name = "My Dataflow Name",
#'   table_name = "My Table Name",
#'   access_token = tk$credentials$access_token,
#'   destfile = "my_table_name.csv"
#' )
#'}
download_dataflow_table <- function(workspace_name, dataflow_name,
                                    table_name, access_token,
                                    destfile = NULL,
                                    verbose = TRUE) {
  target_dataflow <- get_dataflow_metadata(workspace_name, dataflow_name,
                                           access_token, verbose)
  # Extract
  dataflow_id <- target_dataflow$cdsaModel$objectId

  target_table <- get_table_metadata(dataflow_id, table_name, access_token)

  # All our dataflows are either en-AU, en-US, or en-GB
  #  but add warning message in case this changes
  if (!(target_table$locale %in% c("en-AU", "en-US", "en-GB"))) {
    warning(
      "The dataflow's locale is: ", target_table$locale,
      ", for which date-time parsing has not been specifically implemented.",
      " Please open an issue at: https://github.com/AUS-DOH-Safety-and-Quality/qiverse",
      call. = FALSE
    )
  }

  # Extract the column names and types
  table_colnames <- vapply(target_table$attributes, function(x) x[["name"]], character(1))
  type_by_name <- stats::setNames(
    vapply(target_table$attributes, function(x) x[["dataType"]], character(1)),
    table_colnames
  )

  sas_key <- get_sas_key(dataflow_id, table_name, access_token)

  if (interactive() && isTRUE(verbose)) {
    message("Downloading dataflow table...")
  }

  # Now we can simply append the generated SAS to the blob storage download URL
  # and download the CSV into R.
  csv_url <- paste0(target_table$partitions[[1]]$location, "&", sas_key)
  con <- url(csv_url, open = "rt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  if (is.null(destfile)) {
    .pbi_read_csv_base(
      con,
      col_names = table_colnames,
      type_by_name = type_by_name,
      locale = target_table$locale
    )
  } else {
    csv_text <- c(
      paste0(table_colnames, collapse = ","),
      readLines(con)
    )
    cat(csv_text, file = destfile, sep = "\n", append = FALSE)
    invisible(NULL)
  }
}

#' Refresh PowerBI Dataflow
#'
#' This function provides the ability to refresh PowerBI dataflows, via an R
#' wrapper using the PowerBI REST API.
#'
#' @param workspace_name The PowerBI workspace name.
#' @param dataflow_name The name of the PowerBI Dataflow within the workspace.
#' @param access_token The token generated with the correct PowerBI Dataflow
#' permissions. Use get_az_tk('pbi_df') to create this token.
#' @param verbose Whether to print status messages while the function is
#' running. Default is TRUE.
#'
#' @return A response object from the POST request.
#' @export
#' @examples
#'  \dontrun{
#' # Create PowerBI Dataflow azure token
#' tk <- get_az_tk('pbi_df')
#'
#' # Load AD dummy file from PowerBI dataflow
#' refresh_result <- refresh_dataflow(
#'   workspace_name = "My Workspace Name",
#'   dataflow_name = "My Dataflow Name",
#'   access_token = tk$credentials$access_token
#' )
#'}
refresh_dataflow <- function(
  workspace_name,
  dataflow_name,
  access_token,
  verbose = TRUE
) {

  # Get dataflow metadata using utility function
  target_dataflow <- get_dataflow_metadata(workspace_name, dataflow_name,
                                           access_token, FALSE)
  # Extract ids
  dataflow_id <- target_dataflow$cdsaModel$objectId
  workspace_id <- target_dataflow$workspaceObjectId

  # Trigger POST refresh
  refresh_response <- httr::POST(
    url = paste0(
      "https://api.powerbi.com/v1.0/myorg/groups/",
      workspace_id,
      "/dataflows/",
      dataflow_id,
      "/refreshes"
    ),
    config = get_auth_header(access_token),
    httr::content_type_json(),
    body = "{}"
  )

  # Status code handling
  if (verbose == TRUE) {
    if (refresh_response$status_code == 200) {
      message(
        sprintf(
          "Dataflow '%s' in workspace '%s' is refreshing...",
          dataflow_name,
          workspace_name
        )
      )
    } else {
      # Print errors
      message(
        sprintf(
          "Status code %s. See response content (httr::content()) for error details...",
          refresh_response$status_code
        )
      )
    }
  }

  # Return response
  return(refresh_response)
}

#' Update PowerBI Dataflow compute engine behaviour
#'
#' This function provides the ability to change the compute engine behaviour for
#' a selected PowerBI dataflow, via an R wrapper using the PowerBI REST API.
#'
#' You have to be the owner of the PowerBI dataflow to use this function,
#' otherwise there will be an 401 unauthorised error.
#'
#' @param workspace_name The PowerBI workspace name.
#' @param dataflow_name The name of the PowerBI Dataflow within the workspace.
#' @param compute_engine The compute engine behaviour setting to be
#' applied. Must be one of "computeOptimized", "computeOn" or "computeDisabled".
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
#' # Load AD dummy file from PowerBI dataflow
#' response <- update_dataflow_compute_engine(
#'   workspace_name = "My Workspace Name",
#'   dataflow_name = "My Dataflow Name",
#'   compute_engine = "computeOptimized",
#'   access_token = tk$credentials$access_token
#' )
#'}
update_dataflow_compute_engine <- function(
    workspace_name,
    dataflow_name,
    compute_engine,
    access_token
) {
  # Check if compute engine behaviour is a valid option
  valid_compute_engine <- c("computeOptimized", "computeOn", "computeDisabled")
  if (!(compute_engine %in% valid_compute_engine)) {
    stop(paste0(
      "compute_engine setting of '", compute_engine, "' ",
      "is not recognised. Must be one of: ",
      paste0(valid_compute_engine, collapse = ", "), ". \n",
      "See https://learn.microsoft.com/en-us/rest/api/power-bi/dataflows/update-dataflow#request-body")
    )
  }

  # Get dataflow metadata using utility function
  target_dataflow <- get_dataflow_metadata(workspace_name, dataflow_name,
                                           access_token, FALSE)
  # Extract ids
  dataflow_id <- target_dataflow$cdsaModel$objectId
  workspace_id <- target_dataflow$workspaceObjectId

  # Update dataflow compute engine behaviour using PATCH
  update_response <- httr::PATCH(
    url = paste0(
      "https://api.powerbi.com/v1.0/myorg/groups/",
      workspace_id,
      "/dataflows/",
      dataflow_id
    ),
    config = get_auth_header(access_token),
    httr::content_type_json(),
    body = sprintf('{
      "computeEngineBehavior": "%s"
    }', compute_engine
    )
  )

  # Return response
  return(update_response)
}
