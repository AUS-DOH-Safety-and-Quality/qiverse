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
#' @param verbose Whether to print status messages while the function is
#' running. Default is TRUE.
#'
#' @return A data.frame of the PowerBI table from the PowerBI dataflow.
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
#'}
download_dataflow_table <- function(workspace_name, dataflow_name,
                                    table_name, access_token,
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

  .pbi_parse_logical <- function(x) {
    x <- trimws(tolower(as.character(x)))
    ifelse(
      x %in% c("true", "t", "1", "yes", "y"),
      TRUE,
      ifelse(x %in% c("false", "f", "0", "no", "n"), FALSE, NA)
    )
  }

  .pbi_parse_double <- function(x) {
    # PowerBI exports commonly include thousand separators for en-* locales.
    x <- gsub(",", "", as.character(x), fixed = TRUE)
    suppressWarnings(as.numeric(x))
  }

  .pbi_parse_int64 <- function(x) {
    # Base R has no 64-bit integer type; use numeric.
    .pbi_parse_double(x)
  }

  .pbi_read_csv_base <- function(csv_url, col_names, type_by_name, locale) {
    date_fmt <- "%d/%m/%Y"
    datetime_fmt <- "%d/%m/%Y %H:%M:%S %p"

    if (identical(locale, "en-GB")) {
      datetime_fmt <- "%d/%m/%Y %H:%M:%S"
    }

    if (identical(locale, "en-US")) {
      date_fmt <- "%m/%d/%Y"
      datetime_fmt <- "%m/%d/%Y %H:%M:%S %p"
    }

    con <- url(csv_url, open = "rt", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)

    df <- utils::read.csv(
      con,
      header = FALSE,
      col.names = col_names,
      colClasses = rep("character", length(col_names)),
      stringsAsFactors = FALSE,
      check.names = FALSE,
      comment.char = "",
      na.strings = c("", "NA", "NaN")
    )

    for (nm in col_names) {
      pbi_type <- type_by_name[[nm]]
      if (is.null(pbi_type)) {
        next
      }

      df[[nm]] <- switch(
        pbi_type,
        "string" = as.character(df[[nm]]),
        "double" = .pbi_parse_double(df[[nm]]),
        "int64" = .pbi_parse_int64(df[[nm]]),
        "boolean" = .pbi_parse_logical(df[[nm]]),
        "date" = as.Date(df[[nm]], format = date_fmt),
        "dateTime" = as.POSIXct(df[[nm]], format = datetime_fmt, tz = "UTC"),
        "time" = as.difftime(df[[nm]], format = "%H:%M:%S", units = "secs"),
        df[[nm]]
      )
    }

    df
  }

  # Extract the column names and types
  table_colnames <- vapply(target_table$attributes, function(x) x[["name"]], character(1))
  type_by_name <- setNames(
    vapply(target_table$attributes, function(x) x[["dataType"]], character(1)),
    table_colnames
  )

  sas_key <- get_sas_key(dataflow_id, table_name, access_token)


  if (interactive() && isTRUE(verbose)) {
    message("Downloading dataflow table...")
  }

  # Now we can simply append the generated SAS to the blob storage download URL
  # and download the CSV into R.
  .pbi_read_csv_base(
    paste0(target_table$partitions[[1]]$location, "&", sas_key),
    col_names = table_colnames,
    type_by_name = type_by_name,
    locale = target_table$locale
  )
}
