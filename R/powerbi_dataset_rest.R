# When downloading a dataset, the API needs to know the GUID of the workspace
# to download it from. This function takes a workspace name and returns the
# appropriate GUID to use in queries
.get_workspace_id <- function(workspace_name, access_token) {
  # Generic URL to query, the access token tells API what workspaces we have
  # access to
  url <- "https://api.powerbi.com/v1.0/myorg/groups"

  # Specify that the query should use the access token for authorisation
  headers <- httr::add_headers(Authorization = paste("Bearer", access_token))

  # Send query to API, returning a list of lists, where each list contains
  # details about a given workspace
  res <- httr::content(httr::GET(url = url, config = headers))

  # Extract the list that contains details about the workspace of interest
  workspace <- purrr::keep(res$value, function(x) x$name == workspace_name)[[1]]

  # Return the workspace GUID
  workspace$id
}

# Similar to querying workspaces, the API needs the dataset GUID. This function
# takes a workspace name (where the dataset is kept) and the target dataset name
# and returns the dataset GUID
.get_dataset_id <- function(workspace_name, dataset_name, access_token) {
  # Use the workspace name to lookup the workspace GUID
  workspace_id <- .get_workspace_id(workspace_name, access_token)

  # Query URL which specifies that the dataset of interest will be in the
  # specified workspace
  url <- paste0("https://api.powerbi.com/v1.0/myorg/groups/",
                workspace_id, "/datasets")

  # Use access token for authentication, and send query to API. The return is
  # again a list of lists, where each list now contains details about all
  # datasets in a given workspace
  headers <- httr::add_headers(Authorization = paste("Bearer", access_token))
  res <- httr::content(httr::GET(url = url, config = headers))

  # Identify list relating to the dataset of interest
  dataset <- purrr::keep(res$value, function(x) x$name == dataset_name)[[1]]

  # Return dataset GUID
  dataset$id
}

# Specify a function that takes a DAX query as input and returns the necessary
# formatting for passing to the API endpoint
.construct_query <- function(query) {
  stringr::str_glue(
    "{{",
    "  \"queries\": [",
    "    {{",
    "      \"query\":\"{query}\"",
    "    }}",
    "  ],",
    "  \"serializerSettings\": {{",
    "    \"includeNulls\": true",
    "  }}",
    "}}"
  )
}

#' This is the main function to be used. Specify the name of workspace, dataset,
#' and target table, and this function will query the API and return the table
#' as a data.frame
#'
#' The API endpoint is capped at returning a maximum of 1,000,000 values per
#' query, so we query the columns of the table in batches and then join them in
#' R
#'
#' @param workspace_name The PowerBI workspace name.
#' @param dataset_name The name of the PowerBI Dataset within the workspace.
#' @param table_name The name of the table within the PowerBI Dataset to be
#' accessed.
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_ds') to create this token.
#'
#' @return A tibble of the PowerBI table from the PowerBI dataset.
#' @export
#' @examples
#'  \dontrun{
#' # Create PowerBI Dataset azure token
#' tk <- get_az_tk('pbi_ds')
#'
#' # Load AD dummy file from PowerBI dataflow
#' data_lbw <- download_dataset_table_rest(
#'   workspace_name = "DOH - Reference Data",
#'   dataset_name = "r_datasets",
#'   table_name = "COUNT_lbw",
#'   access_token = tk$credentials$access_token)
#'}
download_dataset_table_rest <- function(workspace_name, dataset_name,
                                        table_name, access_token) {
  if (!requireNamespace("furrr", quietly = TRUE)) {
    stop(
      "Package \"furrr\" must be installed to use this function.\n",
      call. = FALSE
    )
  }
  # Dealing with undefined global functions or variables
  .data <- NULL
  `%>%` <- magrittr::`%>%`

  # Lookup GUID of dataset to query
  dataset_id <- .get_dataset_id(workspace_name, dataset_name, access_token)

  # Construct the query URL, specifying that we'll be executing a DAX query
  # against the dataset of interest
  url <- paste0("https://api.powerbi.com/v1.0/myorg/datasets/", dataset_id,
                "/executeQueries")
  headers <- httr::add_headers(Authorization = paste("Bearer", access_token))

  # Specify a DAX query requesting the metadata for all columns in all tables
  # in the specified PowerBI dataset
  query <- paste0("EVALUATE SELECTCOLUMNS(COLUMNSTATISTICS(), [Table Name],",
                  "                       [Column Name], [Cardinality])")
  cnames_query <- .construct_query(query)

  # Submit the query to the API endpoint
  query_res <- httr::POST(url = url, body = cnames_query, config = headers,
                          httr::content_type_json())

  # Extracting the results from the query and filter to only retain
  # the metadata for the table of interest
  table_metadata <- httr::content(query_res)$results[[1]]$tables[[1]]$rows %>%
    dplyr::bind_rows() %>%
    dplyr::rename("table" = "[Table Name]", "column" = "[Column Name]",
                  "rows" = "[Cardinality]") %>%
    dplyr::filter(table == table_name)

  # Extract the column names for the table
  cnames <- table_metadata %>%
    # Returned metadata has a row listing the number of rows in the table,
    # ignore this when extracting the column names
    dplyr::filter(!grepl(pattern = "RowNumber-", .data$column)) %>%
    dplyr::select("column") |>
    t() |>
    c()

  # DAX expects column names to be passed as [column], so wrap each column
  # name in brackets and then collapse all to a single string
  names_coll <- paste0("[", cnames, "]", collapse = ",")
  names_coll <- as.character(names_coll)
  table_name_mod <- paste0("'", table_name, "'") ######## KEY LINE TO put quotes around table name #nolint
  # Use a SELECTCOLUMNS() query to only return the desired batch of columns
  query <- .construct_query(
    stringr::str_glue("EVALUATE SELECTCOLUMNS({table_name_mod}, {names_coll})")
  )

  # Download the values for selected columns, returning a list of lists, where
  # each list contains the values for a given row
  batch_query <- httr::POST(url = url, body = query, config = headers,
                            httr::content_type_json())
  batch_rows <- httr::content(batch_query)$results[[1]]$tables[[1]]$rows
  # Iterate over each row-list and replace NULL values with NA, and stack
  # all rows into a single data.frame
  tab <- suppressWarnings(data.table::rbindlist(batch_rows))

  # The returned column names from the API use the convention:
  # "table_name[column_name]", so we remove the table name from the column names
  # and return the dataset
  pattern <- stringr::str_glue("{table_name}\\[|\\]")
  stats::setNames(tab, stringr::str_remove_all(names(tab), pattern))
}
