#' Create Connection to Snowflake
#'
#' @description
#' Please ensure that you have the Snowflake ODBC driver installed.
#' This can be found here:
#' * https://docs.snowflake.com/en/user-guide/odbc-download.html.
#'
#' Also ensure that your default role in Snowflake has been set correctly.
#' If you have not, execute the following command in Snowflake:
#'
#' ALTER USER "MY_USER_ID@EXAMPLE.COM" SET DEFAULT_ROLE = "MY_DEFAULT_ROLE"
#'
#' @param server_name This is the name of Snowflake server you are connecting
#' to. This should include the name, and the server. For example, this value
#' could be "mysnowflakeserver.australia-east"
#' @param token The token generated with the correct Snowflake permissions.
#' Use `get_az_tk('sf")` to create this token.
#'
#' @return Establishes a connection to the Snowflake server, and stores this
#' as an object.
#' @export
#' @examples
#'  \dontrun{
#' # Create Snowflake azure token
#' tk <- get_az_tk('sf')
#' con <- snowflake_con(server_name = 'my-example.server-australiaeast', token = tk)
#'}
snowflake_con <- function(
  server_name = Sys.getenv("snowflake_server"),
  token
) {
  if (server_name == "") {
    stop("No server_name was entered")
  }
  ## Initialise the connection to Snowflake using the Snowflake ODBC driver
  con <- DBI::dbConnect(odbc::odbc(),
                        driver = "{SnowflakeDSIIDriver}",
                        server = paste0(server_name,
                                        ".azure.snowflakecomputing.com"),
                        authenticator = "oauth",
                        token = token$credentials$access_token)

  return(con)
}

#' Ingest table into Snowflake
#'
#' @param data This is the R object that we are importing into snowflake.
#' This must be a data.frame, data.table or tibble.
#' @param con The connection established by the `snowflake_con()` function.
#' @param database_name The database where the table will be stored.
#' @param schema_name The schema for the table to be stored.
#' @param table_name The name of the table for Snowflake
#'
#' @return A data.frame with output metadata on the ingestion process.
#' @import data.table
#' @export
#' @examples
#'  \dontrun{
#' # Create Snowflake azure token
#' tk <- get_az_tk('sf')
#' con <- snowflake_con(token = tk)
#'
#' # Ingest Example To Snowflake
#' ingest_to_snowflake(
#'   data = data.frame(Alpha = c('a', 'b', 'c'), Number = 1:3),
#'   con = con,
#'   database_name = 'EXAMPLE_DATASET',
#'   schema_name = 'EXAMPLE',
#'   table_name = 'TEST'
#' )
#'}
ingest_to_snowflake <- function(
  data,
  con,
  database_name,
  schema_name,
  table_name
) {
  # Copy data
  input_data <- copy(data)

  # Convert data into data.table for faster processing
  input_data <- data.table::as.data.table(data)

  # Save data into temporary parquetfile ####
  # Create a temporary path to store downloaded file
  temp_path <- tempfile(fileext = ".parquet")
  ## Replace backslash with frontslash for later SQL code
  temp_path <- gsub("\\\\", "/", temp_path)
  # Write as parquet file into temporary path
  arrow::write_parquet(input_data, sink = temp_path)

  # Create schema in Snowflake ####
  DBI::dbGetQuery(con, DBI::SQL(paste0(
    "create schema IF NOT EXISTS ", database_name, ".", schema_name
  )))

  # Set Data Type in SQL ####
  sql_data_type <- DBI::dbDataType(con, input_data)
  # Convert those with character lengths greater than 255, to be the maximum
  # observed
  for (i in seq_len(ncol(input_data))) {
    if (substr(sql_data_type[i], 1, 7) == "VARCHAR" && nrow(input_data) > 0) {
      max_nchar <-
        input_data[, nchar(eval(parse(text = paste0("`", names(input_data)[i], "`"))))] |> #nolint
        max(na.rm = TRUE)
      if (!is.na(max_nchar)) {
        if (max_nchar > 255) {
          sql_data_type[i] <- paste0("VARCHAR(", max_nchar, ")")
        }
      }
    }
  }

  # Create an empty table in snowflake ####
  ## Create a target relational table for the Parquet data.
  # The table is temporary,
  ## meaning it persists only for the duration of the user session and is not
  ## visible to other users.
  DBI::dbGetQuery(con, DBI::SQL(paste0(
    "create or replace table ", database_name, ".", schema_name, ".",
    table_name, " ( ", paste0('"', names(input_data), '" ', sql_data_type,
                              collapse = ", "), ");"
  )))

  # Create a Staging Area for our File ####
  ## Create an internal stage that references the file format object.
  DBI::dbGetQuery(con, DBI::SQL(paste0(
    "create or replace temporary stage ", database_name, ".", schema_name, ".",
    table_name, "_STAGE", " FILE_FORMAT = ( TYPE = PARQUET );"
  )))

  # Stage the data file ####
  ## Uploads the file from our own local directory into the staging area
  put_output <- DBI::dbGetQuery(con, DBI::SQL(paste0(
    "PUT file://", temp_path, " @", database_name, ".", schema_name, ".",
    table_name, "_STAGE", " overwrite=true;"
  )))

  # Load the Staged data into the relational table ####
  ## Load the Parquet data into the relational table.
  ##
  ## A SELECT query in the COPY statement identifies a numbered set of
  # columns in
  ## the data files you are
  ## loading from. Note that all Parquet data is stored in a single column ($1).
  ##
  ## Cast element values to the target column data type.

  DBI::dbGetQuery(con, DBI::SQL(paste0(
    "COPY INTO ", database_name, ".", schema_name, ".", table_name, "
    FROM (
        SELECT ", paste0("$1:", names(input_data), "::", sql_data_type,
                         collapse = ", "), "
        FROM @", database_name, ".", schema_name, ".", table_name, "_STAGE",
    "/", put_output$target, "
    ) FILE_FORMAT = ( TYPE = PARQUET );"
  )))
}

#' Ingest a PowerBI Dataflow table into Snowflake
#'
#' This function uses Snowflake's Azure Blob Storage connectivity to
#' directly import the dataflow table, without needing to first download the
#' file locally and stage.
#'
#' @param con The connection established by the `snowflake_con()` function.
#' @param pbi_workspace The name of the PowerBI workspace containing the dataflow
#' @param pbi_dataflow The name of the PowerBI dataflow containing the target table
#' @param pbi_table The name of the PowerBI dataflow table to ingest
#' @param database_name The database where the table will be stored.
#' @param schema_name The schema for the table to be stored.
#' @param table_name The name of the table for Snowflake
#' @param pbi_tk The token generated with the correct PowerBI permissions.
#' Use `get_az_tk("pbi_df")` to create this token.
#'
#' @return A data.frame with output metadata on the ingestion process.
#' @import glue
#' @import qiverse.powerbi
#' @export
#' @examples
#'  \dontrun{
#' # Create Snowflake azure token
#' tk <- get_az_tk('sf')
#' con <- snowflake_con(token = tk)
#'
#'
#'  <- get_az_tk('pbi_df')
#'
#' # Ingest Example To Snowflake
#' ingest_dataflow_table(
#'   con = con,
#'   pbi_workspace = 'My PBI Workspace',
#'   pbi_dataflow = 'My PBI Dataflow',
#'   pbi_table = 'MY PBI Table',
#'   database_name = 'EXAMPLE_DATASET',
#'   schema_name = 'EXAMPLE',
#'   table_name = 'TEST',
#'   pbi_tk = pbi_tk
#' )
#'}
ingest_dataflow_table <- function(
  con,
  pbi_workspace,
  pbi_dataflow,
  pbi_table,
  database_name,
  schema_name,
  table_name,
  pbi_tk
) {
  for (pkg in c("qiverse.powerbi", "glue")) {
    if (!require(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not installed. ", call. = FALSE)
    }
  }

  verbose <- FALSE
  access_token <- pbi_tk$credentials$access_token
  # Extract the metadata and access tokens needed for the target table
  target_dataflow <- qiverse.powerbi:::get_dataflow_metadata(pbi_workspace, pbi_dataflow,
                                                              access_token, verbose)
  dataflow_id <- target_dataflow$cdsaModel$objectId
  target_table <- qiverse.powerbi:::get_table_metadata(dataflow_id, pbi_table, access_token)
  sas_key <- qiverse.powerbi:::get_sas_key(dataflow_id, pbi_table, access_token)

  # Extract the column names and types
  table_colnames <- purrr::map_chr(target_table$attributes, \(column) {
    # Snowflake accepts all PBI typenames except 'int64
    coltype <- ifelse(column$dataType == "int64", "int", column$dataType)
    # Enclose name in double-quotes in case of spaces in name
    glue::glue('"{column$name}" {coltype}')
  }) |>
    paste(collapse = ",")

  # Need to remove :443 port specification from URL
  location <- gsub(":443", "", target_table$partitions[[1]]$location, fixed = TRUE)
  location_parts <- strsplit(location, "/")[[1]]
  blob_storage_url <- location_parts[3]
  blob_storage_container <- location_parts[4]
  filename <- target_table$`pbi:refreshPolicy`$location

  date_format <- ifelse(target_table$locale == "en-US", "MM/DD/YYYY", "DD/MM/YYYY")
  # Only needed for the TIMESTAMP format, as the TIME format is always 24-hour
  time_format <- ifelse(target_table$locale == "en-GB", "HH24:MI:SS", "HH12:MI:SS PM")

  create_command <- glue::glue(
    "CREATE OR REPLACE TABLE {database_name}.{schema_name}.{table_name}({table_colnames})"
  )
  ingest_command <- glue::glue(
    "COPY INTO {database_name}.{schema_name}.{table_name}",
    "FROM 'azure://{blob_storage_url}/{blob_storage_container}/'",
    "FILES=('{filename}')",
    "CREDENTIALS=(AZURE_SAS_TOKEN='?{sas_key}')",
    "FILE_FORMAT = (TYPE = 'CSV' FIELD_OPTIONALLY_ENCLOSED_BY = '\"' DATE_FORMAT = '{date_format}' TIMESTAMP_FORMAT = '{date_format} {time_format}')",
    .sep = "\n"
  )

  DBI::dbGetQuery(con, create_command)
  DBI::dbGetQuery(con, ingest_command)
}
