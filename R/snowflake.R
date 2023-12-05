#' Create Connection to Snowflake
#'
#' @description
#' Please ensure that you have the Snowflake ODBC driver installed.
#' This can be found here:
#' * https://docs.snowflake.com/en/user-guide/odbc-download.html.
#'
#' Also ensure that your default role in Snowflake has been set correctly.
#' If you have not, execute the following command in Snowflake
#' (replacing your HE number):
#'
#' ALTER USER "HExxxxxx@HEALTH.WA.GOV.AU" SET DEFAULT_ROLE = "HQIU_SANDPIT_ALL"
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
#' con <- snowflake_con(server_name = 'hsswa.australia-east', token = tk)
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
#'   database_name = 'HQIU_SANDPIT',
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

  ## The Table is set in the database "HQIU_SANDPIT", schema "SQUIS" and table
  ## "SQUIS_CORE_WIDE". The database and schema have already been created in
  # Snowflake.
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
