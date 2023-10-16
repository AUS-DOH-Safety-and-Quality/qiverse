#' Download PowerBI Dataset Table
#'
#' This function uses Microsoft's Analysis Services client libraries to connect
#' to a PowerBI dataset by treating it as a SQL Server Analysis Cube. These
#' libraries should be installed by SQL Server and PowerBI, but if you encounter
#' issues they can be manually installed from:
#' https://docs.microsoft.com/en-us/analysis-services/client-libraries
#'
#' This function uses the Common Object Model (COM) in Windows to connect R
#' with the client libraries. Consequently, this function is only compatible
#' with Windows environments
#'
#' @param workspace_name The PowerBI workspace name.
#' @param dataset_name The name of the PowerBI Dataset within the workspace.
#' @param table_name The name of the table within the PowerBI Dataset to be
#' accessed.
#' @param access_token The token generated with the correct PowerBI Dataset
#' permissions. Use get_az_tk('pbi_ds') to create this token.
#' @param verbose Print status messages during connection and download
#'
#' @return A tibble of the PowerBI table from the PowerBI dataset.
#' @export
#' @examples
#'  \dontrun{
#' # Create PowerBI Dataset azure token
#' tk <- get_az_tk('pbi_ds')
#'
#' # Load AD dummy file from PowerBI dataflow
#' ad_dummy <- download_dataset_table_com(
#'   workspace_name = "DOH - Reference Data",
#'   dataset_name = "r_datasets",
#'   table_name = "COUNT_lbw",
#'   access_token = tk$credentials$access_token)
#'}
download_dataset_table_com <- function(workspace_name, dataset_name, table_name,
                                        access_token, verbose = TRUE) {
  if (!requireNamespace("RDCOMClient", quietly = TRUE)) {
    stop(
      "Package \"RDCOMClient\" must be installed to use this function.\n",
      "Install via: ",
      "devtools::install_github(repo=\"BSchamberger/RDCOMClient\")",
      call. = FALSE
    )
  }

  attachNamespace("RDCOMClient")

  # Dealing with undefined global functions or variables
  .data <- NULL

  # Create connection string using the dataset properties and Azure AD App
  # credentials
  #
  # The str_glue() function replaces any text in curly braces with the value
  # from the R environment (in this case just strings, but could also be a
  # function or something more complex)
  connection_string <- stringr::str_glue(
    "Provider = MSOLAP",
    "Datasource = powerbi://api.powerbi.com/v1.0/myorg/{workspace_name}",
    "initial catalog = {dataset_name}",
    "PWD = {access_token}",
    .sep = ";"
  )

  # Construct the DAX query to apply to the dataset, in this case just returning
  # the specified table in full. It's also possible to instead apply more
  # complex queries which involve multiple tables within the dataset, or to
  # send multiple queries to return multiple tables
  query <- stringr::str_glue("EVALUATE('{table_name}')")

  if (verbose) {
    message("Connecting to PowerBI...")
  }

  # Use the RDCOMClient package to create a COM object that R can interact with,
  # 'ADODB.Connection' is a generic COM type for connecting to databases
  con <- RDCOMClient::COMCreate("ADODB.Connection")

  # Update the "ConnectionString" property of the created COM object with the
  # created connection string
  con[["ConnectionString"]] <- as.character(connection_string)

  # Initialise connection to PowerBI dataset, if there are any errors with the
  # connection details, it's this step that fails
  con$Open()

  if (verbose) {
    message("Downloading raw data...")
  }

  # Execute DAX query to return full table from dataset
  rs <- con$Execute(as.character(query))

  # Extract the returned values from the query result
  # This returns a list of lists -> one list per row
  dd <- rs$GetRows()

  # Get the number of returned columns (needed in the next step)
  num_cols <- rs$Fields()$Count()

  # Extract variable names and remove pre-/suffixes
  cnames <- purrr::map_chr(seq(0, num_cols - 1),
                            ~rs$Fields()$Item(.x)$Name()) |>
    stringr::str_remove_all(.data, stringr::str_glue("{table_name}\\[|\\]"))

  # Identify the column number of the date variables, so that they can be
  # formatted correctly
  date_variables <- grep("COMDate", purrr::map(dd[[1]], ~(class(.x))))

  if (verbose) {
    message("Formatting raw data...")
  }

  formatted <- dd |>
    # Replace NULL values with NA so that the row-binding doesn't ignore them
    purrr::modify_depth(.depth = 2, ~ifelse(is.null(.x), NA, .x)) |>
    # Stack the returned lists by row to form a data.frame
    do.call(rbind.data.frame, .data) |>
    # Add the extracted column names
    stats::setNames(cnames) |>
    # Format date variables to match R's date format
    dplyr::mutate(dplyr::across(dplyr::all_of(cnames[date_variables]),
                  as.Date,
                  origin = "1899-12-30"))

  unloadNamespace("RDCOMClient")
  formatted
}

#' This function uses Microsoft's Analysis Services client libraries to connect
#' to a PowerBI dataset by treating it as a SQL Server Analysis Cube. This
#' function uses the .NET Framework implementations of Microsoft's Client
#' Libraries via powershell. Powershell and the .NET libraries are able to be
#' used on Linux and MacOS via Powershell Core and .NET Core.
#
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
#' # Load AD dummy file from PowerBI dataset
#' ad_dummy <- download_dataset_table_pwsh(
#'   workspace_name = "DOH - Reference Data",
#'   dataset_name = "r_datasets",
#'   table_name = "COUNT_lbw",
#'   access_token = tk$credentials$access_token)
#'}
download_dataset_table_pwsh <- function(workspace_name, dataset_name,
                                    table_name, access_token) {
  if (!requireNamespace("sys", quietly = TRUE)) {
    stop(
      "Package \"sys\" must be installed to use this function.\n",
      call. = FALSE
    )
  }
  dotnetpath <- file.path(Sys.getenv("HOME"), ".dotnet")
  adomd_path <- file.path(dotnetpath, "adomd")
  ident_path <- file.path(dotnetpath, "identity")

  # The needed libraries for connecting and authenticating to PowerBI can be
  # downloaded as standalone .dll files. This function block downloads the
  # and stores them in a folder called '.dotnet' in the user's home directory
  if (!dir.exists(dotnetpath)) {
    dir.create(dotnetpath)

    if (!dir.exists(adomd_path)) {
      dir.create(adomd_path)
      utils::download.file(url = "https://globalcdn.nuget.org/packages/microsoft.analysisservices.adomdclient.netcore.retail.amd64.19.39.2.2.nupkg", # nolint
                    destfile = file.path(adomd_path, "adomd.zip"),
                    mode = "wb",
                    quiet = TRUE)
      utils::unzip(file.path(adomd_path, "adomd.zip"), exdir = adomd_path)
    }

    if (!dir.exists(ident_path)) {
      dir.create(ident_path)
      utils::download.file(url = "https://globalcdn.nuget.org/packages/microsoft.identity.client.4.44.0.nupkg", # nolint
                    destfile = file.path(ident_path, "ident.zip"),
                    mode = "wb",
                    quiet = TRUE)
      utils::unzip(file.path(ident_path, "ident.zip"), exdir = ident_path)
    }
  }

  # Next, we need the full file path to each of the downloaded .dll files
  adomd_dll_path <- file.path(
    adomd_path,
    list.files(adomd_path,
               recursive = TRUE,
               pattern = "Microsoft.AnalysisServices.AdomdClient.dll"))
  adomd_dll_path <- as.character(adomd_dll_path)
  ident_dlls <- list.files(ident_path,
                           recursive = TRUE,
                           pattern = "Microsoft.Identity.Client.dll")
  ident_dll_path <- file.path(ident_path,
                              grep("netcoreapp2.1", ident_dlls, value = TRUE))
  ident_dll_path <- as.character(ident_dll_path)

  # Use provided workspace and dataset names to generate connection string
  conn_string <- stringr::str_glue(
    "Provider = MSOLAP",
    "Datasource = powerbi://api.powerbi.com/v1.0/myorg/{workspace_name}",
    "initial catalog = {dataset_name}",
    "PWD = {access_token}",
    .sep = ";"
  )
  conn_string <- as.character(conn_string)

  arg <- stringr::str_glue(
    # Look up where the analysis services and authentication libraries were
    # installed and load them for use
    "Add-Type -Path \"{adomd_dll_path}\"",
    "Add-Type -Path \"{ident_dll_path}\"",

    # Initialise analysis services connection manager
    "$Connection = new-object Microsoft.AnalysisServices.AdomdClient.AdomdConnection",  # nolint

    # Add connection parameters to connection manager
    "$Connection.ConnectionString = \"{conn_string}\"",

    # Open connection to the PowerBI dataset
    "$Connection.Open()",

    # Initialise Analysis Services data adapter, which is used to apply a query
    # to a connection object and download the results
    "$Query = \"EVALUATE {table_name}\"",
    "$Adapter = New-Object Microsoft.AnalysisServices.AdomdClient.AdomdDataAdapter $Query, $Connection",  # nolint

    # Initialise .NET object which will store downloaded dataset in memory
    "$Results = New-Object System.Data.DataTable",

    # Use data adapter to download PowerBI dataset to in-memory storage
    "$Adapter.Fill($Results) | Out-Null",

    # Convert the results to a CSV string which R will read in
    "$Results | ConvertTo-Csv -NoTypeInformation",
    .sep = ";"
  )

  # Execute the above Powershell script, and store the raw (byte-string) output
  out1 <- sys::exec_internal("pwsh", args = c("-Command", arg))

  # Convert the byte-string csv into a character string to be read in
  tab <- readr::read_csv(rawToChar(out1$stdout),
                         # Suppresses column-detection summary output
                         col_types = readr::cols())

  # The returned column names from the download use the convention:
  # "table_name[column_name]", so we remove the table name from the column names
  # and return the dataset
  stats::setNames(tab,
           stringr::str_remove_all(names(tab),
                                   stringr::str_glue("{table_name}\\[|\\]")))
}
