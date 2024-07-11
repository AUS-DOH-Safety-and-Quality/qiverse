#' Download SharePoint File
#'
#' @param site_url This is the url when first accessing the sharepoint team via
#' your web browser (Home page)
#' @param file_url This is a direct link to the file to be downloaded. It can be
#' obtained by navigating to the file you want to download, right-clicking and
#' selecting "Copy Link"
#' @param token The token generated with the correct SharePoint permissions. Use
#' get_az_tk('sp") to create this token.
#' @param download Set this to FALSE to download the file into a temporary file.
#' This temporary file can then be read in using any standard data reading
#' function. Set this to TRUE to download the file to your computer.
#' @param download_dest This option is only active when download is set to TRUE.
#' When download_location is set to NULL, the file is just downloaded into the
#' current working directory. Otherwise, specify the destination path to save
#' the file.
#'
#' @return A temporary file stored in an object if download is set to FALSE. If
#' download is set to TRUE, the selected file is downloaded to the chosen
#' directory.
#' @export
#' @examples
#'  \dontrun{
#' # Create SharePoint azure token
#' tk <- get_az_tk('sp')
#'
#' # Download file to working directory from SharePoint site
#' download_sharepoint_file(
#' site_url = "https://myexample.sharepoint.com/sites/Example-Files/"),
#' file_url = paste0("https://myexample.sharepoint.com/:x:/r/sites/Example-Files/Shared%20Documents/Example_datasets/example_data_amended.csv?d=XXXXXXXXXXXXXXXXXXXXXXXXXXXX"),
#'   token = tk,
#'   download = TRUE
#' )
#'
#' # Download file to C:/ drive from SharePoint subsite
#' download_sharepoint_file(
#' site_url = "https://myexample.sharepoint.com/sites/Example-Files/"),
#' file_url = paste0("https://myexample.sharepoint.com/:x:/r/sites/Example-Files/Shared%20Documents/Example_datasets/example_data_amended.csv?d=XXXXXXXXXXXXXXXXXXXXXXXXXXXX"),
#'   "?d=wa2a4067cf69241ce8f4a5d209878328b&csf=1&web=1&e=ybGCQF"),
#'   token = tk,
#'   download = TRUE,
#'   download_dest = 'C:/cabg.csv'
#' )
#'
#' # Example to download to tempfile, to load straight into R memory
#' data <- download_sharepoint_file(
#'   site_url = paste0("https://wahealthdept.sharepoint.com/sites/",
#'   "DOH-ReferenceData/"),
#' site_url = "https://myexample.sharepoint.com/sites/Example-Files/"),
#' file_url = paste0("https://myexample.sharepoint.com/:x:/r/sites/Example-Files/Shared%20Documents/Example_datasets/example_data_amended.csv?d=XXXXXXXXXXXXXXXXXXXXXXXXXXXX"),
#'   token = tk,
#'   download = FALSE
#' ) |>
#'   # Pipe in the appropriate function to read in the data
#'   read.csv()
#'}
download_sharepoint_file <- function(
  site_url,
  file_url,
  token,
  download = FALSE,
  download_dest = NULL
) {
  # split up site_url to determine where drive starts
  if (substr(site_url, nchar(site_url), nchar(site_url)) == "/") {
    site_url <- substr(site_url, 1, nchar(site_url) - 1)
  }
  site_url_split <- stringr::str_split(site_url, pattern = "/")[[1]]

  # clean file_url
  ## If file url has "?", clean the end of the url to remove the "?"
  # and anything after
  if (gregexpr("\\?", file_url)[[1]][1] != -1) {
    file_url <- substr(
      x = file_url,
      start = 1,
      stop = gregexpr("\\?", file_url)[[1]][1] - 1
    )
  }
  file_url_split <- stringr::str_split(file_url, pattern = "/")[[1]]
  ## remove extra variables in shared link
  file_url_split <- file_url_split[-c(4, 5)]

  drive_url <- paste0(file_url_split[1:(length(site_url_split) + 1)],
                      collapse = "/")
  file_src <- paste0(
    file_url_split[(length(site_url_split) + 2):length(file_url_split)],
    collapse = "/"
  ) |>
    utils::URLdecode()

  # Initialise objects ####
  # Connect to sharepoint site object ####
  site <- Microsoft365R::get_sharepoint_site(
    site_url = site_url,
    token = token
  )

  # Extract table of metadata for all drives in sharepoint site ####
  ## Loop function over all drives to extract metadata
  all_drives <- lapply(site$list_drives(), function(x) {
    # Extract properties of this list
    x_p <- x$properties
    # Output key metadata
    data.frame(
      name = x_p$name,
      id = x_p$id,
      webUrl = x_p$webUrl,
      driveType = x_p$driveType,
      createdDateTime = x_p$createdDateTime,
      lastModifiedDateTime = x_p$lastModifiedDateTime
    )
  }) |>
    # Combine the list of drives into a single data.table
    dplyr::bind_rows()

  drive <- site$get_drive(drive_id = all_drives[all_drives$webUrl == drive_url,
                                                "id"])

  # Output file
  if (download == TRUE) {
    if (!is.null(download_dest)) {
      # Download file to download_dest
      drive$download_file(src = file_src, dest = download_dest,
                          overwrite = TRUE)
    } else {
      # Download file to working directory
      drive$download_file(src = file_src, overwrite = TRUE)
    }
  } else {
    # Get extension for temp file
    fileext <- file_url_split[length(file_url_split)]
    fileext <- substr(fileext, gregexpr("\\.", fileext)[[1]][1], nchar(fileext))

    # Save to temp
    temp <- tempfile(fileext = fileext)
    drive$download_file(src = file_src, dest = temp)
    return(temp)
  }
}

#' Upload SharePoint File
#'
#' @param src The file path to the local file that is being uploaded to
#' SharePoint
#' @param site_url This is the url when first accessing the sharepoint team via
#' your web browser (Home page)
#' @param dest_fldr_url This is a direct link to the SharePoint folder where the
#' file will be uploaded to. It can be obtained by navigating to the folder you
#' want to upload to, right-clicking and selecting "Copy Link"
#' @param token The token generated with the correct SharePoint permissions. Use
#' get_az_tk('sp") to create this token.
#'
#' @return Upload local file to selected SharePoint folder.
#' @export
#' @examples
#' \dontrun{
#' # Create SharePoint azure token
#' tk <- get_az_tk('sp')
#'
#' # Example to upload local file to SharePoint folder
#' upload_sharepoint_file(
#'   src = 'TestFiles/test.txt',
#' site_url = "https://myexample.sharepoint.com/sites/Example-Files/"),
#' file_url = paste0("https://myexample.sharepoint.com/:x:/r/sites/Example-Files/Shared%20Documents/Example_datasets/example_data_amended.csv?d=XXXXXXXXXXXXXXXXXXXXXXXXXXXX"),
#'   token = tk
#' )
#' }

upload_sharepoint_file <- function(
  src,
  site_url,
  dest_fldr_url,
  token
) {
  # split up site_url to determine where drive starts
  if (substr(site_url, nchar(site_url), nchar(site_url)) == "/") {
    site_url <- substr(site_url, 1, nchar(site_url) - 1)
  }
  site_url_split <- stringr::str_split(site_url, pattern = "/")[[1]]

  # clean dest_fldr_url
  ## If destination folder url has "?", clean the end of the url to remove the
  # "?" and anything after
  if (gregexpr("\\?", dest_fldr_url)[[1]][1] != -1) {
    dest_fldr_url <- substr(
      x = dest_fldr_url,
      start = 1,
      stop = gregexpr("\\?", dest_fldr_url)[[1]][1] - 1
    )
  }
  dest_fldr_url_split <- stringr::str_split(dest_fldr_url, pattern = "/")[[1]]
  ## remove extra variables in shared link
  dest_fldr_url_split <- dest_fldr_url_split[-c(4, 5)]

  ## Create clean link to drive
  drive_url <- paste0(dest_fldr_url_split[1:(length(site_url_split) + 1)],
                      collapse = "/")

  # Create clean link to destination folder
  dest_fldr <- paste0(
    dest_fldr_url_split[
      (length(site_url_split) + 2):length(dest_fldr_url_split)
    ],
    collapse = "/"
  ) |>
    utils::URLdecode()

  # Extract file name
  src_split <- stringr::str_split(src, pattern = "/")[[1]]
  src_filename <- src_split[length(src_split)]

  # Initialise objects ####
  # Connect to sharepoint site object ####
  site <- Microsoft365R::get_sharepoint_site(
    site_url = site_url,
    token = token
  )

  # Extract table of metadata for all drives in sharepoint site ####
  ## Loop function over all drives to extract metadata
  all_drives <- lapply(site$list_drives(), function(x) {
    # Extract properties of this list
    x_p <- x$properties
    # Output key metadata
    data.frame(
      name = x_p$name,
      id = x_p$id,
      webUrl = x_p$webUrl,
      driveType = x_p$driveType,
      createdDateTime = x_p$createdDateTime,
      lastModifiedDateTime = x_p$lastModifiedDateTime
    )
  }) |>
    # Combine the list of drives into a single data.table
    dplyr::bind_rows()

  drive <- site$get_drive(drive_id = all_drives[all_drives$webUrl == drive_url,
                                                "id"])

  # Upload to working directory
  drive$upload_file(src = src, dest = paste0(dest_fldr, "/", src_filename) |>
                      utils::URLencode())

  # Print result
  print(paste0(src, " uploaded to ",
               paste0(drive_url, "/", dest_fldr |> utils::URLencode(), "/",
                      src_filename)))
}
