% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sharepoint_file.R
\name{download_sharepoint_file}
\alias{download_sharepoint_file}
\title{Download SharePoint File}
\usage{
download_sharepoint_file(
  site_url,
  file_url,
  token,
  download = FALSE,
  download_dest = NULL
)
}
\arguments{
\item{site_url}{This is the url when first accessing the sharepoint team via
your web browser (Home page)}

\item{file_url}{This is a direct link to the file to be downloaded. It can be
obtained by navigating to the file you want to download, right-clicking and
selecting "Copy Link"}

\item{token}{The token generated with the correct SharePoint permissions. Use
get_az_tk('sp") to create this token.}

\item{download}{Set this to FALSE to download the file into a temporary file.
This temporary file can then be read in using any standard data reading
function. Set this to TRUE to download the file to your computer.}

\item{download_dest}{This option is only active when download is set to TRUE.
When download_location is set to NULL, the file is just downloaded into the
current working directory. Otherwise, specify the destination path to save
the file.}
}
\value{
A temporary file stored in an object if download is set to FALSE. If
download is set to TRUE, the selected file is downloaded to the chosen
directory.
}
\description{
Download SharePoint File
}
\examples{
 \dontrun{
# Create SharePoint azure token
tk <- get_az_tk('sp')

# Download file to working directory from SharePoint site
download_sharepoint_file(
  site_url = "https://myexample.sharepoint.com/sites/Example-Files/"),
  file_url = paste0("https://myexample.sharepoint.com/:x:/r/sites/Example-Files/Shared\%20Documents/Example_datasets/example_data_amended.csv?d=XXXXXXXXXXXXXXXXXXXXXXXXXXXX"),
  token = tk,
  download = TRUE
)

# Download file to C:/ drive from SharePoint subsite
download_sharepoint_file(
  site_url = "https://myexample.sharepoint.com/sites/Example-Files/"),
  file_url = paste0("https://myexample.sharepoint.com/:x:/r/sites/Example-Files/Shared\%20Documents/Example_datasets/example_data_amended.csv?d=XXXXXXXXXXXXXXXXXXXXXXXXXXXX"),
  token = tk,
  download = TRUE,
  download_dest = 'C:/test.csv'
)

# Example to download to tempfile, to load straight into R memory
data <- download_sharepoint_file(
  site_url = "https://myexample.sharepoint.com/sites/Example-Files/"),
  file_url = paste0("https://myexample.sharepoint.com/:x:/r/sites/Example-Files/Shared\%20Documents/Example_datasets/example_data_amended.csv?d=XXXXXXXXXXXXXXXXXXXXXXXXXXXX"),
  token = tk,
  download = FALSE
) |>
  # Pipe in the appropriate function to read in the data
  read.csv()
}
}
