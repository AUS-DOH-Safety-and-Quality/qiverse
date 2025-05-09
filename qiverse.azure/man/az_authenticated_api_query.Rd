% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/azure_authenticated_apis.R
\name{az_authenticated_api_query}
\alias{az_authenticated_api_query}
\title{Make a call to a web API which requires an Azure access token for
authentication}
\usage{
az_authenticated_api_query(method, url, access_token, ...)
}
\arguments{
\item{method}{The HTTP method to call (e.g., "POST", "GET", etc.)}

\item{url}{The web URL of the API to be called}

\item{access_token}{The azure access token string which will be used for
authentication}

\item{...}{Any additional arguments to be passed to the method call. See the
httr documentation for the intended method for valid arguments}
}
\value{
The result of the API call
}
\description{
Make a call to a web API which requires an Azure access token for
authentication
}
\seealso{
Other Azure methods: 
\code{\link{db_secret_scopes_api}()},
\code{\link{db_secrets_api}()},
\code{\link{store_databricks_access_token}()}
}
\concept{Azure methods}
