% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/databricks_secrets.R
\name{db_secret_scopes_api}
\alias{db_secret_scopes_api}
\title{Interact with the Databricks API for managing secret scopes
(collections of secrets). The Databricks API allows for listing, creating,
and deleting secret scopes.}
\usage{
db_secret_scopes_api(operation, workspace_url, access_token, scope_name = NULL)
}
\arguments{
\item{operation}{The scopes operation to apply
("list", "create", or "delete")}

\item{workspace_url}{The url of the target databricks workspace}

\item{access_token}{The azure access token string which will be used for
authentication}

\item{scope_name}{If creating or deleting a secret scope, the name of the
target scope}
}
\value{
The result of the API call
}
\description{
Interact with the Databricks API for managing secret scopes
(collections of secrets). The Databricks API allows for listing, creating,
and deleting secret scopes.
}
\seealso{
Other Azure methods: 
\code{\link{az_authenticated_api_query}()},
\code{\link{db_secrets_api}()},
\code{\link{store_databricks_access_token}()}
}
\concept{Azure methods}
