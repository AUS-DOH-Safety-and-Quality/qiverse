get_auth_header <- function(access_token) {
  httr::add_headers(Authorization = paste("Bearer", access_token))
}

get_cluster_url <- function(access_token) {
  cluster_details <-
    httr::GET(url = "https://api.powerbi.com/powerbi/globalservice/v201606/clusterdetails",
            config = get_auth_header(access_token),
            httr::content_type_json()) |>
    httr::content()

  cluster_details$clusterUrl
}

.bind_rows_base <- function(df_list) {
  if (length(df_list) == 0) {
    return(data.frame())
  }

  df_list <- Filter(function(x) !is.null(x), df_list)
  if (length(df_list) == 0) {
    return(data.frame())
  }

  df_list <- lapply(df_list, function(x) {
    if (is.data.frame(x)) {
      return(x)
    }
    as.data.frame(x, stringsAsFactors = FALSE)
  })

  all_names <- unique(unlist(lapply(df_list, names), use.names = FALSE))

  df_list <- lapply(df_list, function(df) {
    missing_names <- setdiff(all_names, names(df))
    for (nm in missing_names) {
      df[[nm]] <- NA
    }
    df[all_names]
  })

  do.call(rbind, df_list)
}

rowset_to_df <- function(xmla_rowset) {
  # Check if query errored before trying to extract columns
  query_fault <- xml2::xml_find_all(xmla_rowset, "//soap:Fault") |>
    xml2::xml_children()

  if (length(query_fault) > 0) {
    fault_details <- xml2::xml_text(query_fault)
    names(fault_details) <- xml2::xml_name(query_fault)

    stop(paste0(fault_details['faultcode'], ": ", fault_details['faultstring']),
         call. = FALSE)
  }

  schema <- xml2::xml_find_all(xmla_rowset, "//xsd:complexType[@name='row']/xsd:sequence")

  metadata <- lapply(xml2::xml_children(schema), \(child) {
    list(name = xml2::xml_attr(child, "field"),
         type = gsub("xsd:","",xml2::xml_attr(child, "type"), fixed = TRUE))
  })

  names(metadata) <- sapply(xml2::xml_children(schema), \(child) {xml2::xml_attr(child, "name")})

  xmla_extract_fun <- list(
    "long" = xml2::xml_integer,
    "double" = xml2::xml_double,
    "string" = xml2::xml_text,
    "dateTime" = \(x) { as.Date(xml2::xml_text(x), format = "%Y-%m-%dT%H:%M:%S") },
    "boolean" = \(x){ tolower(xml2::xml_text(x)) == "true" }
  )

  all_rows <- xml2::xml_find_all(xmla_rowset, "//d3:row")
  n_rows <- length(all_rows)

  rows <- as.data.frame(
    lapply(metadata, function(col_meta) {
      switch(
        col_meta$type,
        "long" = integer(n_rows),
        "double" = double(n_rows),
        "string" = character(n_rows),
        "dateTime" = as.Date(rep(NA_character_, n_rows)),
        "boolean" = logical(n_rows)
      )
    }),
    stringsAsFactors = FALSE
  )
  col_names <- sapply(metadata, \(xml_col) xml_col[["name"]])
  names(rows) <- col_names


  if (n_rows == 0) {
    return(rows)
  }

  for (idx in seq_len(n_rows)) {
    for (elem in xml2::xml_children(all_rows[[idx]])) {
      xml_name <- xml2::xml_name(elem)
      xml_type <- metadata[[xml_name]]$type
      col_name <- metadata[[xml_name]]$name
      rows[idx, col_name] <- xmla_extract_fun[[xml_type]](elem)
    }
  }

  rows
}

escape_xml_query <- function(query) {
  query <- gsub("&", "&amp;", query, fixed = TRUE)
  query <- gsub("<", "&lt;", query, fixed = TRUE)
  gsub(">", "&gt;", query, fixed = TRUE)
}

construct_xmla_query <- function(dataset, query) {
  paste0('
    <Envelope xmlns="http://schemas.xmlsoap.org/soap/envelope/">
      <Header>
        <BeginSession soap:mustUnderstand="1" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns="urn:schemas-microsoft-com:xml-analysis" />
        <Version Sequence="922" xmlns="http://schemas.microsoft.com/analysisservices/2003/engine/2" />
      </Header>
      <Body>
        <Execute xmlns="urn:schemas-microsoft-com:xml-analysis">
          <Command><Statement>', escape_xml_query(query), '</Statement></Command>
          <Properties>
            <PropertyList>
              <Catalog>', escape_xml_query(dataset),'</Catalog>
              <Format>Tabular</Format>
            </PropertyList>
          </Properties>
        </Execute>
      </Body>
    </Envelope>'
  )
}

construct_rest_query <- function(query) {
  paste0(
    '{
      "queries": [{ "query": "', query, '" }],
      "serializerSettings": { "includeNulls": true }
    }'
  )
}

#' Decode a PowerBI Compressed String
#'
#' @description For more efficient storage, PowerBI will store strings in a compressed
#' binary format - compressed using the deflate algorithm and then represented as a base64 string.
#'
#' This function takes the base64 string and decompresses it to return the original string.
#'
#' @param compressed_string The base64 string of the compressed data.
#'
#' @return Original string after decompression.
#' @export
#' @examples
#' table_str <- "i45W8lXSUfJNzElVitWJVnIDctxSc2HcCCDXMS+/JCO1SKEktSgXLBgKFAzNy87LL88D8/2AfL/8EoXigtTkzLTM1BSl2FgA"
#' decompress_string(table_str) |> cat()
#' # [["M","Male"],["F","Female"],["X","Another term"],["U","Unknown"],["N","Not specified"]]
decompress_string <- function(compressed_string) {
  for (pkg in c("zlib")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not installed. ", call. = FALSE)
    }
  }
  # Need to wrap the call in sapply to properly handle a vector of strings
  # otherwise base64_dec will treat as a single large string
  sapply(compressed_string, \(x) {
    x |>
    jsonlite::base64_dec() |>
    # PBI compresses without standard gzip header
    # setting a negative wbits suppresses the header check
    zlib::decompress(wbits = -15) |>
    rawToChar()
  }, USE.NAMES = FALSE)
}

# Query the cluster resolve endpoint to get XMLA server details for a given capacity (needed for querying datasets)
get_pbi_cluster_details <- function(capacity_id, access_token) {
  httr::POST("https://australiasoutheast.pbidedicated.windows.net/webapi/clusterResolve",
              config = get_auth_header(access_token),
              body = jsonlite::toJSON(list(
                databaseName = NA,
                premiumPublicXmlaEndpoint = TRUE,
                serverName = capacity_id
              ), auto_unbox = TRUE),
              httr::content_type_json()) |>
    httr::content()
}

# Generate an XMLA access token for a PowerBI Dataset given the capacity and workspace IDs
get_dataset_access_token <- function(capacity_id, workspace_id, access_token) {
  astoken <- httr::POST("https://api.powerbi.com/metadata/v201606/generateastoken",
                        config = get_auth_header(access_token),
                        body = jsonlite::toJSON(list(
                          applyAuxiliaryPermission = FALSE,
                          auxiliaryPermissionOwner = NA,
                          capacityObjectId = capacity_id,
                          datasetName = NA,
                          intendedUsage = 0,
                          sourceCapacityObjectId = NA,
                          workspaceObjectId = workspace_id
                        ), auto_unbox = TRUE),
                        httr::content_type_json()) |>
    httr::content()

  astoken$Token
}
