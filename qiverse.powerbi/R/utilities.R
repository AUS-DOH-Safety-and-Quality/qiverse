glue_chars <- function(...) {
  as.character(glue::glue(..., .envir = parent.frame(), .sep = "\n"))
}

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

  metadata <- purrr::map(xml2::xml_children(schema), \(child) {
    list(name = xml2::xml_attr(child, "field"),
         type = gsub("xsd:","",xml2::xml_attr(child, "type"), fixed = TRUE))
  })

  names(metadata) <- purrr::map_chr(xml2::xml_children(schema), \(child) {xml2::xml_attr(child, "name")})

  xmla_extract_fun <- list(
    "long" = xml2::xml_integer,
    "double" = xml2::xml_double,
    "string" = xml2::xml_text,
    "dateTime" = \(x) { as.Date(xml2::xml_text(x), format = "%Y-%m-%dT%H:%M:%S") },
    "boolean" = \(x){ tolower(xml2::xml_text(x)) == "true" }
  )

  rows <- purrr::map_dfr(xml2::xml_find_all(xmla_rowset, "//d3:row"), \(row) {
    row_elems <- xml2::xml_children(row)
    col_names <- xml2::xml_name(row_elems)
    row_to_df <- purrr::map2(row_elems, col_names, \(x, x_name) {
        curr_type <- metadata[[x_name]]$type
        suppressWarnings(xmla_extract_fun[[curr_type]](x))
        }) |>
      data.frame()
    names(row_to_df) <- purrr::map_chr(metadata[col_names], "name")
    row_to_df
  })
}

escape_xml_query <- function(query) {
  query |>
    stringr::str_replace_all("\\&", "&amp;") |>
    stringr::str_replace_all("\\<", "&lt;") |>
    stringr::str_replace_all("\\>", "&gt;")
}

construct_xmla_query <- function(dataset, query) {
  glue::glue('
    <Envelope xmlns="http://schemas.xmlsoap.org/soap/envelope/">
      <Header>
        <BeginSession soap:mustUnderstand="1" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns="urn:schemas-microsoft-com:xml-analysis" />
        <Version Sequence="922" xmlns="http://schemas.microsoft.com/analysisservices/2003/engine/2" />
      </Header>
      <Body>
        <Execute xmlns="urn:schemas-microsoft-com:xml-analysis">
          <Command><Statement>{escape_xml_query(query)}</Statement></Command>
          <Properties>
            <PropertyList>
              <Catalog>{dataset}</Catalog>
              <Format>Tabular</Format>
            </PropertyList>
          </Properties>
        </Execute>
      </Body>
    </Envelope>'
  )
}

construct_rest_query <- function(query) {
  glue::glue(
    '{{
      "queries": [{{ "query": "{query}" }}],
      "serializerSettings": {{ "includeNulls": true }}
    }}'
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
