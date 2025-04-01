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

construct_xmla_query <- function(dataset, query) {
  glue::glue('
    <Envelope xmlns="http://schemas.xmlsoap.org/soap/envelope/">
      <Header>
        <BeginSession soap:mustUnderstand="1" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns="urn:schemas-microsoft-com:xml-analysis" />
        <Version Sequence="922" xmlns="http://schemas.microsoft.com/analysisservices/2003/engine/2" />
      </Header>
      <Body>
        <Execute xmlns="urn:schemas-microsoft-com:xml-analysis">
          <Command><Statement>{query}</Statement></Command>
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
