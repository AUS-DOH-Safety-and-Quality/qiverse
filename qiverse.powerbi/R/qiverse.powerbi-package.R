#' The qiverse.powerbi package.
#'
#' @description A collection of R functions for working with PowerBI Dataflows and Datasets
#'
#' @name qiverse.powerbi-package
#' @aliases qiverse.powerbi
#'
#' @importFrom httr add_headers GET POST content_type_json content
#' @importFrom purrr map_dfr pmap keep map_chr modify_if
#' @importFrom readr read_csv
#' @importFrom glue glue
#' @importFrom jsonlite toJSON
#' @importFrom uuid UUIDgenerate
#' @importFrom xml2 xml_find_all xml_children xml_attr xml_name xml_text
#'
"_PACKAGE"
