#' Sample response from Google Maps Directions API
#'
#' XML documents with \strong{driving} directions from Tel-Aviv to Haifa
#'
#' @format A \code{character} vector of length one
#' @note See \code{\link{response_directions_transit}} for Directions API response with \strong{transit} directions
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_directions_driving)

"response_directions_driving"

#' Sample response from Google Maps Directions API
#'
#' XML documents with \strong{transit} directions from New-York to Boston
#'
#' @format A \code{character} vector of length one
#' @note See \code{\link{response_directions_driving}} for Directions API response with \strong{driving} directions
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_directions_transit)

"response_directions_transit"

#' Sample response from Google Maps Distance Matrix API
#'
#' An XML document with a distance matrix for driving between three locations: Tel-Aviv, Jerusalem and Beer-Sheva
#'
#' @format A \code{character} vector of length one
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_matrix)

"response_matrix"

#' Sample response from Google Maps Geocode API
#'
#' An XML document with a geocoded location for the address "Tel-Aviv"
#'
#' @format A \code{character} vector of length one
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_geocode)

"response_geocode"

