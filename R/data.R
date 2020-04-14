#' Sample response from Google Maps Directions API
#'
#' XML documents with \strong{driving} directions from Tel-Aviv to Haifa
#'
#' @format A \code{list} obtained using \code{as_list} on XML response
#' @note See \code{\link{response_directions_transit}} for Directions API response with \strong{transit} directions
#' @import magrittr
#' @import sf
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom utils URLencode
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_directions_driving)

"response_directions_driving"

#' Sample response from Google Maps Directions API
#'
#' XML documents with \strong{transit} directions from New-York to Boston
#'
#' @format A \code{list} obtained using \code{as_list} on XML response
#' @note See \code{\link{response_directions_driving}} for Directions API response with \strong{driving} directions
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_directions_transit)

"response_directions_transit"

#' Sample response from Google Maps Distance Matrix API
#'
#' An XML document with a distance matrix for driving between three locations: Tel-Aviv, Jerusalem and Beer-Sheva
#'
#' @format A \code{list} obtained using \code{as_list} on XML response
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_matrix)

"response_matrix"

#' Sample response from Google Maps Geocode API
#'
#' An XML document with a geocoded location for the address "Tel-Aviv"
#'
#' @format A \code{list} obtained using \code{as_list} on XML response
#' @examples
#' library(xml2)
#' doc = list("Tel-Aviv" = as_xml_document(response_geocode))

"response_geocode"

#' Sample response from Maps Static API (as 'stars' raster)
#'
#' A \code{stars} raster with a static image of Beer-Sheva from the Maps Static API
#'
#' @format A \code{stars} raster with two dimensions \code{x} and \code{y} and a color table
#' @examples
#' library(stars)
#' plot(response_map)

"response_map"
