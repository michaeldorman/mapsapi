#' Sample response from Google Maps Directions API
#'
#' An XML document with driving directions from Tel-Aviv to Haifa
#'
#' @format A \code{character} vector of length one
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_directions)

"response_directions"

#' Sample response from Google Maps Distance Matrix API
#'
#' An XML document with a distance matrix for driving between three locations: Tel-Aviv, Jerusalem and Beer-Sheva
#'
#' @format A \code{character} vector of length one
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_matrix)

"response_matrix"

