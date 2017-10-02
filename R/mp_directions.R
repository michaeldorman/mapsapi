#' Get directions from the Google Maps Directions API
#' @param origin Origin, as \itemize{
#' \item{\code{character} vector of length one with address to be geocoded}
#' \item{\code{numeric} vector of length two (lon, lat)}
#' \item{\code{matrix} with one row and two columns (lon, lat)}
#' \item{\code{sf} or \code{sfc} point layer with one feature}
#' }
#' @param destination Destination, in one of the same formats as for \code{origins}
#' @param mode Travel mode, one of: \code{"driving"} (default), \code{"transit"}, \code{"walking"}, \code{"bicycling"}
#' @param arrival_time The desired time of arrival for transit directions, as \code{POSIXct}
#' @param departure_time The desired time of departure, as \code{POSIXct}
#' @param alternatives Whether to return more than one alternative (\code{logical})
#' @param avoid \code{NULL} (default) or one of: \code{"tolls"}, \code{"highways"}, \code{"ferries"}
#' @param key Google APIs key (optional)
#' @return XML document with Google Maps Directions API response
#' @note Functions \code{\link{mp_get_routes}} and \code{\link{mp_get_segments}} can be used to extract routes as \code{sf} line layers from returned object
#' @import magrittr
#' @import sf
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @export
#' @examples
#' # Built-in reponse example
#' library(xml2)
#' doc = as_xml_document(response_directions)
#' r = mp_get_routes(doc)
#' seg = mp_get_segments(doc)
#' \dontrun{
#' # Using 'numeric' input
#' doc = mp_directions(
#'   origin = c(34.81127, 31.89277),
#'   destination = c(34.781107, 32.085003),
#'   alternatives = TRUE
#' )
#' # Using 'character' and 'sf' input
#' doc = mp_directions(
#'   origin = "Beer-Sheva",
#'   destination = c(34.781107, 32.085003) %>% st_point %>% st_sfc(crs = 4326),
#'   alternatives = TRUE
#' )
#' }

mp_directions = function(
  origin,
  destination,
  mode = c("driving", "transit", "walking", "bicycling"),
  arrival_time = NULL,
  departure_time = NULL,
  alternatives = FALSE,
  avoid = NULL,
  key = NULL
  ) {

  # Origin & Destination
  origin = encode_locations(origin, single = TRUE)
  destination = encode_locations(destination, single = TRUE)

  # URL & origin and destination
  url = paste0(
    "https://maps.googleapis.com/maps/api/directions/xml?",
    "origin=",
    origin,
    "&destination=",
    destination,
    "&mode=",
    mode[1],
    "&alternatives=",
    tolower(alternatives))

  # Add key
  if(!is.null(key)) {
    url = paste0(
      url,
      "&key=",
      key
    )
  }

  # Add 'arrival_time'
  if(!is.null(arrival_time)) {
    url = paste0(
      url,
      "&arrival_time=",
      arrival_time %>% as.numeric %>% round
    )
  }

  # Add 'departure_time'
  if(!is.null(departure_time)) {
    url = paste0(
      url,
      "&departure_time=",
      departure_time %>% as.numeric
    )
  }

  # Get response
  xml2::read_xml(url)

}


