#' Get directions from the Google Maps Directions API
#' @param origin Origin, as \itemize{
#' \item{\code{character} vector of length one with address to be geocoded}
#' \item{\code{numeric} vector of length two (lon, lat)}
#' \item{\code{matrix} with one row and two columns (lon, lat)}
#' \item{\code{sf} or \code{sfc} layer with one feature}
#' }
#' @param destination Destination, as numeric vector of length two (lon, lat)
#' @param mode Travel mode, one of: \code{"driving"} (default), \code{"transit"}, \code{"walking"}, \code{"bicycling"}
#' @param alternatives Whether to return more than one alternative (\code{logical})
#' @param avoid \code{NULL} (default) or one of: \code{"tolls"}, \code{"highways"}, \code{"ferries"}
#' @param key Google APIs key (optional)
#' @return XML document with Google Maps Directions API response
#' @import magrittr
#' @import sf
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @export
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_directions)
#' \dontrun{
#' doc = google_directions(
#'   origin = c(34.81127, 31.89277),
#'   destination = c(34.781107, 32.085003),
#'   alternatives = TRUE
#' )
#' doc = google_directions(
#'   origin = "Beer-Sheva",
#'   destination = c(34.781107, 32.085003) %>% st_point %>% st_sfc(crs = 4326),
#'   alternatives = TRUE
#' )
#' }

google_directions = function(
  origin,
  destination,
  mode = c("driving", "transit", "walking", "bicycling"),
  alternatives = FALSE,
  avoid = NULL,
  key = NULL
  ) {
  origin = encode_locations(origin, single = TRUE)
  destination = encode_locations(destination, single = TRUE)
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
  if(!is.null(key)) {
    url = paste0(
      url,
      "&key=",
      key
    )
  }
  xml2::read_xml(url)
}


