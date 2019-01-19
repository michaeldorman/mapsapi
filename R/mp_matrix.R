#' Get distance matrix from the Google Maps Distance Matrix API
#' @param origins Origins, as \itemize{
#' \item{\code{character} vector with addresses to be geocoded}
#' \item{\code{numeric} vector of length two (lon, lat)}
#' \item{\code{matrix} with two columns (lon, lat)}
#' \item{\code{sf} or \code{sfc} point layer}
#' }
#' @param destinations Destinations, in one of the same formats as for \code{origins}
#' @param mode Travel mode, one of: \code{"driving"}, \code{"transit"}, \code{"walking"}, \code{"bicycling"}
#' @param arrival_time The desired time of arrival for transit directions, as \code{POSIXct}
#' @param departure_time The desired time of departure, as \code{POSIXct}
#' @param avoid \code{NULL} (default) or one of: \code{"tolls"}, \code{"highways"}, \code{"ferries"} or \code{"indoor"}
#' @param region The region code, specified as a ccTLD ("top-level domain") two-character value (e.g. \code{"es"} for Spain) (optional)
#' @param key Google APIs key (optional)
#' @param quiet Logical; suppress printing URL for Google Maps API call (e.g. to hide API key)
#' @return XML document with Google Maps Distance Matrix API response
#' @note Use function \code{\link{mp_get_matrix}} to extract \strong{distance} and \strong{duration} \code{matrix} objects
#' @references \url{https://developers.google.com/maps/documentation/distance-matrix/intro}
#' @export
#' @examples
#' # Built-in reponse example
#' library(xml2)
#' doc = as_xml_document(response_matrix)
#'
#' \dontrun{
#'
#' # Using 'data.frame' input
#' doc = mp_matrix(
#'   origins = rbind(c(34.81127, 31.89277), c(35.212085, 31.769976)),
#'   destinations = c(34.781107, 32.085003)
#' )
#'
#' # Using 'character' input
#' locations = c("Haifa", "Tel-Aviv", "Jerusalem", "Beer-Sheva")
#' doc = mp_matrix(
#'   origins = locations,
#'   destinations = locations
#' )
#' }

mp_matrix = function(
  origins,
  destinations,
  mode = c("driving", "transit", "walking", "bicycling"),
  arrival_time = NULL,
  departure_time = NULL,
  avoid = NULL,
  region = NULL,
  key = NULL,
  quiet = FALSE
  ) {

  # Checks
  .check_directions_mode(mode[1])
  .check_directions_avoid(avoid)
  .check_posix_time(arrival_time)
  .check_posix_time(departure_time)

  # Origins & Destinations
  origins = encode_locations(origins)
  destinations = encode_locations(destinations)

  # URL & origins and destinations
  url = paste0(
    "https://maps.googleapis.com/maps/api/distancematrix/xml?",
    "origins=",
    origins,
    "&destinations=",
    destinations,
    "&mode=",
    mode[1]
  )

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
      departure_time %>% as.numeric %>% round
    )
  }

  # Add 'avoid'
  if(!is.null(avoid)) {
    url = paste0(
      url,
      "&avoid=",
      avoid
    )
  }

  # Add 'region'
  if(!is.null(region)) {
    url = paste0(
      url,
      "&region=",
      region
    )
  }

  # Add key
  if(!is.null(key)) {
    url = paste0(
      url,
      "&key=",
      key
    )
  }

  # Print URL
  if(!quiet) message(url)

  # Get response
  url = utils::URLencode(url)
  xml2::read_xml(url)

}


