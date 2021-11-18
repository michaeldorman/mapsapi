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
#' @param avoid \code{NA} (default) or one of: \code{"tolls"}, \code{"highways"}, \code{"ferries"} or \code{"indoor"}
#' @param region The region code, specified as a ccTLD ("top-level domain") two-character value (e.g. \code{"es"} for Spain) (optional)
#' @param traffic_model The traffic model, one of: \code{"best_guess"} (the default), \code{"pessimistic"}, \code{"optimistic"}. The \code{traffic_model} parameter is only taken into account when \code{departure_time} is specified!
#' @param transit_mode Transit preferred mode, one or more of: \code{"bus"}, \code{"subway"}, \code{"train"} or \code{"tram"}
#' @param key Google APIs key
#' @param quiet Logical; suppress printing URL for Google Maps API call (e.g. to hide API key)
#' @return XML document with Google Maps Distance Matrix API response
#' @note Use function \code{\link{mp_get_matrix}} to extract \strong{distance} and \strong{duration} \code{matrix} objects
#' @references \url{https://developers.google.com/maps/documentation/distance-matrix/overview}
#' @export
#' @examples
#' # Built-in reponse example
#' library(xml2)
#' doc = as_xml_document(response_matrix)
#'
#' \dontrun{
#' # Text file with API key
#' key = readLines("~/key")
#'
#' # Using 'data.frame' input
#' doc = mp_matrix(
#'   origins = rbind(c(34.811, 31.892), c(35.212, 31.769)),
#'   destinations = c(34.781, 32.085),
#'   key = key
#' )
#'
#' # Using 'character' input
#' locations = c("Tel-Aviv", "Jerusalem", "Beer-Sheva", "Eilat")
#' doc = mp_matrix(
#'   origins = locations,
#'   destinations = locations,
#'   key = key
#' )
#' 
#' Setting transit modes
#' locations = c("Tel-Aviv", "Beer-Sheva", "Eilat")
#' doc = mp_matrix(
#'   origins = locations,
#'   destinations = locations,
#'   key = key,
#'   mode = "transit",
#'   transit_mode = "train"
#' )
#' 
#' }

mp_matrix = function(
  origins,
  destinations,
  mode = c("driving", "transit", "walking", "bicycling"),
  arrival_time = NULL,
  departure_time = NULL,
  avoid = c(NA, "tolls", "highways", "ferries", "indoor"),
  region = NULL,
  traffic_model = c("best_guess", "pessimistic", "optimistic"),
  transit_mode = c("bus", "subway", "train", "tram"),
  key,
  quiet = FALSE
  ) {

  # Checks
  mode = match.arg(mode)
  avoid = match.arg(avoid)
  traffic_model = match.arg(traffic_model)
  transit_mode = match.arg(transit_mode, several.ok = TRUE)
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
      round(as.numeric(arrival_time))
    )
  }

  # Add 'departure_time'
  if(!is.null(departure_time)) {
    url = paste0(
      url,
      "&departure_time=",
      round(as.numeric(departure_time))
    )
  }

  # Add 'traffic_model'
  if(!is.null(departure_time)) {
    url = paste0(
      url,
      "&traffic_model=",
      traffic_model
    )
  }

  # Add 'avoid'
  if(!is.na(avoid)) {
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

  # Add 'transit_mode'
  if(mode == "transit") {
    url = paste0(
      url,
      "&transit_mode=",
      paste0(transit_mode,collapse = "|")
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

