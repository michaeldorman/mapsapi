#' Get directions from the Google Maps Directions API
#' @param origin Origin, as \itemize{
#' \item{\code{character} vector of length one with address to be geocoded}
#' \item{\code{numeric} vector of length two (lon, lat)}
#' \item{\code{matrix} with one row and two columns (lon, lat)}
#' \item{\code{sf} or \code{sfc} point layer with one feature}
#' }
#' @param waypoints Waypoints, in one of the same formats as for \code{origins} but possibly with more than one location, i.e. \itemize{
#' \item{\code{character} vector with addresses to be geocoded}
#' \item{\code{numeric} vector of length two (lon, lat)}
#' \item{\code{matrix} with two columns (lon, lat)}
#' \item{\code{sf} or \code{sfc} point layer}
#' }
#' @param destination Destination, in one of the same formats as for \code{origins}
#' @param mode Travel mode, one of: \code{"driving"} (default), \code{"transit"}, \code{"walking"}, \code{"bicycling"}
#' @param arrival_time The desired time of arrival for transit directions, as \code{POSIXct}
#' @param departure_time The desired time of departure, as \code{POSIXct}
#' @param alternatives Whether to return more than one alternative (\code{logical}, default is \code{FALSE})
#' @param avoid \code{NA} (default, means avoid nothing) or one of: \code{"tolls"}, \code{"highways"}, \code{"ferries"} or \code{"indoor"}
#' @param region The region code, specified as a ccTLD ("top-level domain") two-character value (e.g. \code{"es"} for Spain) (optional)
#' @param traffic_model The traffic model, one of: \code{"best_guess"} (the default), \code{"pessimistic"}, \code{"optimistic"}. The \code{traffic_model} parameter is only taken into account when \code{departure_time} is specified!
#' @param transit_mode Transit preferred mode, one or more of: \code{"bus"}, \code{"subway"}, \code{"train"} or \code{"tram"}
#' @param transit_routing_preference Transit route preference. \code{NA} (default, means no preference) or one of: \code{"less_walking"} or \code{"fewer_transfers"}
#' @param key Google APIs key
#' @param quiet Logical; suppress printing URL for Google Maps API call (e.g. to hide API key)
#' @return XML document with Google Maps Directions API response
#' @note \itemize{
#' \item Use function \code{\link{mp_get_routes}} to extract \code{sf} line layer where each feature is a \strong{route}
#' \item Use function \code{\link{mp_get_segments}} to extract \code{sf} line layer where each feature is a \strong{route segment}
#' }
#' @references \url{https://developers.google.com/maps/documentation/directions/overview}
#' @export
#' @examples
#' # Built-in reponse example
#' library(xml2)
#' doc = as_xml_document(response_directions_driving)
#' r = mp_get_routes(doc)
#' seg = mp_get_segments(doc)
#'
#' \dontrun{
#'
#' # Text file with API key
#' key = readLines("~/key")
#'
#' # Using 'numeric' input
#' doc = mp_directions(
#'   origin = c(34.81127, 31.89277),
#'   destination = c(34.781107, 32.085003),
#'   alternatives = TRUE,
#'   key = key
#' )
#'
#' # Using 'character' and 'sf' input
#' library(sf)
#' doc = mp_directions(
#'   origin = "Beer-Sheva",
#'   destination = c(34.781107, 32.085003) %>% st_point %>% st_sfc(crs = 4326),
#'   alternatives = TRUE,
#'   key = key
#' )
#'
#' # Comparing traffic models
#' doc = mp_directions(
#'   origin = "Beer-Sheva",
#'   destination = "Tel Aviv",
#'   departure_time = Sys.time() + as.difftime(1, units = "hours"),
#'   traffic_model = "best_guess",
#'   key = key
#' )
#' mp_get_routes(doc)$duration_in_traffic_text
#' doc = mp_directions(
#'   origin = "Beer-Sheva",
#'   destination = "Tel Aviv",
#'   departure_time = Sys.time() + as.difftime(1, units = "hours"),
#'   traffic_model = "optimistic",
#'   key = key
#' )
#' mp_get_routes(doc)$duration_in_traffic_text
#' doc = mp_directions(
#'   origin = "Beer-Sheva",
#'   destination = "Tel Aviv",
#'   departure_time = Sys.time() + as.difftime(1, units = "hours"),
#'   traffic_model = "pessimistic",
#'   key = key
#' )
#' mp_get_routes(doc)$duration_in_traffic_text
#' }

mp_directions = function(
  origin,
  waypoints = NULL,
  destination,
  mode = c("driving", "transit", "walking", "bicycling"),
  arrival_time = NULL,
  departure_time = NULL,
  alternatives = FALSE,
  avoid = c(NA, "tolls", "highways", "ferries", "indoor"),
  region = NULL,
  traffic_model = c("best_guess", "pessimistic", "optimistic"),
  transit_mode = c ("bus", "subway", "train", "tram"),
  transit_routing_preference = c(NA, "less_walking", "fewer_transfers"),
  key,
  quiet = FALSE
  ) {

  # Checks
  mode = match.arg(mode)
  avoid = match.arg(avoid)
  traffic_model = match.arg(traffic_model)
  transit_mode = match.arg(transit_mode,several.ok = TRUE)
  transit_routing_preference = match.arg(transit_routing_preference)
  .check_posix_time(arrival_time)
  .check_posix_time(departure_time)

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
    tolower(alternatives)
  )

  # Add 'waypoints'
  if(!is.null(waypoints)) {
    waypoints = encode_locations(waypoints, single = FALSE)
    url = paste0(
      url,
      "&waypoints=optimize:true|",
      waypoints
    )
  }

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

    # Add 'transit_routing_preference'
    if(!is.na(transit_routing_preference)) {
      url = paste0(
        url,
        "&transit_routing_preference=",
        transit_routing_preference
      )
    }
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


