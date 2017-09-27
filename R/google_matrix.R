#' Get distance matrix from the Google Maps Distance Matrix API
#' @param origins Origins, as numeric vector of length two (lon, lat)
#' @param destinations Destinations, as numeric vector of length two (lon, lat)
#' @param mode Travel mode, one of: "driving", "transit", "walking", "bicycling"
#' @param alternatives Whether to return more than one alternative (\code{logical})
#' @param key Google APIs key (optional)
#' @return XML document with Google Maps Distance Matrix API response
#' @export
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_matrix)
#' \dontrun{
#' doc = google_matrix(
#'   origins = rbind(c(34.81127, 31.89277), c(35.212085, 31.769976)),
#'   destinations = c(34.781107, 32.085003)
#' )
#' locations = c("Haifa", "Tel-Aviv", "Jerusalem", "Beer-Sheva")
#' doc = google_matrix(
#'   origins = locations,
#'   destinations = locations
#' )
#' }

google_matrix = function(
  origins,
  destinations,
  mode = c("driving", "transit", "walking", "bicycling"),
  alternatives = FALSE,
  key = NULL
  ) {
  origins = encode_locations(origins)
  destinations = encode_locations(destinations)
  url = paste0(
    "https://maps.googleapis.com/maps/api/distancematrix/xml?",
    "origins=",
    origins,
    "&destinations=",
    destinations,
    "&mode=",
    mode[1]
  )
  if(!is.null(key)) {
    url = paste0(
      url,
      "&key=",
      key
    )
  }
  xml2::read_xml(url)
}


