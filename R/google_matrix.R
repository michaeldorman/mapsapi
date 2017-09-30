#' Get distance matrix from the Google Maps Distance Matrix API
#' @param origins Origins, as \itemize{
#' \item{\code{character} vector with addresses to be geocoded}
#' \item{\code{numeric} vector of length two (lon, lat)}
#' \item{\code{matrix} with two columns (lon, lat)}
#' \item{\code{sf} or \code{sfc} point layer}
#' }
#' @param destinations Destinations, in one of the same formats as for \code{origins}
#' @param mode Travel mode, one of: "driving", "transit", "walking", "bicycling"
#' @param alternatives Whether to return more than one alternative (\code{logical})
#' @param key Google APIs key (optional)
#' @return XML document with Google Maps Distance Matrix API response
#' @note Function \code{\link{mp_get_matrix}} can be used to extract distance and duration matrices from returned object
#' @export
#' @examples
#' # Built-in reponse example
#' library(xml2)
#' doc = as_xml_document(response_matrix)
#' \dontrun{
#' Using 'data.frame' input
#' doc = mp_matrix(
#'   origins = rbind(c(34.81127, 31.89277), c(35.212085, 31.769976)),
#'   destinations = c(34.781107, 32.085003)
#' )
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
  alternatives = FALSE,
  key = NULL
  ) {

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

  # Add key
  if(!is.null(key)) {
    url = paste0(
      url,
      "&key=",
      key
    )
  }

  # Get response
  xml2::read_xml(url)

}


