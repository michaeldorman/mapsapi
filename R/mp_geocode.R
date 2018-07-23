#' Get geocoded coordinates using the Google Maps Geocoding API
#' @param addresses Addresses to geocode, as \code{character} vector
#' @param region The region code, specified as a ccTLD ("top-level domain") two-character value (e.g. \code{"es"} for Spain). This can to be a character vector of length 1 (in which case it is replicated) or a character vector with the same length of \code{addresses} (optional)
#' @param bounds A preferred bounding box, specified as a numeric vector with four values xmin/ymin/xmax/ymax (in latitude/longitude) representing the coordinates of the southwest and northeast corners, e.g. as returned by function `sf::st_bbox`. This can be a single vector (in which case it is replicated) or a \code{list} of numeric vectors with the same length as \code{addresses} (optional)
#' @param key Google APIs key (optional)
#' @return \code{list} of XML documents with Google Maps Geocoding API responses, one item per element in \code{addresses}
#' @note \itemize{
#' \item Use function \code{\link{mp_get_points}} to extract \strong{locations} as \code{sf} point layer
#' \item Use function \code{\link{mp_get_bounds}} to extract \strong{location bounds} as \code{sf} polygonal layer
#' }
#' @references \url{https://developers.google.com/maps/documentation/geocoding/intro}
#' @export
#' @encoding UTF-8
#' @examples
#'
#' # Built-in reponse example
#' library(xml2)
#' doc = list("Tel-Aviv" = as_xml_document(response_geocode))
#' pnt = mp_get_points(doc)
#' bounds = mp_get_bounds(doc)
#'
#' \dontrun{
#'
#' # Basic use
#' key = readLines("~/key") # Text file with API key
#' addresses = c("Rehovot", "Beer-Sheva", "New-York")
#' doc = mp_geocode(addresses) # without key
#' doc = mp_geocode(addresses, key = key) # with key
#' pnt = mp_get_points(doc)
#' pnt
#'
#' # Using the 'region' parameter
#' doc = mp_geocode(addresses = "Toledo", key = key)
#' mp_get_points(doc)
#' doc = mp_geocode(addresses = "Toledo", region = "es", key = key)
#' mp_get_points(doc)
#'
#' # Various addresses
#' addresses = c(
#'   "Baker Street 221b, London",
#'   "Brandenburger Tor, Berlin",
#'   "",
#'   "Platz der Deutschen Einheit 1, Hamburg",
#'   "Arc de Triomphe de l'Etoile, Paris",
#'   NA
#' )
#' doc = mp_geocode(addresses, key = key)
#' pnt = mp_get_points(doc)
#' pnt
#'
#' # Specifying a bounding box
#' b = c(-118.604794, 34.172684, -118.500938, 34.236144) # Bounds as xmin/ymin/xmax/ymax
#' result = mp_geocode(addresses = "Winnetka", key = key)
#' mp_get_points(result)
#' result = mp_geocode(addresses = "Winnetka", bounds = b, key = key)
#' mp_get_points(result)
#' result = mp_geocode(addresses = rep("Winnetka", 3), bounds = list(b, NA, b), key = key)
#' mp_get_points(result)
#'
#' }

mp_geocode = function(
  addresses,
  region = NULL,
  bounds = NULL,
  key = NULL
  ) {

  # Checks
  .check_addresses(addresses)
  .check_region(region, addresses)
  .check_bounds(bounds, addresses)

  # Replicate region/bounds if necessary
  if(length(region) == 1) region = rep(region, length(addresses))
  if(!is.null(bounds) & !is.list(bounds)) {
    bounds = list(bounds)
    bounds = rep(bounds, length(addresses))
  }

  # Remove invalid addresses
  addresses[addresses == ""] = NA

  # Empty list to hold API responses
  response = list()

  # For each address
  for(i in 1:length(addresses)) {

    # Address is missing
    if(is.na(addresses[i])) {

      # Empty response
      response[[i]] = NA

      # 'status' to print
      status = NULL

    } else {

      address = encode_locations(addresses[i], single = TRUE)

      # URL & address
      url = paste0(
        "https://maps.googleapis.com/maps/api/geocode/xml?",
        "address=",
        address
      )

      # Region
      if(!is.null(region)) {
        url = paste0(
          url,
          "&region=",
          region[i]
        )
      }

      # Viewport/Bounding Box Biasing
      if(!is.null(bounds)) {
        url = paste0(
          url,
          "&bounds=",
          encode_bounds(
            c(
              bounds[[i]][2],
              bounds[[i]][1],
              bounds[[i]][4],
              bounds[[i]][3]
            )
          )
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

      # Get response
      url = utils::URLencode(url)
      response[[i]] = xml2::read_xml(url)

      # 'status' to print
      status =
        response[[i]] %>%
        xml2::xml_find_all("/GeocodeResponse/status") %>%
        xml2::xml_text()

    }

    # Print current progress
    address_char = nchar(addresses[i]); if(is.na(address_char)) address_char = 2
    dots = max(c(1, 40 - address_char))
    cat(paste0(addresses[i], paste0(rep(".", dots), collapse = "")))
    if(!is.null(status)) cat(status)
    cat("\n")

    # Wait 1 seconds to avoid rate limit (50 requests per minute)
    if(length(addresses > 1)) Sys.sleep(1)

  }

  # Set list names
  names(response)[!is.na(addresses)] = addresses[!is.na(addresses)]

  # Returned object
  return(response)

}


