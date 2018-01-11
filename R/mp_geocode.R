#' Get geocoded coordinates using the Google Maps Geocoding API
#' @param addresses Addresses to geocode, as \code{character} vector
#' @param region The region code, specified as a ccTLD ("top-level domain") two-character value (e.g. \code{"es"} for Spain) (optional)
#' @param key Google APIs key (optional)
#' @return \code{list} of XML documents with Google Maps Geocoding API responses, one item per element in \code{addresses}
#' @note \itemize{
#' \item Use function \strong{\code{\link{mp_get_points}}} to extract \strong{locations} as \code{sf} point layer
#' \item Use function \strong{\code{\link{mp_get_bounds}}} to extract \strong{location bounds} as \code{sf} polygonal layer
#' }
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
#' doc = mp_geocode(addresses = c("Rehovot", "Beer-Sheva", "New-York"))
#'
#' # Using the 'region' parameter
#' doc = mp_geocode(addresses = "Toledo")
#' mp_get_points(doc)
#' doc = mp_geocode(addresses = "Toledo", region = "es")
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
#' doc = mp_geocode(addresses)
#' pnt = mp_get_points(doc)
#'
#' # Using API key
#' key = readLines("~/key") # Text file with API key
#' doc = mp_geocode(addresses, key = key)
#' pnt = mp_get_points(doc)
#'
#' }

mp_geocode = function(
  addresses,
  region = NULL,
  key = NULL
  ) {

  # Replicate region if necessary
  if(length(region) == 1) region = rep(region, length(addresses))

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


