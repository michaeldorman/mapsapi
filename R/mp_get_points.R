#' Extract geocoded points from Google Maps Geocode API response
#' @param doc XML document with Google Maps Geocode API response
#' @param all_results The geocoder may return several results when address queries are ambiguous. Should all results be returned (\code{TRUE}), or just the first one (\code{FALSE}, default)?
#' @return \code{sf} Point layer representing geocoded locations
#' @export
#' @examples
#' library(xml2)
#' doc = list("Tel-Aviv" = as_xml_document(response_geocode))
#' pnt = mp_get_points(doc)
#' \dontrun{
#' key = readLines("~/key")
#' doc = mp_geocode(addresses = c("Rehovot", "Beer-Sheva", "New-York"), key = key)
#' pnt = mp_get_points(doc)
#' }

mp_get_points = function(doc, all_results = FALSE)  {

  # Empty list for results per address
  result = list()

  # For each XML response / address...
  for(i in 1:length(doc)) {

    # Non-empty document
    if(!is.na(doc[[i]])) {

      # Check status
      status =
        doc[[i]] %>%
        xml2::xml_find_all("/GeocodeResponse/status") %>%
        xml2::xml_text()

    } else {

      status = NA

    }

    if(!is.na(status) & status == "OK") {

      # Address from Google
      address_google =
        doc[[i]] %>%
        xml2::xml_find_all("/GeocodeResponse/result/formatted_address") %>%
        xml2::xml_text()

      # Location type
      location_type =
        doc[[i]] %>%
        xml2::xml_find_all("/GeocodeResponse/result/geometry/location_type") %>%
        xml2::xml_text()

      # Coordinates
      lon =
        doc[[i]] %>%
        xml2::xml_find_all("/GeocodeResponse/result/geometry/location/lng") %>%
        xml2::xml_text() %>%
        as.numeric
      lat =
        doc[[i]] %>%
        xml2::xml_find_all("/GeocodeResponse/result/geometry/location/lat") %>%
        xml2::xml_text() %>%
        as.numeric
      coords = cbind(lon, lat)
      coords = split(coords, 1:nrow(coords))
      pnt = lapply(coords, function(x) sf::st_point(x))
      pnt = sf::st_sfc(pnt, crs = 4326)

    } else {

      # Empty geometry
      pnt = sf::st_point()
      pnt = sf::st_sfc(pnt, crs = 4326)

      # Empty attributes
      address_google = NA
      location_type = NA

    }

    # Combine result
    result[[i]] = sf::st_sf(
      data.frame(
        id = i,
        status = status,
        address = ifelse(!is.null(names(doc)[i]), names(doc)[i], NA),
        address_google = address_google,
        location_type = location_type,
        stringsAsFactors = FALSE
      ),
      pnt
    )

  }

  # Combine results
  result = do.call(rbind, result)

  # Take only first response
  if(!all_results) {
    result = result[!duplicated(result$id), ]
  }

  return(result)

}


