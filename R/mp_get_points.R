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
#' doc = mp_geocode(addresses = c("Rehovot", "Beer-Sheva", "New-York"))
#' pnt = mp_get_points(doc)
#' }

mp_get_points = function(doc, all_results = FALSE)  {

  # Empty lists for attributes and geometries per address
  geometry = list()
  dat = list()

  # For each XML response / address...
  for(i in 1:length(doc)) {

    # Check status
    status =
      doc %>%
      magrittr::extract2(i) %>%
      xml2::xml_find_all("/GeocodeResponse/status") %>%
      xml2::xml_text()

    if(status == "OK") {

    # Address from Google
    address_google =
      doc %>%
      magrittr::extract2(i) %>%
      xml2::xml_find_all("/GeocodeResponse/result/formatted_address") %>%
      xml2::xml_text()

    # Coordinates
    lon =
      doc %>%
      magrittr::extract2(i) %>%
      xml2::xml_find_all("/GeocodeResponse/result/geometry/location/lng") %>%
      xml2::xml_text() %>%
      as.numeric
    lat =
      doc %>%
      magrittr::extract2(i) %>%
      xml2::xml_find_all("/GeocodeResponse/result/geometry/location/lat") %>%
      xml2::xml_text() %>%
      as.numeric
    coords = cbind(lon, lat)
    coords = split(coords, 1:nrow(coords))
    pnt = lapply(coords, function(x) sf::st_point(x))
    pnt = sf::st_sfc(pnt, crs = 4326)

    } else {

      # Empty geometry
      pnt = st_point()

      # Empty attributes
      address_google = NA

    }

    # Add attribute and geometry to lists
    geometry[[i]] = pnt
    dat[[i]] = data.frame(
      id = i,
      status = status,
      address = names(doc)[i],
      address_google = address_google,
      stringsAsFactors = FALSE
    )

  }

  # Combine attributes and geometries
  geometry = do.call(c, geometry)
  dat = do.call(rbind, dat)

  # To 'sf'
  result = sf::st_sf(dat, geometry)

  # Select just first result
  if(!all_results) {
    result = result[!duplicated(dat$id), ]
  }

  return(result)

}


