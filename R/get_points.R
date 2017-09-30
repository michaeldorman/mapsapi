#' Extract geocoded points from Google Maps Geocode API response
#' @param doc XML document with Google Maps Geocode API response
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

mp_get_points = function(doc)  {

  # Empty lists for attributes and geometries per address
  geometry = list()
  dat = list()

  # For each XML response / address...
  for(i in 1:length(doc)) {

    # Check status
    status =
      doc %>%
      extract2(i) %>%
      xml_find_all("/GeocodeResponse/status") %>%
      xml_text

    if(status == "OK") {

    # Address from Google
    address_google =
      doc %>%
      extract2(i) %>%
      xml_find_all("/GeocodeResponse/result/formatted_address") %>%
      xml_text

    # Coordinates
    lon =
      doc %>%
      extract2(i) %>%
      xml_find_all("/GeocodeResponse/result/geometry/location/lng") %>%
      xml_text %>%
      as.numeric
    lat =
      doc %>%
      extract2(i) %>%
      xml_find_all("/GeocodeResponse/result/geometry/location/lat") %>%
      xml_text %>%
      as.numeric
    pnt = st_point(c(lon, lat))

    } else {

      # Empty geometry
      pnt = st_point()

      # Empty attributes
      address_google = NA

    }

    # Add attribute and geometry to lists
    geometry[[i]] = pnt
    dat[[i]] = data.frame(
      status = status,
      address = names(doc)[i],
      address_google = address_google,
      stringsAsFactors = FALSE
    )

  }

  # Combine attributes and geometries
  geometry = st_sfc(geometry, crs = 4326)
  dat = do.call(rbind, dat)

  # To 'sf'
  sf::st_sf(dat, geometry)

}


