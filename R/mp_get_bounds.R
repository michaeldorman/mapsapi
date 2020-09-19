#' Extract geocoded *bounds* from Google Maps Geocode API response
#' @param doc XML document with Google Maps Geocode API response
#' @return \code{sf} Polygonal layer representing bounds of geocoded locations
#' @export
#' @examples
#' library(xml2)
#' doc = list("Tel-Aviv" = as_xml_document(response_geocode))
#' b = mp_get_bounds(doc)
#' \dontrun{
#' doc = mp_geocode(addresses = c("Tel-Aviv", "Rehovot", "Beer-Sheva"))
#' b = mp_get_bounds(doc)
#' }

mp_get_bounds = function(doc)  {

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

      # Bounds
      xmin =
        doc %>%
        extract2(i) %>%
        xml_find_all("/GeocodeResponse/result/geometry/bounds/southwest/lng") %>%
        xml_text %>%
        as.numeric
      ymin =
        doc %>%
        extract2(i) %>%
        xml_find_all("/GeocodeResponse/result/geometry/bounds/southwest/lat") %>%
        xml_text %>%
        as.numeric
      xmax =
        doc %>%
        extract2(i) %>%
        xml_find_all("/GeocodeResponse/result/geometry/bounds/northeast/lng") %>%
        xml_text %>%
        as.numeric
      ymax =
        doc %>%
        extract2(i) %>%
        xml_find_all("/GeocodeResponse/result/geometry/bounds/northeast/lat") %>%
        xml_text %>%
        as.numeric
      lowerleft = st_point(c(xmin, ymin))
      upperleft = st_point(c(xmin, ymax))
      lowerright = st_point(c(xmax, ymin))
      upperright = st_point(c(xmax, ymax))
      bounds = c(lowerleft, upperleft, upperright, lowerright)
      bounds = st_cast(bounds, "POLYGON")

    } else {

      # Empty geometry
      bounds = st_polygon()

      # Empty attributes
      address_google = NA

    }

    # Add attribute and geometry to lists
    geometry[[i]] = bounds
    dat[[i]] = data.frame(
      status = status,
      address = ifelse(!is.null(names(doc)[i]), names(doc)[i], NA),
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


