#' Extract geocoded *bounds* from Google Maps Geocode API response
#' @param doc XML document with Google Maps Geocode API response
#' @return \code{sf} Polygonal layer representing bounds of geocoded locations. In cases when there is more than one response per address, only first response is considered.
#' @export
#' @examples
#' 
#' # Built-in reponse example
#' library(xml2)
#' doc = list("Tel-Aviv" = as_xml_document(response_geocode))
#' b = mp_get_bounds(doc)
#' 
#' \dontrun{
#' 
#' # Text file with API key
#' key = readLines("~/key")
#' 
#' # Get bounds
#' doc = mp_geocode(addresses = c("Tel-Aviv", "Rehovot", "Beer-Sheva"), region = "il", key = key)
#' b = mp_get_bounds(doc)
#' 
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

      # Take only first response
      if(length(xmin) > 1) xmin = xmin[1]
      if(length(ymin) > 1) ymin = ymin[1]
      if(length(xmax) > 1) xmax = xmax[1]
      if(length(ymax) > 1) ymax = ymax[1]
      
      if(length(c(xmin, ymin, xmax, ymax) == 4)) {
      
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
    } else {
      
      # Empty geometry
      bounds = st_polygon()
      
      # Empty attributes
      address_google = NA
    
    }

    # Add attribute and geometry to lists
    geometry[[i]] = bounds
    dat[[i]] = data.frame(
      id = i,
      status = status,
      address = ifelse(!is.null(names(doc)[i]), names(doc)[i], NA),
      address_google = address_google,
      stringsAsFactors = FALSE
    )

  }

  # Take only first response
  for(i in 1:length(doc)) {
    dat[[i]] = dat[[i]][1, ]
  }

  # Combine attributes and geometries
  geometry = st_sfc(geometry, crs = 4326)
  dat = do.call(rbind, dat)

  # To 'sf'
  sf::st_sf(dat, geometry)
  
}


