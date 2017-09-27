#' Extract *routes* from Google Maps Directions API response
#' @param doc XML document with Google Maps Directions API response
#' @return Line layer (class \code{sf}) representing routes
#' @export
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_directions)
#' r = extract_routes(doc)
#' plot(r)
#' \dontrun{
#' doc = google_directions(
#'   origin = c(34.81127, 31.89277),
#'   destination = c(34.781107, 32.085003),
#'   alternatives = TRUE
#' )
#' r = extract_routes(doc)
#' plot(r)
#' }

extract_routes = function(doc)  {

  # Count alternative routes
  alternatives =
    doc %>%
    xml_find_all("/DirectionsResponse/route") %>%
    length

  routes = list()

  for(i in 1:alternatives) {

    route =
      doc %>%
      xml_find_all(sprintf("/DirectionsResponse/route[%s]/overview_polyline/points", i)) %>%
      xml_text

    summary =
      doc %>%
      xml_find_all(sprintf("/DirectionsResponse/route[%s]/summary", i)) %>%
      xml_text

    distance_m =
      doc %>%
      xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/step/distance/value", i)) %>%
      xml_text %>%
      as.numeric %>%
      sum

    duration_s =
      doc %>%
      xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/step/duration/value", i)) %>%
      xml_text %>%
      as.numeric %>%
      sum

    rt = lapply(route, decode_line)
    rt = lapply(rt, st_linestring)
    rt = lapply(rt, st_sfc, crs = 4326)
    rt = do.call(c, rt)

  routes[[i]] = st_sf(
    alternative_id = i,
    summary = summary,
    distance_m = distance_m,
    duration_s = duration_s,
    geomerty = rt,
    stringsAsFactors = FALSE
    )

  }

  routes = do.call(rbind, routes)

  return(routes)

}


