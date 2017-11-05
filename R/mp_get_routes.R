#' Extract *routes* from Google Maps Directions API response
#' @param doc XML document with Google Maps Directions API response
#' @return Line layer (class \code{sf}) representing routes
#' @export
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_directions_driving)
#' r = mp_get_routes(doc)
#' plot(r)
#' doc = as_xml_document(response_directions_transit)
#' r = mp_get_routes(doc)
#' plot(r)
#' \dontrun{
#' # Duration in traffic (only with API key)
#' key = readLines("~/key") # API key
#' doc = mp_directions(
#'   origin = c(34.81127, 31.89277),
#'   destination = c(34.781107, 32.085003),
#'   departure_time = Sys.time(),
#'   alternatives = TRUE,
#'   key = key
#' )
#' r = mp_get_routes(doc)
#' plot(r)
#' }

mp_get_routes = function(doc)  {

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
      xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/distance/value", i)) %>%
      xml_text %>%
      as.numeric

    distance_text =
      doc %>%
      xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/distance/text", i)) %>%
      xml_text

    duration_s =
      doc %>%
      xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/duration/value", i)) %>%
      xml_text %>%
      as.numeric

    duration_text =
      doc %>%
      xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/duration/text", i)) %>%
      xml_text

    duration_in_traffic_s =
      doc %>%
      xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/duration_in_traffic/value", i)) %>%
      xml_text %>%
      as.numeric
    if(length(duration_in_traffic_s) == 0)
      duration_in_traffic_s = NA

    duration_in_traffic_text =
      doc %>%
      xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/duration_in_traffic/text", i)) %>%
      xml_text
    if(length(duration_in_traffic_text) == 0)
      duration_in_traffic_text = NA

    rt = lapply(route, decode_line)
    rt = lapply(rt, sf::st_linestring)
    rt = lapply(rt, sf::st_sfc, crs = 4326)
    rt = do.call(c, rt)

  routes[[i]] = sf::st_sf(
    alternative_id = i,
    summary = summary,
    distance_m = distance_m,
    distance_text = distance_text,
    duration_s = duration_s,
    duration_text = duration_text,
    duration_in_traffic_s = duration_in_traffic_s,
    duration_in_traffic_text = duration_in_traffic_text,
    geomerty = rt,
    stringsAsFactors = FALSE
    )

  }

  routes = do.call(rbind, routes)

  return(routes)

}


