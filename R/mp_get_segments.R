#' Extract *route segments* from a Google Maps Directions API response
#' @param doc XML document with Google Maps Directions API response
#' @return Line layer (class \code{sf}) representing route segments
#' @export
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_directions_driving)
#' seg = mp_get_segments(doc)
#' plot(seg)
#' doc = as_xml_document(response_directions_transit)
#' seg = mp_get_segments(doc)
#' plot(seg)
#' \dontrun{
#' # Transit example
#' doc = mp_directions(
#'   origin = c(34.81127, 31.89277),
#'   destination = c(34.781107, 32.085003),
#'   mode = "transit",
#'   alternatives = TRUE,
#' )
#' seg = mp_get_segments(doc)
#' }

mp_get_segments = function(doc)  {

  # Count alternative routes
  alternatives =
    doc %>%
    xml_find_all("/DirectionsResponse/route") %>%
    length

  routes = list()

  for(i in 1:alternatives) {

    summary =
      doc %>%
      xml_find_all(sprintf("/DirectionsResponse/route[%s]/summary", i)) %>%
      xml_text

    # Count steps per alternative
    steps =
      doc %>%
      xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/step", i)) %>%
      length

    for(j in 1:steps) {

      travel_mode =
        doc %>%
        xml_find_all(sprintf(
          "/DirectionsResponse/route[%s]/leg/step[%s]/travel_mode"
          , i, j)) %>%
        xml_text %>%
        tolower

      step =
        doc %>%
        xml_find_all(sprintf(
          "/DirectionsResponse/route[%s]/leg/step[%s]/polyline/points",
          i, j)) %>%
        xml_text

      instructions =
        doc %>%
        xml_find_all(sprintf(
          "/DirectionsResponse/route[%s]/leg/step[%s]/html_instructions"
          , i, j)) %>%
        xml_text

      distance_m =
        doc %>%
        xml_find_all(sprintf(
          "/DirectionsResponse/route[%s]/leg/step[%s]/distance/value"
          , i, j)) %>%
        xml_text %>%
        as.numeric

      distance_text =
        doc %>%
        xml_find_all(sprintf(
          "/DirectionsResponse/route[%s]/leg/step[%s]/distance/text"
          , i, j)) %>%
        xml_text

      duration_s =
        doc %>%
        xml_find_all(sprintf(
          "/DirectionsResponse/route[%s]/leg/step[%s]/duration/value"
          , i, j)) %>%
        xml_text %>%
        as.numeric

      duration_text =
        doc %>%
        xml_find_all(sprintf(
          "/DirectionsResponse/route[%s]/leg/step[%s]/duration/text"
          , i, j)) %>%
        xml_text

      departure_stop_name =
        doc %>%
        xml_find_all(sprintf(
          "/DirectionsResponse/route[%s]/leg/step[%s]/transit_details/departure_stop/name",
          i, j)) %>%
        xml_text

      rt = decode_line(step)
      rt = sf::st_linestring(rt)
      rt = sf::st_sfc(rt, crs = 4326)

      routes[[paste(i, j, sep = "-")]] = sf::st_sf(
        alternative_id = i,
        segment_id = j,
        summary = summary,
        travel_mode = travel_mode,
        instructions = instructions,
        distance_m = distance_m,
        distance_text = distance_text,
        duration_s = duration_s,
        duration_text = duration_text,
        geomerty = rt,
        stringsAsFactors = FALSE
      )

    }

  }

  routes = do.call(rbind, routes)

  return(routes)

}


