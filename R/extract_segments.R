#' Extract *route segments* from a Google Maps Directions API response
#' @param doc XML document with Google Maps Directions API response
#' @return Line layer (class \code{sf}) representing route segments
#' @export
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_directions)
#' seg = extract_segments(doc)
#' plot(seg)
#' \dontrun{
#' doc = google_directions(
#'   origin = c(34.81127, 31.89277),
#'   destination = c(34.781107, 32.085003),
#'   alternatives = TRUE
#' )
#' seg = extract_segments(doc)
#' }

extract_segments = function(doc)  {

  # Count alternative routes
  alternatives =
    doc %>%
    xml_find_all("//route") %>%
    length

  routes = list()

  for(i in 1:alternatives) {

  summary =
    doc %>%
    xml_find_all(sprintf("/DirectionsResponse/route[%s]/summary", i)) %>%
    xml_text

  steps =
    doc %>%
    xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/step/polyline/points", i)) %>%
    xml_text

  instructions =
    doc %>%
    xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/step/html_instructions", i)) %>%
    xml_text

  distance_m =
    doc %>%
    xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/step/distance/value", i)) %>%
    xml_text %>%
    as.numeric

  distance_text =
    doc %>%
    xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/step/distance/text", i)) %>%
    xml_text

  duration_s =
    doc %>%
    xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/step/duration/value", i)) %>%
    xml_text %>%
    as.numeric

  duration_text =
    doc %>%
    xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg/step/duration/text", i)) %>%
    xml_text

  rt = lapply(steps, decode_line)
  rt = lapply(rt, st_linestring)
  rt = lapply(rt, st_sfc, crs = 4326)
  rt = do.call(c, rt)

  routes[[i]] = st_sf(
    alternative_id = i,
    segment_id = 1:length(rt),
    summary = summary,
    instructions = instructions,
    distance_m = distance_m,
    distance_text = distance_text,
    duration_s = duration_s,
    duration_text = duration_text,
    geomerty = rt,
    stringsAsFactors = FALSE
    )

  }

  routes = do.call(rbind, routes)

  return(routes)

}


