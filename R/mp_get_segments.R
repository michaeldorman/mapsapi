#' Extract *route segments* from a Google Maps Directions API response
#' @param doc XML document with Google Maps Directions API response
#' @return Line layer (class \code{sf}) representing route segments
#' @export
#' @examples
#'
#' library(xml2)
#'
#' doc = as_xml_document(response_directions_driving)
#' seg = mp_get_segments(doc)
#' plot(seg)
#'
#' doc = as_xml_document(response_directions_transit)
#' seg = mp_get_segments(doc)
#' plot(seg)
#'
#' \dontrun{
#'
#' # Text file with API key
#' key = readLines("~/key")
#'
#' # Transit example
#' doc = mp_directions(
#'   origin = c(34.81127, 31.89277),
#'   destination = c(34.781107, 32.085003),
#'   mode = "transit",
#'   alternatives = TRUE,
#'   key = key
#' )
#' seg = mp_get_segments(doc)
#' plot(seg)
#'
#' # Using waypoints
#' doc = mp_directions(
#'   origin = c(34.81127, 31.89277),
#'   waypoints = rbind(c(35.01582, 31.90020), c(34.84246, 31.85356)),
#'   destination = c(34.781107, 32.085003),
#'   alternatives = TRUE,
#'   key = key
#' )
#' seg = mp_get_segments(doc)
#' plot(seg)
#'
#' }

mp_get_segments = function(doc)  {

  # Check status
  status = doc |> 
    xml2::xml_find_all("/DirectionsResponse/status") |> 
    xml2::xml_text()
  
  if(status != "OK") stop(sprintf("Google Maps API status is %s", status)) else {

    # Count alternative routes
    alternatives =
      doc |>
      xml2::xml_find_all("/DirectionsResponse/route") |>
      length()

    routes = list()

    for(i in 1:alternatives) {

      summary =
        doc |>
        xml2::xml_find_all(sprintf("/DirectionsResponse/route[%s]/summary", i)) |>
        xml2::xml_text()

      # Count legs
      legs =
        doc |>
        xml2::xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg", i)) |>
        length()

      for(l in 1:legs) {

          # Count steps per alternative per leg
          steps =
            doc |>
            xml2::xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg[%s]/step", i, l)) |>
            length()

          for(j in 1:steps) {

            travel_mode =
              doc |>
              xml2::xml_find_all(sprintf(
                "/DirectionsResponse/route[%s]/leg[%s]/step[%s]/travel_mode"
                , i, l, j)) |>
              xml2::xml_text() |>
              tolower()

            step =
              doc |>
              xml2::xml_find_all(sprintf(
                "/DirectionsResponse/route[%s]/leg[%s]/step[%s]/polyline/points",
                i, l, j)) |>
              xml2::xml_text()

            instructions =
              doc |>
              xml2::xml_find_all(sprintf(
                "/DirectionsResponse/route[%s]/leg[%s]/step[%s]/html_instructions"
                , i, l, j)) |>
              xml2::xml_text()

            # Duration
            distance_m =
              doc |>
              xml2::xml_find_all(sprintf(
                "/DirectionsResponse/route[%s]/leg[%s]/step[%s]/distance/value"
                , i, l, j)) |>
              xml2::xml_text() |>
              as.numeric()
            distance_text =
              doc |>
              xml2::xml_find_all(sprintf(
                "/DirectionsResponse/route[%s]/leg[%s]/step[%s]/distance/text"
                , i, l, j)) |>
              xml2::xml_text()
            duration_s =
              doc |>
              xml2::xml_find_all(sprintf(
                "/DirectionsResponse/route[%s]/leg[%s]/step[%s]/duration/value"
                , i, l, j)) |>
              xml2::xml_text() |>
              as.numeric()
            duration_text =
              doc |>
              xml2::xml_find_all(sprintf(
                "/DirectionsResponse/route[%s]/leg[%s]/step[%s]/duration/text"
                , i, l, j)) |>
              xml2::xml_text()

            # Departure & arrival time
            departure_time =
              doc |>
              xml2::xml_find_all(sprintf(
                "/DirectionsResponse/route[%s]/leg[%s]/step[%s]/transit_details/departure_time/value",
                i, l, j)) |>
              xml2::xml_text()
            if(length(departure_time) == 0) {
              departure_time = as.POSIXct(NA)} else {
                departure_time = as.numeric(departure_time)
                departure_time = as.POSIXct(departure_time, tz = "UTC", origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"))
                time_zone =
                  doc |>
                  xml2::xml_find_all(sprintf(
                    "/DirectionsResponse/route[%s]/leg[%s]/step[%s]/transit_details/departure_time/time_zone",
                    i, l, j)) |>
                  xml2::xml_text()
                departure_time = format(departure_time, tz = time_zone)
                departure_time = as.POSIXct(departure_time, tz = time_zone)
              }
            arrival_time =
              doc |>
              xml2::xml_find_all(sprintf(
                "/DirectionsResponse/route[%s]/leg[%s]/step[%s]/transit_details/arrival_time/value",
                i, l, j)) |>
              xml2::xml_text()
            if(length(arrival_time) == 0) {
              arrival_time = as.POSIXct(NA)} else {
                arrival_time = as.numeric(arrival_time)
                arrival_time = as.POSIXct(arrival_time, tz = "UTC", origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"))
                time_zone =
                  doc |>
                  xml2::xml_find_all(sprintf(
                    "/DirectionsResponse/route[%s]/leg[%s]/step[%s]/transit_details/arrival_time/time_zone",
                    i, l, j)) |>
                  xml2::xml_text()
                arrival_time = format(arrival_time, tz = time_zone)
                arrival_time = as.POSIXct(arrival_time, tz = time_zone)
              }

            rt = decode_line(step)
            rt = sf::st_linestring(rt)
            rt = sf::st_sfc(rt, crs = 4326)

            routes[[paste(i, l, j, sep = "-")]] = sf::st_sf(
              alternative_id = i,
              leg_id = l,
              segment_id = j,
              summary = summary,
              travel_mode = travel_mode,
              instructions = instructions,
              distance_m = distance_m,
              distance_text = distance_text,
              duration_s = duration_s,
              duration_text = duration_text,
              departure_time = departure_time,
              arrival_time = arrival_time,
              geometry = rt,
              stringsAsFactors = FALSE
            )

          }

    }

    }

    routes = do.call(rbind, routes)

    return(routes)

  }

}


