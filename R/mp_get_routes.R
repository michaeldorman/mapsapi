#' Extract *routes* from Google Maps Directions API response
#' @param doc XML document with Google Maps Directions API response
#' @return Line layer (class \code{sf}) representing routes.
#'
#' When document contains no routes ("ZERO_RESULTS" status), the function returns an empty line layer with \code{NA} in all fields.
#' @export
#' @examples
#' library(xml2)
#'
#' doc = as_xml_document(response_directions_driving)
#' r = mp_get_routes(doc)
#' plot(r)
#'
#' doc = as_xml_document(response_directions_transit)
#' r = mp_get_routes(doc)
#' plot(r)
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
#' r = mp_get_routes(doc)
#' plot(r)
#'
#' # Duration in traffic
#' doc = mp_directions(
#'   origin = c(34.81127, 31.89277),
#'   destination = c(34.781107, 32.085003),
#'   departure_time = Sys.time(),
#'   alternatives = TRUE,
#'   key = key
#' )
#' r = mp_get_routes(doc)
#' plot(r)
#'
#' # Using waypoints
#' doc = mp_directions(
#'   origin = c(34.81127, 31.89277),
#'   waypoints = rbind(c(35.01582, 31.90020), c(34.84246, 31.85356)),
#'   destination = c(34.781107, 32.085003),
#'   key = key
#' )
#' r = mp_get_routes(doc)
#' plot(r)
#'
#' }

mp_get_routes = function(doc)  {

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

    # Zero results
    if(alternatives == 0) {

      routes = sf::st_sf(
        alternative_id = NA,
        summary = NA,
        distance_m = NA,
        distance_text = NA,
        duration_s = NA,
        duration_text = NA,
        duration_in_traffic_s = NA,
        duration_in_traffic_text = NA,
        geometry = sf::st_sfc(sf::st_linestring(), crs = 4326),
        stringsAsFactors = FALSE
      )

    } else {

      # Non-zero results
      routes = list()
      for(i in 1:alternatives) {

        route =
          doc |>
          xml2::xml_find_all(sprintf("/DirectionsResponse/route[%s]/overview_polyline/points", i)) |>
          xml2::xml_text()

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

          distance_m =
            doc |>
            xml2::xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg[%s]/distance/value", i, l)) |>
            xml2::xml_text() |>
            as.numeric() |>
            sum()

          distance_text =
            doc |>
            xml2::xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg[%s]/distance/text", i, l)) |>
            xml2::xml_text() |>
            paste(collapse = "|")

          duration_s =
            doc |>
            xml2::xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg[%s]/duration/value", i, l)) |>
            xml2::xml_text() |>
            as.numeric() |>
            sum()

          duration_text =
            doc |>
            xml2::xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg[%s]/duration/text", i, l)) |>
            xml2::xml_text() |>
            paste(collapse = "|")

          duration_in_traffic_s =
            doc |>
            xml2::xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg[%s]/duration_in_traffic/value", i, l)) |>
            xml2::xml_text() |>
            as.numeric()
          if(length(duration_in_traffic_s) == 0)
            duration_in_traffic_s = NA
          if(length(duration_in_traffic_s) > 1)
            duration_in_traffic_s = sum(duration_in_traffic_s)

          duration_in_traffic_text =
            doc |>
            xml2::xml_find_all(sprintf("/DirectionsResponse/route[%s]/leg[%s]/duration_in_traffic/text", i, l)) |>
            xml2::xml_text()
          if(length(duration_in_traffic_text) == 0)
            duration_in_traffic_text = NA
          if(length(duration_in_traffic_text) > 1)
            duration_in_traffic_text = paste(duration_in_traffic_text, collapse = "|")

          # Departure & arrival time
          departure_time =
            doc |>
            xml2::xml_find_all(sprintf(
              "/DirectionsResponse/route[%s]/leg[%s]/departure_time/value",
              i, l)) |>
            xml2::xml_text()
          if(length(departure_time) == 0) {
            departure_time = as.POSIXct(NA)} else {
              departure_time = as.numeric(departure_time)
              departure_time = as.POSIXct(departure_time, tz = "UTC", origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"))
              time_zone =
                doc |>
                xml2::xml_find_all(sprintf(
                  "/DirectionsResponse/route[%s]/leg[%s]/departure_time/time_zone",
                  i, l)) |>
                xml2::xml_text()
              departure_time = format(departure_time, tz = time_zone)
              departure_time = as.POSIXct(departure_time, tz = time_zone)
            }
          arrival_time =
            doc |>
            xml2::xml_find_all(sprintf(
              "/DirectionsResponse/route[%s]/leg[%s]/arrival_time/value",
              i, l)) |>
            xml2::xml_text()
          if(length(arrival_time) == 0) {
            arrival_time = as.POSIXct(NA)} else {
              arrival_time = as.numeric(arrival_time)
              arrival_time = as.POSIXct(arrival_time, tz = "UTC", origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"))
              time_zone =
                doc |>
                xml2::xml_find_all(sprintf(
                  "/DirectionsResponse/route[%s]/leg[%s]/arrival_time/time_zone",
                  i, l)) |>
                xml2::xml_text()
              arrival_time = format(arrival_time, tz = time_zone)
              arrival_time = as.POSIXct(arrival_time, tz = time_zone)
            }

          rt = lapply(route, decode_line)
          rt = lapply(rt, sf::st_linestring)
          rt = lapply(rt, sf::st_sfc, crs = 4326)
          rt = do.call(c, rt)

        routes[[paste(i, l, sep = "-")]] = sf::st_sf(
          alternative_id = i,
          leg_id = l,
          summary = summary,
          distance_m = distance_m,
          distance_text = distance_text,
          duration_s = duration_s,
          duration_text = duration_text,
          duration_in_traffic_s = duration_in_traffic_s,
          duration_in_traffic_text = duration_in_traffic_text,
          departure_time = departure_time,
          arrival_time = arrival_time,
          geometry = rt,
          stringsAsFactors = FALSE
          )

        }

      }
      routes = do.call(rbind, routes)

    }

    return(routes)

  }

}

