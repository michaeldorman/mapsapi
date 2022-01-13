#' Extract distance or duration *matrix* from a Google Maps Distance Matrix API response
#' @param doc XML document with Google Maps Distance Matrix API response
#' @param value Value to extract, one of: \code{"distance_m"} (the default), \code{"distance_text"}, \code{"duration_s"}, \code{"duration_text"}, \code{"duration_in_traffic_s"}, \code{"duration_in_traffic_text"}
#' @return A \code{matrix}, where rows represent origins and columns represent destinations. Matrix values are according to selected \code{value}, or \code{NA} if the API returned zero results
#' @note The \code{"duration_in_traffic_s"} and \code{"duration_in_traffic_text"} options are only applicable when the API response contains these fields, i.e., when using \code{\link{mp_matrix}} with \code{mode="driving"}, with \code{departure_time} specified, and API key \code{key} provided
#' @export
#' @examples
#'
#' library(xml2)
#' doc = as_xml_document(response_matrix)
#' mp_get_matrix(doc, value = "distance_m")
#' mp_get_matrix(doc, value = "distance_text")
#' mp_get_matrix(doc, value = "duration_s")
#' mp_get_matrix(doc, value = "duration_text")
#'
#' \dontrun{
#' # Text file with API key
#' key = readLines("~/key")
#'
#' locations = c("Tel-Aviv", "Jerusalem", "Neve Shalom")
#'
#' # Driving times
#' doc = mp_matrix(
#'   origins = locations,
#'   destinations = locations,
#'   mode = "driving",
#'   departure_time = Sys.time() + as.difftime(10, units = "mins"),
#'   key = key
#' )
#' mp_get_matrix(doc, value = "distance_m")
#' mp_get_matrix(doc, value = "distance_text")
#' mp_get_matrix(doc, value = "duration_s")
#' mp_get_matrix(doc, value = "duration_text")
#' mp_get_matrix(doc, value = "duration_in_traffic_s")
#' mp_get_matrix(doc, value = "duration_in_traffic_text")
#'
#' # Public transport times
#' doc = mp_matrix(
#'   origins = locations,
#'   destinations = locations,
#'   mode = "transit",
#'   key = key
#' )
#' mp_get_matrix(doc, value = "distance_m")
#' mp_get_matrix(doc, value = "distance_text")
#' mp_get_matrix(doc, value = "duration_s")
#' mp_get_matrix(doc, value = "duration_text")
#'
#' }

mp_get_matrix = function(doc, value = c("distance_m", "distance_text", "duration_s", "duration_text", "duration_in_traffic_s", "duration_in_traffic_text"))  {

  value = match.arg(value)

  rows =
    doc |>
    xml2::xml_find_all("/DistanceMatrixResponse/row") |>
    length()
  cols =
    doc |>
    xml2::xml_find_all("/DistanceMatrixResponse/row[1]/element") |>
    length()

  origin_addresses =
    doc |>
    xml2::xml_find_all("/DistanceMatrixResponse/origin_address") |>
    xml2::xml_text()
  destination_addresses =
    doc |>
    xml2::xml_find_all("/DistanceMatrixResponse/destination_address") |>
    xml2::xml_text()

  m = matrix(NA, nrow = rows, ncol = cols)

  for(row in 1:rows) {

    for(col in 1:cols) {

      val = switch(value,

        distance_m =
          doc |>
          xml2::xml_find_all(sprintf("/DistanceMatrixResponse/row[%s]/element[%s]/distance/value", row, col)) |>
          xml2::xml_text() |>
          as.numeric(),

        distance_text =
          doc |>
          xml2::xml_find_all(sprintf("/DistanceMatrixResponse/row[%s]/element[%s]/distance/text", row, col)) |>
          xml2::xml_text(),

        duration_s =
          doc |>
          xml2::xml_find_all(sprintf("/DistanceMatrixResponse/row[%s]/element[%s]/duration/value", row, col)) |>
          xml2::xml_text() |>
          as.numeric(),

        duration_text =
          doc |>
          xml2::xml_find_all(sprintf("/DistanceMatrixResponse/row[%s]/element[%s]/duration/text", row, col)) |>
          xml2::xml_text(),

        duration_in_traffic_s =
          doc |>
          xml2::xml_find_all(sprintf("/DistanceMatrixResponse/row[%s]/element[%s]/duration_in_traffic/value", row, col)) |>
          xml2::xml_text() |>
          as.numeric(),

        duration_in_traffic_text =
          doc |>
          xml2::xml_find_all(sprintf("/DistanceMatrixResponse/row[%s]/element[%s]/duration_in_traffic/text", row, col)) |>
          xml2::xml_text()

      )

      # "Zero results"
      if(length(val) == 0) val = NA

      # Fill matrix value
      m[row, col] = val

    }

  }

  rownames(m) = origin_addresses
  colnames(m) = destination_addresses

  return(m)

}

