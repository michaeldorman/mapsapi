#' Extract distance or duration *matrix* from a Google Maps Distance Matrix API response
#' @param doc XML document with Google Maps Distance Matrix API response
#' @param value Value to extract, one of: \code{"distance_m"} (default), \code{"distance_text"}, \code{"duration_s"}, \code{"duration_text"}
#' @return \code{matrix}
#' @export
#' @examples
#' library(xml2)
#' doc = as_xml_document(response_matrix)
#' extract_matrix(doc, value = "distance_m")
#' extract_matrix(doc, value = "distance_text")
#' extract_matrix(doc, value = "duration_s")
#' extract_matrix(doc, value = "duration_text")
#' \dontrun{
#' locations = c("Haifa", "Tel-Aviv", "Jerusalem", "Beer-Sheva")
#' doc = google_matrix(
#'   origins = locations,
#'   destinations = locations
#' )
#' extract_matrix(doc, value = "distance_m")
#' extract_matrix(doc, value = "distance_text")
#' extract_matrix(doc, value = "duration_s")
#' extract_matrix(doc, value = "duration_text")
#' }

extract_matrix = function(doc, value = "distance_m")  {

  rows =
    doc %>%
    xml_find_all("/DistanceMatrixResponse/row") %>%
    length
  cols =
    doc %>%
    xml_find_all("/DistanceMatrixResponse/row[1]/element") %>%
    length

  origin_addresses =
    doc %>%
    xml_find_all("/DistanceMatrixResponse/origin_address") %>%
    xml_text
  destination_addresses =
    doc %>%
    xml_find_all("/DistanceMatrixResponse/destination_address") %>%
    xml_text

  m = matrix(NA, nrow = rows, ncol = cols)

  for(row in 1:rows) {

    for(col in 1:cols) {

      val = switch(value,

    distance_m =
      doc %>%
      xml_find_all(sprintf("/DistanceMatrixResponse/row[%s]/element[%s]/distance/value", row, col)) %>%
      xml_text %>%
      as.numeric,

    distance_text =
      doc %>%
      xml_find_all(sprintf("/DistanceMatrixResponse/row[%s]/element[%s]/distance/text", row, col)) %>%
      xml_text,

    duration_s =
      doc %>%
      xml_find_all(sprintf("/DistanceMatrixResponse/row[%s]/element[%s]/duration/value", row, col)) %>%
      xml_text %>%
      as.numeric,

    duration_text =
      doc %>%
      xml_find_all(sprintf("/DistanceMatrixResponse/row[%s]/element[%s]/duration/text", row, col)) %>%
      xml_text

      )

    m[row, col] = val

    }

  }

  rownames(m) = origin_addresses
  colnames(m) = destination_addresses

  return(m)

}


