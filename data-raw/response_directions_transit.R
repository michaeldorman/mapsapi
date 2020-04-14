## code to prepare `response_directions_transit` dataset

library(mapsapi)
library(xml2)

key = readLines("~/key")
doc = mp_directions(
  origin = c(34.81127, 31.89277),
  destination = "Haifa",
  mode = "transit",
  alternatives = TRUE,
  key = key,
  quiet = TRUE
)
doc = as_list(doc)

response_directions_transit = doc
usethis::use_data(response_directions_transit, overwrite = TRUE)
