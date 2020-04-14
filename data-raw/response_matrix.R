## code to prepare `response_matrix` dataset

library(mapsapi)
library(xml2)

key = readLines("~/key")
locations = c("Tel-Aviv", "Jerusalem", "Beer-Sheva")
doc = mp_matrix(
  origins = locations,
  destinations = locations,
  key = key,
  quiet = TRUE
)
doc = as_list(doc)

response_matrix = doc
usethis::use_data(response_matrix, overwrite = TRUE)
