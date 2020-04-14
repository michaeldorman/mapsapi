## code to prepare `response_geocode` dataset

library(mapsapi)
library(xml2)

key = readLines("~/key")
doc = mp_geocode(
  addresses = "Tel-Aviv",
  key = key
)
doc = as_list(doc[[1]])

response_geocode = doc
usethis::use_data(response_geocode, overwrite = TRUE)
