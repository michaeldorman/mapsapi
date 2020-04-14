## code to prepare `response_map` dataset

library(mapsapi)
library(stars)

key = readLines("~/key")
r = mp_map("31.253205,34.791914", 14, key = key)
st_crs(r)$wkt = gsub("°", "", st_crs(r)$wkt)  ## Remove non-ascii character

response_map = r
usethis::use_data(response_map, overwrite = TRUE)
