encode_locations = function(x, single = FALSE) {

  if(class(x)[1] == "character") {
    encoded =
      x %>%
      paste0(collapse = "|")
  }

  if(class(x)[1] == "numeric") {
    if(single) stopifnot(length(x) == 2)
    encoded =
      x[2:1] %>%
      paste0(collapse = ",")
  }

  if(inherits(x, c("sfc", "sf"))) {
    x = st_coordinates(x)
  }

  if(class(x)[1] == "matrix") {
    if(single) stopifnot(nrow(x) == 1)
    encoded =
      x[, c(2,1), drop = FALSE] %>%
      apply(1, paste0, collapse = ",") %>%
      paste0(collapse = "|")
  }

  return(encoded)

}
