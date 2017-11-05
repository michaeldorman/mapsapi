.check_matrix_value = function(x) {
  valid = x %in% c("distance_m", "distance_text", "duration_s", "duration_text")
  if(!valid) stop("'value' must be one of: 'distance_m', 'distance_text', 'duration_s', 'duration_text'")
}

.check_directions_mode = function(x) {
  valid = x %in% c("driving", "transit", "walking", "bicycling")
  if(!valid) stop("'mode' must be one of: 'driving', 'transit', 'walking', 'bicycling'")
}

.check_directions_avoid = function(x) {
  valid = x %in% c("tolls", "highways", "ferries", "indoor") || is.null(x)
  if(!valid) stop("'avoid' must either 'NULL' or one of: 'tolls', 'highways', 'ferries', 'indoor'")
}

.check_posix_time = function(x) {
  valid = inherits(x, "POSIXt")  || is.null(x)
  if(!valid) stop("Times must be provided as 'POSIXct' or 'POSIXlt' objects")
}
