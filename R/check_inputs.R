### Geocode

.check_addresses = function(addresses) {
  if(!is.character(addresses)) stop("'addresses' must be a character vector")
}

.check_region = function(region, addresses) {
  if(!is.null(region)) {
    if(!is.character(region))
      stop("'region' must be a character vector")
    if(!(length(region) == 1 | length(region) == length(addresses)))
       stop("'region' must be of length 1 or same length as 'addresses'")
  }
}

.check_bounds_single = function(bounds) {
  if(!(is.na(bounds[1]) & length(bounds) == 1)) {
    if(!is.numeric(bounds))
      stop("'bounds' must be numeric")
    if(length(bounds) != 4)
      stop("'bounds' must be composed of four values xmin/ymin/xmax/ymax (in latitude/longitude)")
  }
}

.check_bounds = function(bounds, addresses) {
  if(!is.null(bounds)) {
    if(is.list(bounds)) {
      sapply(bounds, .check_bounds_single)
    } else {
      .check_bounds_single(bounds)
    }
  }
}

### Directions

.check_matrix_value = function(x) {
  valid = x %in% c("distance_m", "distance_text", "duration_s", "duration_text")
  if(!valid) stop("'value' must be one of: 'distance_m', 'distance_text', 'duration_s', 'duration_text'")
}

.check_directions_mode = function(mode) {
  valid = mode %in% c("driving", "transit", "walking", "bicycling")
  if(!valid) stop("'mode' must be one of: 'driving', 'transit', 'walking', 'bicycling'")
}

.check_directions_avoid = function(avoid) {
  valid = avoid %in% c("tolls", "highways", "ferries", "indoor") || is.null(avoid)
  if(!valid) stop("'avoid' must either 'NULL' or one of: 'tolls', 'highways', 'ferries', 'indoor'")
}

.check_posix_time = function(time) {
  valid = inherits(time, "POSIXt")  || is.null(time)
  if(!valid) stop("Times must be provided as 'POSIXct' or 'POSIXlt' objects")
}

## Map

.check_map_center = function(center) {
  if(class(center)[1] == "character") return(center) else {
    if(inherits(center, c("sfg", "sfc", "sf"))) {
      if(inherits(center, "sfg")) {
        return(st_sfc(center, crs = 4326))
      } else {
        if(is.na(st_crs(center))) {
          st_crs(center) = 4326
          return(center)
        } else {
          return(center)
        }
      }
    }
  }
}

.check_map_zoom = function(zoom) {
  if(!zoom %% 1 == 0) stop("'zoom' must be an integer")
  if(zoom < 0) stop("'zoom' must be positive")
}
