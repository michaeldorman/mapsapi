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

.check_posix_time = function(time) {
  valid = inherits(time, "POSIXt")  || is.null(time)
  if(!valid) stop("Times must be provided as 'POSIXct' or 'POSIXlt' objects")
}

## Map

.check_map_center = function(center) {
  if(class(center)[1] == "character") return(center) else {
    if(inherits(center, c("sfg", "sfc", "sf"))) {
      if(inherits(center, "sfg")) {
        return(sf::st_sfc(center, crs = 4326))
      } else {
        if(is.na(sf::st_crs(center))) {
          center = sf::st_set_crs(center, 4326)
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
  if(!(zoom >= 0 && zoom <= 21)) stop("'zoom' must be within 0 and 21")
}

.check_map_size = function (size) {
  if(any(is.na(as.integer(size)))) stop("'size' must be an integer or coercible to one")
  if(!length(size) == 2) stop("'size' must be of length 2")
  if(!all(0 < size & size <= 640)) stop("'size' must be within 0 and 640")
}

.check_map_scale = function (scale) {
  if(!scale %in% 1:2) stop("'scale' must be either 1 or 2")
}

.check_map_style = function(style) {
  valid = inherits(style, 'list') && all(sapply(style, \(x) inherits(x, 'character')))
  if (!valid) stop("'style' should be a list of named character vectors")
}

