#' Get static map from the Maps Static API
#'
#' Download a static map from the Maps Static API, given map center and zoom level.
#'
#' @param center Character of length 1 of the form \code{"lat,lon"} or a geometry of class \code{sfg}, \code{sfc} or \code{sf}. If \code{center} is a geometry, the center of the geometry bounding box is passed as map center. Missing Coordinate Reference System (CRS) is assumed WGS84.
#' @param zoom Zoom level, a positive integer or zero. The appropriate range is \code{0} to \code{20}.
#' @param maptype Map type, one of: \code{"roadmap"}, \code{"satellite"}, \code{"terrain"}, \code{"hybrid"}.
#' @param key Google APIs key
#' @param quiet Logical; suppress printing URL for Google Maps API call (e.g. to hide API key)
#' @return A \code{stars} raster with the requested map, in Web Mercator CRS (EPSG:3857).
#' @references \url{https://developers.google.com/maps/documentation/maps-static/overview}
#' @export
#' @examples
#'
#' \dontrun{
#'
#' library(stars)
#' key = readLines("~/key")
#'
#' # Using coordinates
#' r = mp_map("31.253205,34.791914", 14, key = key)
#' plot(r)
#'
#' # Using 'sfc' point - WGS84
#' pnt = st_point(c(34.791914, 31.253205))
#' pnt = st_sfc(pnt, crs = 4326)
#' r = mp_map(pnt, 14, key = key)
#' plot(r)
#'
#' # Using 'sfc' point - UTM
#' pnt = st_point(c(34.791914, 31.253205))
#' pnt = st_sfc(pnt, crs = 4326)
#' pnt = st_transform(pnt, 32636)
#' r = mp_map(pnt, 14, key = key)
#' plot(r)
#'
#' # Using 'sfc' polygon
#' pnt = st_point(c(34.791914, 31.253205))
#' pnt = st_sfc(pnt, crs = 4326)
#' pol = st_buffer(pnt, 0.01)
#' r = mp_map(pol, 14, key = key)
#' plot(r)
#'
#' # 'ggplot2'
#' library(ggplot2)
#' cols = attr(r[[1]], "colors")
#' ggplot() +
#'   geom_stars(data = r, aes(x = x, y = y, fill = color)) +
#'   scale_fill_manual(values = cols, guide = FALSE) +
#'   coord_sf()
#'
#' # 'ggplot2' - map types
#' r1 = mp_map(pnt, 14, maptype = "roadmap", key = key)
#' r2 = mp_map(pnt, 14, maptype = "satellite", key = key)
#' r3 = mp_map(pnt, 14, maptype = "terrain", key = key)
#' r4 = mp_map(pnt, 14, maptype = "hybrid", key = key)
#' cols1 = attr(r1[[1]], "colors")
#' cols2 = attr(r2[[1]], "colors")
#' cols3 = attr(r3[[1]], "colors")
#' cols4 = attr(r4[[1]], "colors")
#' theme1 = theme(
#'   axis.text = element_blank(),
#'   axis.title = element_blank(),
#'   axis.ticks = element_blank()
#' )
#' g1 = ggplot() +
#'   geom_stars(data = r1, aes(x = x, y = y, fill = color)) +
#'   scale_fill_manual(values = cols1, guide = FALSE) +
#'   coord_sf() +
#'   ggtitle("roadmap") +
#'   theme1
#' g2 = ggplot() +
#'   geom_stars(data = r2, aes(x = x, y = y, fill = color)) +
#'   scale_fill_manual(values = cols2, guide = FALSE) +
#'   coord_sf() +
#'   ggtitle("satellite") +
#'   theme1
#' g3 = ggplot() +
#'   geom_stars(data = r3, aes(x = x, y = y, fill = color)) +
#'   scale_fill_manual(values = cols3, guide = FALSE) +
#'   coord_sf() +
#'   ggtitle("terrain") +
#'   theme1
#' g4 = ggplot() +
#'   geom_stars(data = r4, aes(x = x, y = y, fill = color)) +
#'   scale_fill_manual(values = cols4, guide = FALSE) +
#'   coord_sf() +
#'   ggtitle("hybrid") +
#'   theme1
#' g1 + g2 + g3 + g4
#'
#' }

mp_map = function(
  center,
  zoom,
  maptype = c("roadmap", "satellite", "terrain", "hybrid"),
  key,
  quiet = FALSE
) {

  # Checks
  center = .check_map_center(center)
  .check_map_zoom(zoom)

  # Map type
  maptype = match.arg(maptype)

  # Location to center
  if("character" %in% class(center)) {
    coords = strsplit(center, ",")
    coords = coords[[1]]
    coords = as.numeric(coords)
    coords = rev(coords)
  } else {
    center = sf::st_geometry(center)
    center = sf::st_bbox(center)
    center = sf::st_as_sfc(center)
    center = sf::st_transform(center, 3857)
    center = sf::st_centroid(center)
    center = sf::st_transform(center, 4326)
    coords = sf::st_coordinates(center)
    center = paste0(coords[1, 2], ",", coords[1, 1])
  }

  # URL, center, zoom & maptype
  url = paste0(
    "https://maps.googleapis.com/maps/api/staticmap?",
    "size=640x640&scale=2&",
    "center=",
    center,
    "&zoom=",
    zoom,
    "&maptype=",
    maptype
  )

  # Add key
  if(!is.null(key)) {
    url = paste0(
      url,
      "&key=",
      key
    )
  }

  # Print URL
  if(!quiet) message(url)

  # Get response
  url = utils::URLencode(url)
  filename = tempfile(fileext = ".png")
  utils::download.file(url, filename, mode = "wb", quiet = quiet)

  # Process to raster
  r = stars::read_stars(filename)
  names(r) = "color"

  # Get bounding box
  size = c(640, 640)
  ll = RgoogleMaps::XY2LatLon(
    list(lat = coords[2], lon = coords[1], zoom = zoom),
    -size[1]/2,
    -size[2]/2
  )
  ur = RgoogleMaps::XY2LatLon(
    list(lat = coords[2], lon = coords[1], zoom = zoom),
    size[1]/2,
    size[2]/2
  )
  pnt_ll = sf::st_point(c(ll[2], ur[1]))
  pnt_ur = sf::st_point(c(ur[2], ll[1]))
  p = c(pnt_ll, pnt_ur)
  bb = sf::st_bbox(p)
  bb = sf::st_as_sfc(bb)
  bb = sf::st_set_crs(bb, 4326)
  bb = sf::st_transform(bb, 3857)
  bb1 = sf::st_bbox(bb)

  # Set spatial properties
  attr(r, "dimensions")[["x"]][["offset"]] = bb1[1]
  attr(r, "dimensions")[["y"]][["offset"]] = bb1[4]
  attr(r, "dimensions")[["x"]][["delta"]] = (bb1[3]-bb1[1])/dim(r)["x"]
  attr(r, "dimensions")[["y"]][["delta"]] = -(bb1[4]-bb1[2])/dim(r)["y"]
  r = sf::st_set_crs(r, 3857)

  # Set class
  class(r) = c("mapsapi_map", class(r))

  # Return
  return(r)

}

#' Plot static Google map
#'
#' Plot method for static maps using function \code{\link{mp_map}}.
#'
#' @param x Map object of class \code{stars} and \code{mapsapi_map} obtained from function \code{\link{mp_map}}
#' @param ... Further arguments passed to \code{plot.stars}

#' @method plot mapsapi_map
#' @importFrom graphics plot
#' @export

plot.mapsapi_map = function(x, ...) {
  class(x) = "stars"
  plot(x, key.pos = NULL, main = NULL, ...)
}
