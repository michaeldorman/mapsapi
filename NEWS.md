## mapsapi 0.1.0 (2017-09-27)

* Added 'Directions' API interface
* Added 'Distance Matrix' API interface

## mapsapi 0.2.0 (2017-11-02)

* Added 'Geocode' API interface
* Added input checks
* Added arrival & departure time parameters

## mapsapi 0.3.0 (2018-01-11)

* Added delay in 'mp_geocode' to avoid rate limit
* Fixed index issue in 'mp_geocode'
* Fixed 'mp_get_points' issue with multiple points

## mapsapi 0.3.5 (2018-03-12)

* Added 'bounds' parameter for 'mp_geocode'
* Added argument validation in 'mp_geocode'
* Corrected typo when creating geometry column in 'mp_get_routes' and 'mp_get_segments'

## mapsapi 0.3.7 (2018-07-23)

* Minor updates in documentation
* Added 'duration_in_traffic' option in 'mp_get_matrix'
* 'mp_matrix' now returns 'NA' when there are zero results

## mapsapi 0.3.9 (2019-01-19)

* Support for 'waypoints' parameter in 'mp_directions' and 'ms_get_segments'
* 'mp_get_routes' now returns empty LINESTRING when 'doc' contains no route alternatives ('ZERO_RESULTS')
* Added 'quiet' parameter to 'mp_directions' and 'mp_matrix' to hide API from text key when necessary

## mapsapi 0.4.0 (2019-04-06)

* Added 'address_type' (geocode accuracy) field for 'mp_get_points'
* Added 'departure_time' and 'arrival_time' fields for 'mp_get_routes' and 'mp_get_segments'

## mapsapi 0.4.1 (2019-06-14)

* 'mp_get_points' now returns empty POINT when 'doc' contains geocode results ('ZERO_RESULTS')

## mapsapi 0.4.2 (2019-09-07)

* Added 'postcode' parameter in 'mp_geocode'

## mapsapi 0.4.5 (2020-04-14)

* Added 'mp_map' function
* Replace 'NULL' with 'NA' in parameters

## mapsapi 0.4.6 (2020-06-15)

* Added 'traffic_model' parameter in 'mp_directions' and 'mp_matrix'
* Added 'quiet' parameter in 'mp_geocode'

## mapsapi 0.4.7 (2020-09-19)

* 'mp_get_points' and 'mp_get_bounds' now also works given unnamed 'list' of documents

## mapsapi 0.4.8 (2020-12-17)

* Added 'pkgdown' site
* Fixed 'mp_get_bounds' when bounds results are empty
* Fixed 'mp_get_bounds' when more than one response per address

## mapsapi 0.4.9 (2021-06-13)

* Fixed bug in 'mp_matrix' (by Juan P. Fonseca-Zamora)
* New arguments in 'mp_directions' (by Juan P. Fonseca-Zamora)

## mapsapi 0.5.0 (2021-09-06)

* Corrected download method in 'mp_map' to avoid corrupted PNG (on Windows)

## mapsapi 0.5.3 (2022-01-13)

* Added 'timeout' option in 'mp_geocode'
* Added 'transit_mode' parameter in 'mp_matrix'
* Added 'language' parameter in 'mp_directions'

## mapsapi 0.5.4

* New options in 'mp_map' (by Rodrigo Vega)

## To do in 0.5.5

* Switch to native pipe and remove 'magrittr' dependency
* Add timeout option in all functions (similarly to 'mp_geocode')
* Allow selecting >1 'avoid' options in 'mp_directions'
* Static maps from sources other than Google Maps (?)

