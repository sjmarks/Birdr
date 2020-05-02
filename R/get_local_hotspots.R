#' Returns dataframe of local birding hotspots for a given latitude and longitude.
#'
#' @param key The users eBird key
#' @param lat Latitude, must be to two decimal places
#' @param lng Longitude, must be to two decimal places
#' @param back How many days back to collect data, default is 1
#' @param dist The search radius of given lat and lng in kilometers, default of 25km
#'
#' @return subregions datafram
#'
#' @importFrom httr GET add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#'
#' @export
get_local_hotspots <- function(lat, lng, dist = 25, back = 1, key) {
  url <- glue("https://api.ebird.org/v2/ref/hotspot/geo?lat={lat}&lng={lng}")
  hotspots <- GET(url,
                  add_headers("x-ebirdapitoken" = key),
                  query = list(back = back,
                               dist = dist,
                               fmt = "json"))
  hotspots <- fromJSON(rawToChar(hotspots$content))
  hotspots
}
