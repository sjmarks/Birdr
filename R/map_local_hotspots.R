#' Returns map of local hotspots. Labels each hotspots title. Popup will include name, lat/lng, latest obeservation
#' and the total number of species observed
#'
#' @param key The users eBird key
#' @param lat Latitude, must be to two decimal places
#' @param lng Longitude, must be to two decimal places
#' @param back How many days back to collect data, default is 1
#' @param dist The search radius of given lat and lng in kilometers, default of 25km
#' @param iconwidth Width of map icon, default is 30
#' @param iconheight Height of map icon, default is 30
#'
#' @return countries dataframe
#'
#' @importFrom httr GET add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom leaflet makeIcon leaflet makeIcon addPolylines
#'
#' @export
map_local_hotspots <- function(latitude, longitude, back = 1, dist = 25, iconwidth = 30, iconheight = 30, key) {
  #Get the data
  hotspots <- get_local_hotspots(latitude, longitude, dist, back, key)
  #Icons
  bird_icon <- makeIcon(iconUrl = "http://www.clker.com/cliparts/b/2/a/4/1195426695650497164PeterM_Feather.svg.med.png",
                        iconWidth = iconwidth, iconHeight = iconheight)
  you_icon <- makeIcon(iconUrl = "http://www.clker.com/cliparts/q/I/Q/u/Z/1/marker-md.png",
                        iconWidth = iconwidth, iconHeight = iconheight)
  #MAP#
  m <- leaflet(data = hotspots) %>%
    addTiles() %>%
    addMarkers(data = hotspots,
               lat = ~lat,
               lng = ~lng,
               label = paste("Hotspot Name: ", hotspots$locName),
               popup = paste("Latitude: ", as.character(hotspots$lat), "<br>", "Longitude: ", as.character(hotspots$lng), "<br>", "Most Recent Observation: ", hotspots$latestObsDt, "<br>", "Total Species Observed: ", hotspots$numSpeciesAllTime),
               icon = bird_icon)
  m <- addMarkers(map = m,
                  lat = latitude,
                  lng = longitude,
                  label = "You are here",
                  popup = paste("Latitude: ", as.character(latitude), "<br>", "Longitude: ", as.character(longitude)),
                  icon = you_icon)
  for (i in 1:length(hotspots$locName)){
    map_latitude = c(latitude, hotspots$lat[i])
    map_longitude = c(longitude, hotspots$lng[i])
    m <- addPolylines(map = m, lat = ~map_latitude, lng = ~map_longitude)
  }
  m
}
