test_that("get_local_hotspots works for San Luis Obispos latitude and longitude", {
  correct_result <- "Alice Keck Park"

  my_result <- get_local_hotspots(lat = 35.28, lng = -120.65, back = 1, dist = 300, key = "6qqv2oh5smkm")

  expect_true(correct_result %in% my_result)
})

test_that("map_local_hotspots works", {
    hotspots <- get_local_hotspots(lat = 35.28, lng = -120.66, key = "6qqv2oh5smkm")
    #Icon of a cool bird
    bird_icon <- makeIcon(iconUrl = "http://www.clker.com/cliparts/b/2/a/4/1195426695650497164PeterM_Feather.svg.med.png",
                          iconWidth = 30, iconHeight = 30)
    you_icon <- makeIcon(iconUrl = "http://www.clker.com/cliparts/q/I/Q/u/Z/1/marker-md.png",
                         iconWidth = 30, iconHeight = 30)
    #MAP#
    m <- leaflet(data = hotspots) %>%
      addTiles() %>%
      addMarkers(data = hotspots, lat = ~lat, lng = ~lng,
                 label = paste("Hotspot Name: ", hotspots$locName),
                 popup = paste("Latitude: ", as.character(hotspots$lat), "<br>", "Longitude: ", as.character(hotspots$lng), "<br>", "Most Recent Observation: ", hotspots$latestObsDt, "<br>", "Total Species Observed: ", hotspots$numSpeciesAllTime),
                 icon = bird_icon)
    m <- addMarkers(map = m, lat = latitude, lng = longitude,
                    label = "You are here",
                    popup = paste("Latitude: ", as.character(latitude), "<br>", "Longitude: ", as.character(longitude)),
                    icon = you_icon)
    for (i in 1:length(hotspots$locName)){
      your_lat = 35.28
      your_lng = -120.68
      latitude = c(your_lat, hotspots$lat[i])
      longitude = c(your_lng, hotspots$lng[i])
      m <- addPolylines(map = m, lat = ~latitude, lng = ~longitude)
    }
    m
    correct_result = m

    my_result = map_local_hotspots(latitude = 35.28, longitude = -120.66, key = "6qqv2oh5smkm")

    expect_equal(correct_result$x$limits[1] == my_result$x$limits[1])
})
