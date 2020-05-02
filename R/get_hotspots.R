#' Returns dataframe of birding hotspots for a given region. Must enter the regions code for region, for example if you wanted hotspots
#' in the United States you would enter "region = `US`".
#'
#' @param key The users eBird key
#' @param region The region code for the desired region
#' @param back How many days back to collect data, default is 1
#'
#' @return subregions datafram
#'
#' @importFrom httr GET add_headers
#' @importFrom jsonlite FromJSON
#' @importFrom glue glue
#'
#' @export
get_hotspots <- function(region, back = 1, key) {
  url <- glue("https://api.ebird.org/v2/ref/hotspot/{region}")
  hotspots <- GET(url,
                 add_headers("x-ebirdapitoken" = key),
                 query = list(back = back,
                              fmt = "json"))
  hotspots <- fromJSON(rawToChar(hotspots$content))
  hotspots
}
