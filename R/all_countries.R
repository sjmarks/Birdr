#' Returns dataframe of all countries
#'
#' @param key The users eBird key
#'
#' @return countries dataframe
#'
#' @importFrom httr GET add_headers
#' @importFrom jsonlite FromJSON
#'
#' @export
all_countries <- function(key) {
  url <- "https://api.ebird.org/v2/ref/region/list/country/world.json"
  country <- GET(url,
                 add_headers("x-ebirdapitoken" = key))
  country <- fromJSON(rawToChar(country$content))
  country
}
