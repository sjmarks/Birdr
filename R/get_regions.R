#' Returns dataframe of subregions and their codes for a parent country or a parent subregion
#' Can also give list of all countries if region_type is set to "country"
#'
#' @param key The users eBird key
#' @param region_type The options for this are country, subnational1, and subnational2
#' @param parent_region The options for this are world (the default),
#'
#' @return subregions datafram
#'
#' @importFrom httr GET add_headers
#' @importFrom jsonlite FromJSON
#' @importFrom glue glue
#'
#' @export
get_regions <- function(region_type, parent_region = "world", key) {
  stopifnot(region_type %in% c("country", "subnational1", "subnational2"),
            parent_region %in% c("world", "country", "subnational1"))
  url <- glue("https://api.ebird.org/v2/ref/region/list/{{region_type}}/{parent_region}")
  regions <- GET(url,
                 add_headers("x-ebirdapitoken" = key))
  regions <- fromJSON(rawToChar(regions$content))
  regions
}
