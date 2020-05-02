#' Returns dataframe of regions and the birds present on a given day
#' Can also give list of all countries if region_type is set to "country"
#'
#' @param country two letter country abreviation, default is United States
#' @param year 4 digit year written as a string
#' @param month month 1-12 written as a string
#' @param day day 1-31 written as a string
#' @param key The users eBird key
#'
#' @return bird_obs dataframe
#'
#' @importFrom httr GET add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom tidyr drop_na
#'
#' @export
bird_day <- function(country = "US", year, month, day, key ){

  url <- glue("https://api.ebird.org/v2/data/obs/{country}/historic/{year}/{month}/{day}")

  bird_obs <- GET(url,
                  add_headers("x-ebirdapitoken" = key),
                  query = list(fmt = "json"))
  bird_obs <- fromJSON(rawToChar(bird_obs$content)) %>%
    drop_na()

}
