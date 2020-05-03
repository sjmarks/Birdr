#' Creates a dataframe of birds spotted on a single day for a region (default United States)
#' For specific states or counties use pick_UScode to generate the country ID. 
#'
#' @description returns dataframe of all birds spotted on a single day for a region
#'
#' @param country string of country letter abreviation, default is United States
#' @param year string of 4 digit year
#' @param month string of month 1-12 
#' @param day string of day 1-31 
#' @param key The users eBird key
#'
#' @return bird_obs dataframe
#'
#' @examples
#' bird_day <- function(year = "1999", month = "1", day = "28", key = my_birdkey)
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
  
return(bird_obs)
}
