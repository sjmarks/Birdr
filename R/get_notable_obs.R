#' Creates tibble of recent notable bird observations for specified region within the United States
#'
#' @description Creates tibble of recent notable bird observations for specified region within the United States using a subnational1 or subnational2 region code. Accesses data from ebird API.
#'
#' @param region_code United States subnational1 or subnational2 region code
#' @param ebirdkey User ebird key
#' @param back The number of days back to fetch observations, max is 30
#' @param hotspot Only fetch observations from hotspots- logical
#' @param includeProvisional Include observations which have not yet been reviewed- logical
#' @param maxResults Only fetch this number of observations, max is 10000
#'
#' @return Tibble of recent notable bird observations ordered from most to least recent for the specified number of days back to collect records
#'
#' @examples
#' get_notable_obs(region = "US-CA-079", ebirdkey = 'mykey', back = 15)
#'
#' @note Use pick_UScode() to determine region code for region_code argument
#'
#' @author Simon Marks
#'
#' @importFrom dplyr arrange desc mutate
#'
#' @export
get_notable_obs <- function(region_code, ebirdkey, back = 14, hotspot = FALSE,
                            includeProvisonal = FALSE, maxResults = 10000) {

  if(!back %in% 1:30){
    stop("Invalid number of days back to fetch observations, must be value 1-30")
  }

  if(!maxResults %in% 1:10000){
    stop("Invalid number of observations, must be value 1-10000")
  }

  # Relies on ebird_api fun and clean_ebirdlist fun as helpers
  states_parsed <- ebird_api(path = "/v2/ref/region/list/subnational1/US.json",
                               ebirdkey = ebirdkey)

  states_df <- clean_ebirdlist(states_parsed)

  states_vector <- states_df$code

  counties_parsed <- ebird_api(path = "/v2/ref/region/list/subnational2/US.json",
                                 ebirdkey = ebirdkey)

  counties_df <- clean_ebirdlist(counties_parsed)

  counties_vector <- counties_df$code

  if(!region_code %in% states_vector && !region_code %in% counties_vector){
    stop("Invalid region code supplied. Consult pick_UScode()")
  }

  if (hotspot == FALSE){
    hotspot <- "false"
  } else {
    hotspot <- "true"
  }

  if (includeProvisonal == FALSE){
    includeProvisonal <- "false"
  } else {
    includeProvisonal <- "true"
  }

  region_path <- glue::glue("/v2/data/obs/{region_code}/recent/notable")

  query_reg <- list(back = back, hotspot = hotspot,
                    includeProvisonal = includeProvisonal,
                    maxResults = maxResults)

  ebird_resp <- ebird_api(path = region_path, ebirdkey = ebirdkey, query = query_reg)

  notable_result <- clean_ebirdlist(ebird_resp) %>%
    mutate(obsDt = as.POSIXct(obsDt)) %>%
    arrange(desc(obsDt))

  return(notable_result)

}
