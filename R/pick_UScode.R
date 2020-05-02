#' Determines subnational1 and subnational2 region codes for United States from ebird API
#'
#' @description Determines subnational1 and subnational2 region codes for United States from ebird API, useful for deciding region arguments in some Birdr functions
#'
#' @param state String of full US state name (i.e. "California")
#' @param county String of full county name (i.e "San Luis Obispo"), only required if subnational2 region code is desired
#' @param ebirdkey User ebird key
#'
#' @return Tibble containing region name and code
#'
#' @examples
#' pick_UScode(state = "California", county = "San Luis Obispo", ebirdkey = 'mykey'))
#'
#' @note Some counties will result in a return of multiple subnational2 ebird region codes because there are different counties with the same name.
#'
#' @importFrom dplyr filter
#'
#' @export
pick_UScode <- function(state, county = NULL, ebirdkey){

  states_parsed <- ebird_api(path = "/v2/ref/region/list/subnational1/US.json",
                             ebirdkey = ebirdkey)

  states_df <- clean_ebirdlist(states_parsed)

  states_vector <- states_df$name

  if(!state %in% states_vector){
    stop("Invalid state name provided, ensure state is properly spelled")
  }

  counties_parsed <- ebird_api(path = "/v2/ref/region/list/subnational2/US.json",
                               ebirdkey = ebirdkey)

  counties_df <- clean_ebirdlist(counties_parsed)

  counties_vector <- counties_df$name

  if(!is.null(county) && !county %in% counties_vector){
    stop("Invalid county name provided, ensure county is properly spelled")
  }

  if(is.null(county)){
    state_code <- filter(states_df, name == state)
    return(state_code)
  }

  county_code <- filter(counties_df, name == county)


  return(county_code)

}

#' Helper function to get content, path, and response from an ebird API request
#'
#' @param path Path used to modify the base ebird API URL
#' @param ebirdkey User ebird key
#' @param query optional argument allowing helper function to complete a query during API request
#'
#' @return A list containing content, path, and response
#'
#' @importFrom httr GET add_headers modify_url http_type
ebird_api <- function(path, ebirdkey, query = NULL) {

  url <- modify_url('https://api.ebird.org', path = path)

  resp <- GET(url, add_headers('X-eBirdApiToken' = ebirdkey), query = query)

  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  if (status_code(resp) != 200) {
    stop(
      sprintf(
        "ebird API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "ebird_api"
  )

}

#' Helper function to create tibble from ebird JSON parsed content
#'
#' @param list list containing JSON parsed content returned from ebird_api
#'
#' @return A tibble of ebird data
#'
#' @importFrom purrr pluck
#' @importFrom dplyr bind_rows
clean_ebirdlist <- function(list){

  content_list <- pluck(list, "content")

  tibble <- bind_rows(content_list)

}

