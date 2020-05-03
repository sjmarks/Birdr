#' Creates dot plot of highest recent notable bird observation counts for a specified region within the United States
#'
#' @description Creates dot plot of highest recent notable bird observation counts for a specified region within the United States. Will plot up to the top 25 most notably sighted birds for a region. Accesses data from ebird API.

#' @param region_code United States subnational1 or subnational2 region code
#' @param ebirdkey User ebird key
#' @param obs_to_plot Number of observations to plot, default is 15, max is 25
#' @param back The number of days back to fetch observations, max is 30
#' @param hotspot Only fetch observations from hotspots- logical
#' @param includeProvisional Include observations which have not yet been reviewed- logical
#' @param maxResults Only fetch this number of observations, max is 10000
#'
#' @return A dotplot of highest recent notable bird observation counts
#'
#' @examples
#' get_notable_chart(region_code = "US-CA", ebirdkey = 'mykey', obs_to_plot = 25, back = 30)
#'
#' @note Use pick_UScode() to determine region code for region_code argument
#'
#' @author Simon Marks

#' @importFrom rlang .data
#' @importFrom dplyr mutate case_when arrange desc group_by summarise slice
#' @import ggplot2
#'
#' @export
get_notable_chart <- function(region_code, ebirdkey, obs_to_plot = 15,
                              back = 14, hotspot = FALSE,
                              includeProvisonal = FALSE, maxResults = 10000) {

  if(!obs_to_plot %in% 1:25){
    stop("Invalid number obs to plot, must be value 1-25")
  }


  # Relies on get_notable_obs as helpoer function
  notable_data <- get_notable_obs(region_code = region_code, ebirdkey = ebirdkey,
                                   back = back, hotspot = hotspot, includeProvisonal = includeProvisonal,
                                   maxResults = maxResults)

  # group by common name and order by number of observations, NA values are assumed to be a count of 1
  notable_data_cleaned <- notable_data %>%
    mutate(howMany = as.numeric(howMany)) %>%
    mutate(howMany = case_when(
      is.na(howMany) ~ 1,
      TRUE ~ howMany)
    ) %>%
    group_by(comName) %>%
    summarise(total = sum(howMany)) %>%
    arrange(desc(total)) %>%
    mutate(comName = factor(comName, comName))

  # plot preliminaries
  english_no <- english::as.english(obs_to_plot)

  days <- english::as.english(back)

  rows <- nrow(notable_data_cleaned)

  english_no_2 <- english::as.english(rows)

  theme <- theme(axis.title = element_blank(),
                 panel.grid = element_blank(),
                 panel.background = element_blank(),
                 axis.text = element_text(face = "bold"))

  if(rows <= obs_to_plot){

    max <- max(notable_data_cleaned$total)
    min <- min(notable_data_cleaned$total)

    plot <- ggplot(notable_data_cleaned, aes(x = .data$comName, y = .data$total)) +
      # Draw points
      geom_point(col = "tomato2", size = 2) +
      geom_segment(aes(x = .data$comName,
                       xend = .data$comName,
                       y = min(.data$total),
                       yend = max(.data$total)),
                   linetype = "dashed",
                   size = 0.1) +
      labs(title = glue::glue("Top {english_no_2} notable bird observation counts. Tweet!"),
           subtitle = glue::glue("Region: {region_code}, last {days} days."),
           caption= "source: ebird API") +
      expand_limits(y = c(min, max)) +
      coord_flip() +
      theme

    print(plot)

  }

  if(rows > obs_to_plot){

    notable_data_cleaned <- notable_data_cleaned %>%
      slice(1:obs_to_plot)

    max <- max(notable_data_cleaned$total)
    min <- min(notable_data_cleaned$total)

    plot <- ggplot(notable_data_cleaned, aes(x = .data$comName, y = .data$total)) +
      # Draw points
      geom_point(col = "tomato2", size = 2) +
      geom_segment(aes(x = .data$comName,
                       xend = .data$comName,
                       y = min(.data$total),
                       yend = max(.data$total)),
                   linetype = "dashed",
                   size = 0.1) +
      labs(title = glue::glue("Top {english_no} notable bird observation counts. Tweet!"),
           subtitle = glue::glue("Region: {region_code}, last {days} days."),
           caption= "source: ebird API") +
      expand_limits(y = c(min, max)) +
      coord_flip() +
      theme

    print(plot)

  }

}
