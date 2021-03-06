% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pick_UScode.R
\name{ebird_api}
\alias{ebird_api}
\title{Helper function to get content, path, and response from an ebird API request}
\usage{
ebird_api(path, ebirdkey, query = NULL)
}
\arguments{
\item{path}{Path used to modify the base ebird API URL}

\item{ebirdkey}{User ebird key}

\item{query}{optional argument allowing helper function to complete a query during API request}
}
\value{
A list containing content, path, and response
}
\description{
Helper function to get content, path, and response from an ebird API request
}
\author{
Simon Marks
}
