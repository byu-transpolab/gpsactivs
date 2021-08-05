#' Trajectories of 5 individuals
#'
#' A dataset containing 5 individuals from the Microsoft GeoLife Trajectories
#' dataset. The data is an sf object with proper projection and timezones.
#'
#' @format A simple features collection with 505937 features and 4 fields:
#' \describe{
#'   \item{id}{ID, describing which folder in the original Geolife dataset the
#'   record was drawn from.}
#'   \item{altitude}{Altitude of point in meters}
#'   \item{gmt}{Timestamp of point in GMT}
#'   \item{localtime}{Timestamp of point in local time}
#' }
#'
#' @source \url{https://www.microsoft.com/en-us/download/details.aspx?id=52367}
#'
"trajectories"
