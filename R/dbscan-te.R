#' Determine stop locations in trajectory data via DBSCAN-TE
#'
#' @param trajectory A simple features collection of points representing a
#'   single trajectory (i.e. person, day). MUST have a `dttm` column called
#'   "timestamp" representing the time of GPS point.
#' @param eps distance threshold for defining clusters. Should be provided
#'   in the same units as the trajectory projection (usually meters). DBSCAN input.
#' @param minpts minimum number of points in a cluster. DBSCAN input.
#' @param delta_t  time threshold (seconds): a gap of this length within a
#'   spatial cluster will split the cluster into two potential activities.
#'   An activity must also be at least this long.
#' @param entr_t entropy threshold: the entropy of a cluster is a function of
#'   the chaotic movement between points in a cluster. Clusters with higher
#'   entropy are more likely to be activities; this parameter will exclude
#'   potential activities below this threshold
#'
#' @return A simple features collection of points with the start and end
#'   time of each activity, the estimated cluster entropy (for debugging / calibrating)
#'
#' @details Implements the method described in Gong, L.,
#'   Yamamoto, T., &#38; Morikawa, T. (2018). Identification of activity stop
#'   locations in GPS trajectories by DBSCAN-TE method combined with support
#'   vector machines. Transportation Research Procedia, 32,
#'   146–154. \url{https://doi.org/10.1016/J.TRPRO.2018.10.028}
#'
#'   Note: the SVM is not (yet) implemented. The entropy calculation and
#'   heuristics seem to get us almost all the way there.
#'
#' @examples
#'
#'
#'
#' @export
#'
#' @importFrom sf st_as_sf st_crs
#'
dbscan_te <- function(trajectory, eps = 25, minpts = 4, delta_t = 300,
                      entr_t = 1.75) {

  # execute dbscan on trajectory points ======
  cl <- do_dbscan(trajectory, eps, minpts)
  trajectory$cluster <- cl$cluster


  # check temporal sequence and entropy constraints ======
  activities <- check_clusters(trajectory, delta_t, minpts, entr_t)

  # TODO
  # SVM classifier into activity / non-activity =========


  activities %>%
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(trajectory))

}



#' Execute dbscan on trajectory data
#'
#' @inheritParams dbscan_te
#'
#' @importFrom sf st_coordinates
#' @importFrom dbscan dbscan
#'
#'
do_dbscan <- function(trajectory, eps, minpts) {

  d <- sf::st_coordinates(trajectory)
  dbscan::dbscan(d, eps = eps, minPts = minpts)

}


#' Check the temporal sequence constraint and return
#' stop points
#'
#' @inheritParams dbscan_te
#'
#' @importFrom dplyr filter bind_rows group_by mutate
#' @importFrom tibble as_tibble
#' @importFrom tidyr nest
#' @importFrom purrr map map_dbl map_int
#'
check_clusters <- function(trajectory, delta_t, minpts, entr_t){

  # put all cluster points into separate chunks, then
  # recursively split clusters if they have gaps in their
  # timeline greater than delta
  all_clusters <- trajectory %>%
    dplyr::filter(cluster > 0) %>%
    tibble::as_tibble() %>%
    split(.$cluster) %>%
    lapply(function(cluster) split_cluster(cluster, delta_t))  %>%
    dplyr::bind_rows()


  # get clusters in the right order with the right number of points
  all_clusters %>%
    dplyr::group_by(cluster) %>%
    tidyr::nest() %>%

    # find elapsed time and entropy for all clusters
    dplyr::mutate(
      start = purrr::map(data, get_min)[[1]],
      end = purrr::map(data, get_max)[[1]],
      x = purrr::map_dbl(data, get_meanx),
      y = purrr::map_dbl(data, get_meany),
      elapsed = end - start,
      n = purrr::map_int(data, nrow),
      entr = purrr::map_dbl(data, get_cluster_entropy)
    ) %>%

    # only keep clusters over entropy threshold
    dplyr::filter(entr > entr_t) %>%
    dplyr::arrange(start) %>%
    dplyr::select(cluster, start, end, elapsed, x, y, entr)

}


#' Split clusters based on a delta_t
#'
#' @details This is a recursive algorithm that divides spatially
#'   defined clusters based on gaps in their timeline.
#'
#' @importFrom dplyr mutate lag
#' @importFrom stringr str_c
#'
split_cluster <- function(cluster, delta_t){
  a <- cluster %>%
    dplyr::mutate(
      diff = timestamp - dplyr::lag(timestamp, default = timestamp[1]),
      big_gap = ifelse(diff > delta_t, T, F),
      gaps = cumsum(big_gap),
      cluster = stringr::str_c(cluster, gaps, sep = ".")
    )  %>%
    split(.$cluster)

  if(length(a) > 1){
    a <- lapply(a, function(x) split_cluster(x, delta_t))
  }

  dplyr::bind_rows(a)
}

#' Calculate the entropy of cluster rays
#'
#' @param d a tibble with a point geometry column
#' @return The cluster entropy
#'
#' @details Equation 1 in Gong, et al. (2018)
#'
#' @importFrom sf st_coordinates st_as_sf
#'
get_cluster_entropy <- function(d) {
  # get coordinates of points (should be in order)
  xy <- sf::st_coordinates(sf::st_as_sf(d))

  # compute distance between consecutive points
  dist = sqrt(diff(xy[, 1])^2 + diff(xy[, 2])^2)

  # compute angle in radians (from -pi to pi)
  angles <- atan2(diff(xy[, 1]) , diff(xy[, 2]))
  a_group <- cut(angles[dist > 0], breaks = seq(-pi, pi, by = 2 * pi / 8))

  # count how many rays are in each octant of the circle
  nd <- table(a_group)

  - sum(nd / sum(nd) * log(nd / sum(nd)), na.rm = TRUE)
}

get_min <- function(d) min(d$timestamp)
get_max <- function(d) max(d$timestamp)

get_meanx <- function(d){
  mean(sf::st_coordinates(sf::st_as_sf(d))[, 1])
}

get_meany <- function(d){
  mean(sf::st_coordinates(sf::st_as_sf(d))[, 2])
}

