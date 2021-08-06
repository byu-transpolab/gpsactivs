#' Determine stop locations in trajectory data via DBSCAN-TE
#'
#' @param trajectory A simple features collection representing a single
#'   trajectory.
#' @param eps distance threshold for defining clusters
#' @param minpts minimum number of points in a cluster
#'
#' @details Implements the method described in Gong, L.,
#'   Yamamoto, T., &#38; Morikawa, T. (2018). Identification of activity stop
#'   locations in GPS trajectories by DBSCAN-TE method combined with support
#'   vector machines. Transportation Research Procedia, 32,
#'   146–154. \url{https://doi.org/10.1016/J.TRPRO.2018.10.028}
#'
#' @examples
#' trajectory <- trajectories %>%  filter(id == "131") %>%
#'   mutate(day = day(localtime)) %>% filter(day == 16)
#'
#'
#'
#' @export
dbscan_te <- function(trajectory, eps = 25, minpts = 4, delta_t = 120) {

  # execute dbscan on trajectory points ======
  cl <- do_dbscan(trajectory, eps, minpts)
  trajectory$cluster <- cl$cluster


  # check temporal sequence and entropy constraints ======
  check_clusters(trajectory, delta_t, minpts)






  # SVM classifier into activity / non-activity =========



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
#' @importFrom dplyr filter bind_rows
#' @importFrom tibble as_tibble
#'
check_clusters <- function(trajectory, delta_t, minpts){

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
    group_by(cluster) %>%
    nest() %>%

    mutate(
      min_time = map(data, get_min)[[1]],
      max_time = map(data, get_max)[[1]],
      n = map_int(data, nrow)
    )






}


#' Split clusters based on a delta_t
#'
#' @details This is a recursive algorithm
#'
#' @importFrom dplyr mutate lag cumsum
#' @importFrom stringr str_c
#'
split_cluster <- function(cluster, delta_t){
  a <- cluster %>%
    dlyr::mutate(
      diff = timestamp - dplyr::lag(timestamp, default = timestamp[1]),
      big_gap = ifelse(diff > delta_t, T, F),
      gaps = dplyr::cumsum(big_gap),
      cluster = stringr::str_c(cluster, gaps, sep = ".")
    )  %>%
    split(.$cluster)

  if(length(a) > 1){
    a <- lapply(a, function(x) split_cluster(x, delta_t))
  }

  dplyr::bind_rows(a)
}


get_min <- function(d) min(d$timestamp)
get_max <- function(d) max(d$timestamp)


get_cluster_entropy <- function() {

}
