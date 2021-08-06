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


  # check temporal sequence constraint ======
  check_tempsequence()


  # check entropy constraint ========




  # SVM classifier into activity / non-activity =========



}



#' Execute dbscan on trajectory data
#'
#' @inheritParams dbscan_te
#'
#'
do_dbscan <- function(trajectory, eps, minpts) {

  d <- sf::st_coordinates(trajectory)

  cl <- dbscan::dbscan(d, eps = eps, minPts = minpts)


}


#' Check the temporal sequence constraint
#'
#' @inheritParams dbscan_te
#'
#' @importFrom dplyr filter bind_rows
#' @importFrom tibble as_tibble
#'
check_tempsequence <- function(trajectory, delta_t){
  # put all cluster points into separate chunks, then
  # recursively split clusters if they have gaps in their
  # timeline greater than delta
  all_clusters <- trajectory %>%
    dplyr::filter(cluster > 0) %>%
    tibble::as_tibble() %>%
    split(.$cluster) %>%
    lapply(function(cluster) split_cluster(cluster, delta_t))  %>%
    dplyr::bind_rows()


}


split_cluster <- function(cluster, delta_t){
  a <- cluster %>%
    mutate(
      diff = timestamp - lag(timestamp, default = timestamp[1]),
      big_gap = ifelse(diff > delta_t, T, F),
      gaps = cumsum(big_gap),
      cluster = str_c(cluster, gaps, sep = ".")
    )  %>%
    split(.$cluster)

  if(length(a) > 1){
    a <- lapply(a, function(x) split_cluster(x, delta_t))
  }

  bind_rows(a)
}
