#' Determine stop locations in trajectory data via TDBC
#'
#' @param trajectory A simple features collection representing a single
#'   trajectory.
#' @param eps distance threshold for defining clusters
#' @param minpts minimum number of points in a cluster
#'
#'
#' @details Implements the method describes in Fu, Z., Tian, Z., Xu, Y., &#38;
#'    Qiao, C. (2016). A Two-Step Clustering Approach to Extract Locations from
#'    Individual GPS Trajectory Data. ISPRS International Journal of
#'    Geo-Information 2016, Vol. 5, Page 166, 5(10), 166.
#'    \url{https://doi.org/10.3390/IJGI5100166}
#'
#' @examples
#' trajectory <- trajectories %>%  filter(id == "131") %>%
#'   mutate(day = day(localtime)) %>% filter(day == 16)
#'
#' @export
#'
tdbc <- function(trajectory) {

}



#' Fast Density
#'
#' The algorithm has its basis in the assumptions that cluster centers are
#' surrounded by neighbors with lower local density and that they are at a
#' relatively large distance from any points with a higher local density.
#'
#' @param trajectory A projected sf points dataframe representing a trajectory.
#' @param d_threshold A distance (in units of the projection system) to include
#'   in the cluster search.
#'
#' @details For each data point `i`, we compute two quantities: its
#'   local density `rho_i` and its distance `delta_i` from points of
#'   higher density. Both these quantities depend only on the distances Embedded
#'   Image between data points, which are assumed to satisfy the triangular
#'   inequality. The local density Embedded Image of data point Embedded Image
#'   is defined as
#'
fast_density <- function(trajectory, d_threshold){

  # calculate distance matrix
  distances <- as.numeric(st_distance(trajectory, trajectory)) %>%
    matrix(., nrow = nrow(trajectory), byrow = TRUE)

  # calculate point density
  rho <- apply(distances, 1, local_density, d_threshold = d_threshold)


  # calculate distance to mean point
  delta <- lapply(1:nrow(distances), function(i){
    min_distance(distances[i, ], rho, rho[i])
  }) %>%
    unlist()

  l <- trajectory %>%
    mutate(
      rho = rho,
      delta = delta,
      lambda = rho * delta
    )
  plot(l[, "lambda"])

  lambda <- rho * delta

}




#' Calculate local point density
#'
#' @param d A vector of distances between a point and all other
#'   points in the trajectory
#'
local_density <- function(d, d_threshold){
  sum(d - d_threshold < 0)
}

#' Find minimum distance to point with lower density
#'
#' @param d vector of distances
#' @param rho vector of point densities
#' @param rho_i point density for point i
#'
#' @details `delta_i` is measured by computing the minimum distance between the
#'   point `i` and any other point with higher density:
#'
#'
min_distance <- function(d, rho, rho_i){

  # distances to points with higher density; zero if lower or equal density
  x <- d * (rho > rho_i)

  # For the point with highest density, we conventionally take `max(d)`
  if(max(x) == 0){
    max(d)
  } else {
    # minimum distance
    min(x[x > 0])
  }

}


