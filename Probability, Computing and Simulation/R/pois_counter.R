# create a function to avoid nested maps when tallying the counts 
# per partition in pois_verifier()

#' Utility Function for Counting the Poisson points
#'
#' @param interval an interval
#' @param pois_runs n_runs of x-coordinates of the 2D Poisson process
#'
#' @return number of Poisson points from each run that fall within the interval 
#' @export
#'
#' @examples
pois_counter <- function(interval, pois_runs) {
  map_int(pois_runs, ~sum(between(., interval[1], interval[2])))
}