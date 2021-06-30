#' @title Simulating a 2D Poisson Process
#' 
#' @description Workhorse function for simulating a 2D Poisson process
#' over a rectangular region with its lower-left corner at the origin.
#' This implementation determines the total number of points first and 
#' then assign coordinates uniformly.
#' 
#' @param b base of the rectangle 
#' @param h height of the rectangle
#' @param lambda Poisson rate
#'
#' @return x-coordinates of the Poisson points
#' @export
#'
#' @examples
#' pois_2d_simp(10, 100, 0.01)
pois_2d_simp <- function(b, h, lambda) {
  n_pts <- rpois(1, b*h*lambda)
  x_coords <- b * runif(n_pts)
  x_coords
}