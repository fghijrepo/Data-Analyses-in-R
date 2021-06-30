# this function feeds both verification and animation
# the workhorse that simulates a 2D Poisson process on a rectangular region 
# by returning the x-coordinates of the Poisson points.

# the rectangle is anchored at the origin on its lower left corner
# with base B and height H
# this 2D process is meant to be invoked T times by verification and animation,
# which compute counts and build up ggplot layers on the fly
# a list of the X- coordinates each as vectors is returned

#' @title Simulating a 2D Poisson Process
#' 
#' @description Workhorse function for simulating a 2D Poisson process
#' over a rectangular region with its lower-left corner at the origin.
#' 
#' @param b base of the rectangle 
#' @param h height of the rectangle
#' @param lambda Poisson rate
#'
#' @return x-coordinates of the Poisson points
#' @export
#'
#' @examples
#' pois_2d(10, 100, 0.01)
pois_2d <- function(b, h, lambda) {
  # grow x-coordinates in chunks, but generate y-coordinates in one go
  # after the total count of the Poisson points becomes known.
  # OR... do not generate y-coordinates at all, at this time
  
  # initialize X_COORDS to hold the expected number of points
  n_chunk <- b * h * lambda
  x_coords <- double(n_chunk) 
  
  A <- b*h # calculate area once instead of in the while loop
  W_sum <- rexp(1, rate=lambda) 
  i <- 1
  while (W_sum < A) {
    if (i > length(x_coords))
      x_coords <- c(x_coords, double(n_chunk)) 
    x_coords[i] <- W_sum / h
    i <- i + 1
    W_sum <- W_sum + rexp(1, rate=lambda) 
  }
  #shorten to final length
  x_coords <- x_coords[seq_len(i-1)]
  x_coords
  # can further boost efficiency by generating y-coordinates 
  # across different runs in one go
  # y_coords <- h * runif(i-1)
  # list(x_coords, y_coords)
}