#' Function Factory of the `add_layer` Utility
#'
#' @return
#' a function that builds up the static ggplot to be used by gganimate
#' @export
#'
#' @examples
#' add_layer <- add_layer_factory()
add_layer_factory <- function(){
  # the indices I, J are used to carve out the y-coordinates
  
  # plot-store available in the environment of the child function
  # this way the factory can potentially roll out functions for 
  # Poisson runs simulated from different configurations of the 
  # Poisson process
  gg <- ggplot()
  i <- 1
  function(x_coords){
    j <- i + length(x_coords)
    y_coords <- y_coords_all_runs[i : (j-1)]
    gg <<- gg + 
      geom_point(aes(x = x_coords, y = y_coords))
    i <<- j 
  }
}