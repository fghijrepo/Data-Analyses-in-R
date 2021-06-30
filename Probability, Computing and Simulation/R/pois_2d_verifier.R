#' Verifier for the 2D Poisson Process Simulations
#'
#' @param n_runs number of runs of the 2D Poisson process
#' @param b base of the rectangle
#' @param h height of the rectangle
#' @param lambda Poisson rate
#' @param n_parts Number of parts into which the region is partitioned
#' @param pois_2d_fn The implementation of the workhorse Poisson simulator to be used
#'
#' @return n_runs of the x-coordinates of the 2D Poisson process
#' @export
#'
#' @examples
#' pois_2d_verifier(1000, 10, 100, 0.01, 5, pois_2d_simp)
pois_2d_verifier <- function(n_runs, b, h, lambda, n_parts, pois_2d_fn) {
  
  partition <- map(seq(0, b-b/n_parts, b/n_parts), ~c(. , . + b/n_parts))
  
  # save the N_RUNS, each a vector of around B*H*LAMBDA x-coordinates
  pois_runs <- map(1:n_runs, ~pois_2d_fn(b, h, lambda))
  
  
  counts_per_partition <- map(partition, ~pois_counter(., pois_runs))
  mean_count_per_partition <- map_dbl(partition, ~mean(pois_counter(., pois_runs)))
  
  print(paste("n_runs = ", n_runs, "b = ", b, "h = ", h, "lambda = ", lambda,
              "n_parts = ", n_parts, "pois_2d_fn = ", as.character(substitute(pois_2d_alt))))
  print(paste("mean_count_per_partition = ", mean_count_per_partition))
  
  
  map(counts_per_partition, ~print(ggplot() +
                                     geom_bar(aes(x = ., y = stat(prop))) +
                                     geom_point(aes(x = 0:max(.),
                                                    y = dpois(0:max(.), b*h*lambda/n_parts))) +
                                     geom_function(fun=dnorm, 
                                                   args=list(mean = b*h*lambda/n_parts, 
                                                             sd = sqrt(b*h*lambda/n_parts)), 
                                                   color='red')))
  pois_runs
}
