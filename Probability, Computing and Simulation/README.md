
# fghijrepo-project-poisson

<!-- badges: start -->
<!-- badges: end -->
Project Title: Simulation of a 2D Poisson Process

Name: Funfay Jen

Description: The goal of fghijrepo-project-poisson is to simulate the Poisson process through computational lenses. We start from understanding the concepts, and then implement the Poisson process, examine alternative approaches, verify the correctness, and visualize it.

You can find my slides [here](./presentation_report/presentation.html), which gives an overview of the project. The [report](./presentation_report/report.html) goes into more details and is more polished.

Some pointers:  

- A useful resource for Issue #1 is the Simulation book by Ross.
- The required packages to run this project are:   
  + tidyverse  
  + here  
  + gganimate  
  + bench  
- The order in which the Rmd files, which present project details, should be run in are:
  1. pois_2d_verfier.Rmd  
  2. pois_2d_animator.Rmd  
  + pois_2d_timer.Rmd the can be run anytime.  
- The functions that these Rmd files invoke reside in the /R folder. You can pull out documentation files for these functions by typing `?function_name`. 
