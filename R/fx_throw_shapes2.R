throw_shapes2 <- function(
  seed_num, n_shapes, palette, personality = c("haphazard", "bold")){
  
  # Requires {purrr}, {dplyr}
  
  if(missing(personality)){
    
    personality <- "haphazard"
    
  }
  
  set.seed(seed_num)
  seed_vec <- sample(seq(1, 10000000, by = 1), n_shapes, replace = FALSE)
  
  purrr::map_df(seed_vec, function(i){
    
    if(personality == "haphazard"){
      
      set.seed(i)
      n_control_points <- sample(seq(3, 6, by = 1), 1)
      
    } else{
     
      set.seed(i)
      n_control_points <- sample(seq(3, 4, by = 1), 1)
      
    }
    
    set.seed(i)
    data <- data.frame(
      seed_num = i,
      x = scales::rescale(runif(n_control_points), to = c(-0.2, 1.2)),
      y = scales::rescale(runif(n_control_points), to = c(-0.2, 1.2)))
    
  }) -> splines_data
  
  set.seed(seed_num)
  splines_with_colours <- splines_data |>
    dplyr::distinct(seed_num) |>
    dplyr::mutate(
      hex_fill = sample(palette, dplyr::n(), replace = TRUE),
      spline_size = dplyr::case_when(
        personality == "haphazard" ~ sample(
          c(seq(0.05, 0.4, by = 0.05), c(0.6, 0.7, 0.8, 0.9)),
          n(), replace = TRUE,
          prob = c(rep(0.1, times = 8), rep(0.05, times = 4))),
        personality == "bold"      ~ sample(
          c(seq(0.05, 0.45, by = 0.05), c(0.6, 0.9)),
          n(), replace = TRUE,
          prob = c(rep(0.124, times = 5), rep(0.1, times = 4), 0.05, 0.005)))
      )
  
  splines_for_plot <- dplyr::left_join(
    splines_data, splines_with_colours, by = "seed_num")
  
  return(splines_for_plot)
  
}