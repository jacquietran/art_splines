throw_shapes <- function(seed_num, n_shapes, palette){
  
  # Requires {purrr}, {dplyr}
  
  set.seed(seed_num)
  seed_vec <- sample(seq(1, 10000000, by = 1), n_shapes, replace = FALSE)
  
  purrr::map_df(seed_vec, function(i){
    
    set.seed(i)
    n_control_points <- sample(seq(3, 6, by = 1), 1)
    
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
      hex_fill = sample(palette, dplyr::n(), replace = TRUE))
  
  splines_for_plot <- dplyr::left_join(
    splines_data, splines_with_colours, by = "seed_num")
  
  return(splines_for_plot)
  
}