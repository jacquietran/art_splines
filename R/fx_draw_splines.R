draw_splines <- function(seed_num, n_splines){
  
  # Requires {purrr}, {dplyr}
  
  set.seed(seed_num)
  seed_vec <- sample(seq(1, 10000000, by = 1), n_splines, replace = FALSE)
  
  purrr::map_df(seed_vec, function(i){
    
    set.seed(i)
    n_control_points <- sample(seq(3, 6, by = 1), 1)
    
    set.seed(i)
    data <- data.frame(
      seed_num = i,
      x = runif(n_control_points, min = -0.2, max = 1.2),
      y = runif(n_control_points, min = -0.21, max = 1.21))
    
  }) -> splines_data
  
  set.seed(seed_num)
  splines_with_colours <- splines_data |>
    dplyr::distinct(seed_num) |>
    dplyr::mutate(
      # hex_fill = sample(palette, dplyr::n(), replace = TRUE),
      spline_size = sample(
        c(seq(0.05, 0.4, by = 0.05), c(0.6, 0.8, 1.1, 1.5)),
        n(), replace = TRUE,
        prob = c(rep(0.1, times = 8), c(0.1, 0.05, 0.03, 0.02))))
  
  splines_for_plot <- dplyr::left_join(
    splines_data, splines_with_colours, by = "seed_num")
  
  return(splines_for_plot)
  
}