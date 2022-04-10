draw_with_marker <- function(seed_num, n_lines){
  
  # Requires {purrr}, {tibble}, {dplyr}
  
  set.seed(seed_num)
  seed_vec <- sample(seq(1, 10000000, by = 1), n_lines, replace = FALSE)
  
  purrr::map_df(seed_vec, function(i){
    
    set.seed(i)
    data <- tibble::tibble(
      seed_num = i,
      x = -0.2,
      xend = 1.2,
      y = runif(1, min = -0.2, max = 1.2),
      yend = runif(1, min = -0.21, max = 1.21))
    
  }) -> marker_lines
  
  set.seed(seed_num)
  line_sizes <- marker_lines |>
    dplyr::distinct(seed_num) |>
    dplyr::mutate(
      spline_size = sample(
        c(seq(10, 25, by = 2), c(27, 31, 39, 51)),
        n(), replace = TRUE,
        prob = c(rep(0.1, times = 8), c(0.1, 0.05, 0.03, 0.02))))
  
  lines_for_plot <- dplyr::left_join(
    marker_lines, line_sizes, by = "seed_num")
  
  return(lines_for_plot)
  
}