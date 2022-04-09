squishies <- function(seed_num, n_splines){
  
  # Requires {purrr}, {tibble}
  
  set.seed(seed_num)
  seed_vec <- sample(seq(1, 10000000, by = 1), n_splines, replace = FALSE)
  
  purrr::map_df(seed_vec, function(i){
    
    set.seed(i)
    seed_vec <- sample(seq(1, 2000000, by = 1), 8, replace = FALSE)
    
    # Select x coords for control points
    set.seed(seed_vec[1])
    x1 <- runif(1, min = -0.2, max = 0.45)
    set.seed(seed_vec[2])
    x2 <- runif(1, min = -0.2, max = 0.45)
    set.seed(seed_vec[3])
    x3 <- runif(1, min = 0.55, max = 1.2)
    set.seed(seed_vec[4])
    x4 <- runif(1, min = 0.55, max = 1.2)
    
    # Select y coords for control points
    set.seed(seed_vec[5])
    y1 <- runif(1, min = -0.2, max = 0.45)
    set.seed(seed_vec[6])
    y2 <- runif(1, min = 0.55, max = 1.2)
    set.seed(seed_vec[7])
    y3 <- runif(1, min = 0.55, max = 1.2)
    set.seed(seed_vec[8])
    y4 <- runif(1, min = -0.2, max = 0.45)
    
    set.seed(i)
    squishy_blob <- tibble::tibble(
      seed_num = i,
      x = c(x1, x2, x3, x4),
      y = c(y1, y2, y3, y4))
    
  }) -> squishies_data
  
  set.seed(seed_num)
  squishies_sizes <- squishies_data |>
    dplyr::distinct(seed_num) |>
    dplyr::mutate(
      # hex_fill = sample(palette, dplyr::n(), replace = TRUE),
      spline_size = sample(
        c(seq(0.05, 0.4, by = 0.05), c(0.6, 0.8, 1.1, 1.5)),
        n(), replace = TRUE,
        prob = c(rep(0.1, times = 8), c(0.1, 0.05, 0.03, 0.02))))
  
  squishies <- dplyr::left_join(
    squishies_data, squishies_sizes, by = "seed_num")
  
  return(squishies)
  
}