create_rects <- function(seed_num){
  
  # Requires {tibble}
  
  set.seed(seed_num)
  seed_vec <- sample(seq(1, 1000000, by = 1), 5, replace = FALSE)
  
  break_window1 <- seq(0.1, 0.4, by = 0.02)
  break_window2 <- seq(0.5, 0.9, by = 0.02)
  
  set.seed(seed_vec[1])
  col1_breaks <- c(sample(break_window1, 1), sample(break_window2, 1))
  
  set.seed(seed_vec[2])
  col2_breaks <- c(sample(break_window1, 1), sample(break_window2, 1))
  
  set.seed(seed_vec[3])
  col3_breaks <- c(sample(break_window1, 1), sample(break_window2, 1))
  
  set.seed(seed_vec[4])
  col4_breaks <- c(sample(break_window1, 1), sample(break_window2, 1))
  
  set.seed(seed_vec[5])
  col5_breaks <- c(sample(break_window1, 1), sample(break_window2, 1))
  
  # Base rectangles
  rect_data <- tibble::tibble(
    group = rep(1:15, each = 4),
    x = c(
      rep(c(0, 0, 0.18, 0.18), times = 3),           # column 1
      rep(c(0.205, 0.205, 0.385, 0.385), times = 3), # column 2
      rep(c(0.41, 0.41, 0.59, 0.59), times = 3), # column 3
      rep(c(0.615, 0.615, 0.795, 0.795), times = 3), # column 4
      rep(c(0.82, 0.82, 1, 1), times = 3)        # column 5
    ), 
    y = c(
      # column 1
      0, col1_breaks[1], col1_breaks[1], 0, # shape 1
      (col1_breaks[1] + 0.025), col1_breaks[2], col1_breaks[2], (col1_breaks[1] + 0.025), # shape 2
      (col1_breaks[2] + 0.025), 1, 1, (col1_breaks[2] + 0.025), # shape 3
      # column 2
      0, col2_breaks[1], col2_breaks[1], 0, # shape 1
      (col2_breaks[1] + 0.025), col2_breaks[2], col2_breaks[2], (col2_breaks[1] + 0.025), # shape 2
      (col2_breaks[2] + 0.025), 1, 1, (col2_breaks[2] + 0.025), # shape 3
      # column 3
      0, col3_breaks[1], col3_breaks[1], 0, # shape 1
      (col3_breaks[1] + 0.025), col3_breaks[2], col3_breaks[2], (col3_breaks[1] + 0.025), # shape 2
      (col3_breaks[2] + 0.025), 1, 1, (col3_breaks[2] + 0.025), # shape 3
      # column 4
      0, col4_breaks[1], col4_breaks[1], 0, # shape 1
      (col4_breaks[1] + 0.025), col4_breaks[2], col4_breaks[2], (col4_breaks[1] + 0.025), # shape 2
      (col4_breaks[2] + 0.025), 1, 1, (col4_breaks[2] + 0.025), # shape 3
      # column 5
      0, col5_breaks[1], col5_breaks[1], 0, # shape 1
      (col5_breaks[1] + 0.025), col5_breaks[2], col5_breaks[2], (col5_breaks[1] + 0.025), # shape 2
      (col5_breaks[2] + 0.025), 1, 1, (col5_breaks[2] + 0.025)) # shape 3
      )
  
  return(rect_data)
  
}