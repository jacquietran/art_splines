squish2 <- function(seed_num){
  
  # Requires {tibble}
  
  set.seed(seed_num)
  seed_vec <- sample(seq(1, 1000000, by = 1), 8, replace = FALSE)
  
  # Select x coords for control points
  set.seed(seed_vec[1])
  x1 <- runif(1, min = -0.3, max = 0.3)
  set.seed(seed_vec[2])
  x2 <- runif(1, min = -0.3, max = 0.3)
  set.seed(seed_vec[3])
  x3 <- runif(1, min = 0.7, max = 1.2)
  set.seed(seed_vec[4])
  x4 <- runif(1, min = 0.7, max = 1.2)
  
  # Select y coords for control points
  set.seed(seed_vec[5])
  y1 <- runif(1, min = -0.3, max = 0.3)
  set.seed(seed_vec[6])
  y2 <- runif(1, min = 0.7, max = 1.2)
  set.seed(seed_vec[7])
  y3 <- runif(1, min = 0.7, max = 1.2)
  set.seed(seed_vec[8])
  y4 <- runif(1, min = -0.3, max = 0.3)
  
  set.seed(seed_num)
  squishy_blob <- tibble::tibble(
    x = c(x1, x2, x3, x4),
    y = c(y1, y2, y3, y4))
  
  return(squishy_blob)
  
}