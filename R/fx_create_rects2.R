create_rects2 <- function(seed_num){
  
  # Requires {tibble}, {purrr}, {dplyr}, {tidyr}
  
  set.seed(seed_num)
  seed_vec <- sort(sample(seq(1, 1000000, by = 1), 10, replace = FALSE))
  
  break_window1 <- seq(0.1, 0.4, by = 0.02)
  break_window2 <- seq(0.5, 0.9, by = 0.02)
  
  col_width <- 0.09
  col_spacer <- 0.1/9
  
  col_breaks <- purrr::map_df(seed_vec, function(i){
    
    set.seed(i)
    data <- tibble::tibble(
      seed_num = i,
      break1_upper = sample(break_window1, 1),
      break2_lower = break1_upper + col_spacer,
      break2_upper = sample(break_window2, 1),
      break3_lower = break2_upper + col_spacer)
    
  })
  
  col_breaks_tidy <- col_breaks |>
    dplyr::mutate(column = dplyr::row_number())
  
  # Base rectangles
  rect_frame <- tibble::tibble(
    column = rep(1:10, each = 3),
    group = seq(1:30),
    group_in_column = rep(1:3, times = 10))
  
  rect_data <- dplyr::left_join(rect_frame, col_breaks_tidy, by = "column") |>
    dplyr::mutate(
      x1 = dplyr::case_when(
        group == 1:3   ~ 0,
        group == 4:6   ~ (1 * col_width) + (1 * col_spacer),
        group == 7:9   ~ (2 * col_width) + (2 * col_spacer),
        group == 10:12 ~ (3 * col_width) + (3 * col_spacer),
        group == 13:15 ~ (4 * col_width) + (4 * col_spacer),
        group == 16:18 ~ (5 * col_width) + (5 * col_spacer),
        group == 19:21 ~ (6 * col_width) + (6 * col_spacer),
        group == 22:24 ~ (7 * col_width) + (7 * col_spacer),
        group == 25:27 ~ (8 * col_width) + (8 * col_spacer),
        group == 28:30 ~ (9 * col_width) + (9 * col_spacer)),
      x2 = x1,
      x3 = dplyr::case_when(
        group == 1:3   ~ (1 * col_width),
        group == 4:6   ~ (2 * col_width) + (1 * col_spacer),
        group == 7:9   ~ (3 * col_width) + (2 * col_spacer),
        group == 10:12 ~ (4 * col_width) + (3 * col_spacer),
        group == 13:15 ~ (5 * col_width) + (4 * col_spacer),
        group == 16:18 ~ (6 * col_width) + (5 * col_spacer),
        group == 19:21 ~ (7 * col_width) + (6 * col_spacer),
        group == 22:24 ~ (8 * col_width) + (7 * col_spacer),
        group == 25:27 ~ (9 * col_width) + (8 * col_spacer),
        group == 28:30 ~ (10 * col_width) + (9 * col_spacer)),
      x4 = x3,
      y1 = dplyr::case_when(
        group_in_column == 1 ~ 0,
        group_in_column == 2 ~ break2_lower,
        group_in_column == 3 ~ break3_lower),
      y2 = dplyr::case_when(
        group_in_column == 1 ~ break1_upper,
        group_in_column == 2 ~ break2_upper,
        group_in_column == 3 ~ 1),
      y3 = y2,
      y4 = y1) |>
    dplyr::select(-seed_num, -group_in_column, -dplyr::starts_with("break"))
  
  rect_data_x <- rect_data |>
    dplyr::select(-dplyr::starts_with("y")) |>
    tidyr::pivot_longer(
      cols = c(dplyr::starts_with("x")),
      names_to = "coord_id_x",
      values_to = "x")
  
  rect_data_y <- rect_data |>
    dplyr::select(-dplyr::starts_with("x")) |>
    tidyr::pivot_longer(
      cols = c(dplyr::starts_with("y")),
      names_to = "coord_id_y",
      values_to = "y") |>
    dplyr::select(dplyr::ends_with("y"))
  
  rect_data_long <- dplyr::bind_cols(rect_data_x, rect_data_y) |>
    dplyr::select(column, group, x, y)
  
  return(rect_data_long)
  
}