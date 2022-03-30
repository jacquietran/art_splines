# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggforce)
library(ggplot2)

# Create data ------------------------------------------------------------------

iteration_id <- "test_0008"
initial_seed <- 789408
bg_colour <- "#FFFFFF"
colour_vec <- c(
  "#7be0ad", "#aee5d8", "#e7e5e5", "#e5d0e3", "#e0b0d5")
n_shapes <- 200

set.seed(initial_seed)
seed_vec <- sample(seq(1, 1000000, by = 1), n_shapes, replace = FALSE)

# Base rectangles
rect_data <- tibble::tibble(
  group = rep(1:4, each = 4),
  x = c(
    0, 0, 0.3, 0.3, # shape 1
    0, 0, 0.3, 0.3, # shape 2 
    0.35, 0.35, 1, 1, # shape 3
    0.35, 0.35, 1, 1), # shape 4
  y = c(
    0, 0.6, 0.6, 0, # shape 1
    0.65, 1, 1, 0.65, # shape 2
    0, 0.3, 0.3, 0, # shape 3
    0.35, 1, 1, 0.35)) # shape 4

# Splines data
splines_data <- purrr::map_df(seed_vec, function(i){
  
  set.seed(i)
  n_control_points <- sample(seq(3, 4, by = 1), 1)
  
  set.seed(i)
  data <- data.frame(
    seed_num = i,
    x = scales::rescale(runif(n_control_points), to = c(-0.2, 1.2)),
    y = scales::rescale(runif(n_control_points), to = c(-0.2, 1.2)))
  
})

set.seed(initial_seed)
splines_with_colours <- splines_data %>%
  distinct(seed_num) %>%
  mutate(
    hex_fill = sample(colour_vec, n(), replace = TRUE)) %>%
  left_join(splines_data, .)

# Build plot -------------------------------------------------------------------

ggplot() +
  #geom_shape(
  #  data = rect_data,
  #  aes(x = x, y = y, group = group, fill = group),
  #  radius = unit(0.5, "cm")) +
  ggfx::as_reference(
    geom_shape(
      data = rect_data,
      aes(x = x, y = y, group = group),
      radius = unit(0.25, "cm")),
    id = "rect") +
  ggfx::with_mask(
    geom_bspline_closed(
      data = splines_with_colours,
      aes(x = x, y = y, group = seed_num, fill = hex_fill),
      alpha = 0.8),
    mask = ggfx::ch_alpha("rect")) +
  scale_fill_identity() +
  coord_equal(xlim = c(0,1), ylim = c(0,1), expand = TRUE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export to file
ggsave(
  here::here(glue::glue("img/tests/{iteration_id}.png")),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 300)
