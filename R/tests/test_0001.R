# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggforce)
library(ggplot2)

# Create data ------------------------------------------------------------------

iteration_id <- "test_0001"
initial_seed <- 112455
bg_colour <- "#B0BABF"
colour_vec <- c(
  "#002F3D", "#F7EDF0", "#F4F482", "#236C6B", "#F18F01", "#AD343E", "#6BBAEC")
n_shapes <- 8

set.seed(initial_seed)
seed_vec <- sample(seq(1, 1000000, by = 1), n_shapes, replace = FALSE)

data <- purrr::map_df(seed_vec, function(i){
  
  set.seed(i)
  data <- data.frame(
    seed_num = i,
    x = runif(4),
    y = runif(4))
  
})

set.seed(initial_seed)
data_with_colours <- data %>%
  distinct(seed_num) %>%
  mutate(
    hex_fill = sample(colour_vec, n(), replace = TRUE)) %>%
  left_join(data, .)

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_bspline_closed(
    data = data_with_colours,
    aes(x = x, y = y, group = seed_num, fill = hex_fill),
    alpha = 0.8) +
  scale_fill_identity() +
  coord_equal() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export to file
ggsave(
  here::here(glue::glue("img/tests/{iteration_id}.png")),
  last_plot(), width = 10, height = 10, units = "cm", dpi = 300)
