# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggforce)
library(ggplot2)

# Source custom functions ------------------------------------------------------

source(here::here("R/fx_throw_shapes.R"))
source(here::here("R/fx_create_rects2.R"))
source(here::here("R/fx_make_noise.R"))

# Set parameters ---------------------------------------------------------------

iteration_id <- "test_0019"
initial_seed <- 789419
bg_colour <- "#F1DCD0"
# For colourful splines
colour_vec1 <- c(
  "#ecc8af", "#e7ad99", "#ce796b", "#c18c5d", "#495867")
# For ghost splines oooOooOoOOooOoooo... darker to lighter
colour_vec2 <- c(
  "#C9ADA7", "#D4BCB7", "#DECBC6", "#E8DAD5", "#F2E9E4")
n_shapes <- 100

set.seed(initial_seed)
seed_vec <- sample(seq(1, 100000, by = 1), 2, replace = FALSE)

# Create data ------------------------------------------------------------------

# Splines data
splines_data1 <- throw_shapes(
  seed_num = seed_vec[1], n_shapes = n_shapes, palette = colour_vec1)

splines_data2 <- throw_shapes(
  seed_num = seed_vec[2], n_shapes = n_shapes, palette = colour_vec2)

# Rectangles
rect_data <- create_rects2(seed_num = initial_seed)

set.seed(initial_seed)
rect_w_subsets <- rect_data %>%
  distinct(group) %>%
  mutate(subset = sample(c(1,2,3), n(), replace = TRUE, prob = c(0.5, 0.3, 0.2))) %>%
  left_join(rect_data, ., by = "group")

rect_subset1 <- rect_w_subsets %>%
  filter(subset == 1)

rect_subset2 <- rect_w_subsets %>%
  filter(subset == 2)

rect_subset3 <- rect_w_subsets %>%
  filter(subset == 3)
  
# Make noise layer
noise <- make_noise(
  seed_num = initial_seed, colours = c("#FFFFFF", "#000000"),
  lower_limit = -0.1, upper_limit = 1.1, frequency = 2000, noise_type = "value")
noise_data <- noise$noise
noise_gradient <- noise$noise_gradient

# Build plot -------------------------------------------------------------------

ggplot() +
  ggfx::as_reference(
    geom_shape(
      data = rect_subset1,
      aes(x = x, y = y, group = group),
      radius = unit(0.325, "cm")),
    id = "rect1") +
  ggfx::with_mask(
    geom_bspline_closed(
      data = splines_data1,
      aes(x = x, y = y, group = seed_num, fill = hex_fill),
      alpha = 0.8),
    mask = ggfx::ch_alpha("rect1")) +
  ggfx::as_reference(
    geom_shape(
      data = rect_subset2,
      aes(x = x, y = y, group = group),
      radius = unit(0.325, "cm")),
    id = "rect2") +
  ggfx::with_mask(
    geom_bspline_closed(
      data = splines_data2,
      aes(x = x, y = y, group = seed_num, fill = hex_fill),
      alpha = 0.2),
    mask = ggfx::ch_alpha("rect2")) +
  geom_shape(
    data = rect_subset3,
    aes(x = x, y = y, group = group),
    radius = unit(0.325, "cm"), colour = colour_vec2[3], size = 0.25, fill = NA) +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  # Noise layer
  ggplot2::geom_raster(
    data = noise_data,
    ggplot2::aes(x, y, fill = noise),
    alpha = 0.15) +
  ggplot2::scale_fill_gradientn(colours = noise_gradient) +
  coord_equal(xlim = c(-0.1,1.1), ylim = c(-0.1,1.1), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(60, 60, 60, 60, unit = "pt"))

# Export to file
ggsave(
  here::here(glue::glue("img/tests/{iteration_id}.png")),
  last_plot(), width = 4000, height = 4000, units = "px", dpi = 600)

beepr::beep(10)
