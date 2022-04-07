# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggforce)
library(ggplot2)

# Source custom functions ------------------------------------------------------

source(here::here("R/fx_throw_shapes.R"))
source(here::here("R/fx_create_rects.R"))

# Set parameters ---------------------------------------------------------------

iteration_id <- "test_0009"
initial_seed <- 789409
bg_colour <- "#FFFFFF"
colour_vec <- c(
  "#7be0ad", "#aee5d8", "#e7e5e5", "#e5d0e3", "#e0b0d5")
n_shapes <- 100

# Create data ------------------------------------------------------------------

# Splines data
splines_data <- throw_shapes(
  seed_num = initial_seed, n_shapes = n_shapes, palette = colour_vec)

# Rectangles
rect_data <- create_rects(seed_num = initial_seed)
  

# Build plot -------------------------------------------------------------------

ggplot() +
  ggfx::as_reference(
    geom_shape(
      data = rect_data,
      aes(x = x, y = y, group = group),
      radius = unit(0.25, "cm")),
    id = "rect") +
  ggfx::with_mask(
    geom_bspline_closed(
      data = splines_data,
      aes(x = x, y = y, group = seed_num, fill = hex_fill),
      alpha = 0.8),
    mask = ggfx::ch_alpha("rect")) +
  scale_fill_identity() +
  coord_equal(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export to file
ggsave(
  here::here(glue::glue("img/tests/{iteration_id}.png")),
  last_plot(), width = 4000, height = 4000, units = "px", dpi = 600)
