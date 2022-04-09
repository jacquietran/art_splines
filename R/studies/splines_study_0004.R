# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggforce)
library(ggplot2)

# Source custom functions ------------------------------------------------------

source(here::here("R/fx_draw_splines.R"))
source(here::here("R/fx_make_noise.R"))

# Set parameters ---------------------------------------------------------------

iteration_id <- "splines_study_0004"
initial_seed <- 457204

bg_colour <- "#ebf2fa"
bg_splines_colour <- "#427AA1" # a few shades darker than bg_colour

# For splines to be drawn
n_splines_drawn <- 50

# For noise layer
noise_colours <- c("#FFFFFF", "#000000")
noise_type <- "perlin"
# Type can be any one of: "value", "perlin", "cubic", "simplex", "waves",
# "white", "worley", "checkerboard", "spheres"

# Create data ------------------------------------------------------------------

# Splines to draw
splines <- draw_splines(
  seed_num = initial_seed, n_splines = n_splines_drawn)

# Dots to "erode" splines
dots <- tibble::tibble(
  x = runif(1000, min = -0.1, max = 1.1),
  y = runif(1000, min = -0.11, max = 1.11))

# Make noise layer
noise <- make_noise(
  seed_num = initial_seed, colours = noise_colours, lower_limit = -0.1,
  upper_limit = 1.1, frequency = 2000, noise_type = noise_type)
noise_data <- noise$noise
noise_gradient <- noise$noise_gradient

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  # Splines
  geom_bspline(
    data = splines,
    aes(x = x, y = y, group = seed_num), # size = spline_size),
    colour = bg_splines_colour, size = 0.1) +
  geom_point(
    data = dots,
    aes(x = x, y = y),
    colour = bg_colour) +
  # scale_size_identity() +
  # Noise layer
  ggplot2::geom_raster(
    data = noise_data,
    ggplot2::aes(x, y, fill = noise),
    alpha = 0.05) +
  ggplot2::scale_fill_gradientn(colours = noise_gradient) +
  coord_equal(xlim = c(-0.1,1.1), ylim = c(-0.1,1.1), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(60, 60, 60, 60, unit = "pt"))

# Export to file
ggsave(
  here::here(glue::glue("img/studies/{iteration_id}.png")),
  last_plot(), width = 4000, height = 4000, units = "px", dpi = 600)

beepr::beep(10)
