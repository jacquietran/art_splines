# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggforce)
library(ggplot2)

# Source custom functions ------------------------------------------------------

source(here::here("R/fx_squish.R"))
source(here::here("R/fx_throw_shapes2.R"))
source(here::here("R/fx_draw_with_marker.R"))
source(here::here("R/fx_make_noise.R"))

# Set parameters ---------------------------------------------------------------

iteration_id <- "splines_0017"
initial_seed <- 496617

bg_colour <- "#F1DCD0"
bg_splines_colour <- "#E3B9A1" # a couple shades darker than bg_colour
bg_marker_colour <- "#EED4C5" # in between bg_colour and bg_splines_colour

# For colour gradient - 3 to 5 colours seems to work well
colour_vec <- c("#F8C7CC", "#81A684", "#57886C", "#466060")
n_shapes_filled <- 100

# For splines to be drawn
n_splines_drawn <- 15

# For marker lines to be drawn
set.seed(initial_seed)
n_marker_lines <- sample(2:5, 1)

# For noise layer
noise_colours <- c("#FFFFFF", "#000000")
noise_type <- "perlin"
# Type can be any one of: "value", "perlin", "cubic", "simplex", "waves",
# "white", "worley", "checkerboard", "spheres"

# Create data ------------------------------------------------------------------

# Squishy blob
blob_form <- squish(seed_num = initial_seed)

# Colour gradient for blob
blob_colour <- throw_shapes2(
  seed_num = initial_seed, n_shapes = n_shapes_filled, palette = colour_vec)

# Marker lines
marker_lines <- draw_with_marker(
  seed_num = initial_seed, n_lines = n_marker_lines)

# Splines to draw
splines <- throw_shapes2(
  seed_num = initial_seed, n_shapes = n_splines_drawn, palette = colour_vec)

# Make noise layer
noise <- make_noise(
  seed_num = initial_seed, colours = noise_colours, lower_limit = -0.1,
  upper_limit = 1.1, frequency = 2000, noise_type = noise_type)
noise_data <- noise$noise
noise_gradient <- noise$noise_gradient

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  # Marker lines
  geom_segment(
    data = marker_lines,
    aes(x = x, xend = xend, y = y, yend = yend, group = seed_num,
        size = spline_size),
    colour = bg_marker_colour) +
  # Background splines
  geom_bspline(
    data = splines,
    aes(x = x, y = y, group = seed_num, size = spline_size),
    colour = bg_splines_colour) +
  # Colourful blob
  ggfx::as_reference(
    geom_bspline_closed(
      data = blob_form,
      aes(x = x, y = y)),
    id = "blob") +
  ggfx::with_mask(
    ggfx::with_blur(
      geom_bspline_closed(
        data = blob_colour,
        aes(x = x, y = y, group = seed_num, fill = hex_fill),
        alpha = 0.4, radius = unit(1, "cm")),
      sigma = 80),
    mask = ggfx::ch_alpha("blob")) +
  # Foreground splines
  ggfx::with_mask(
    geom_bspline(
      data = splines,
      aes(x = x, y = y, group = seed_num, size = spline_size),
      colour = bg_colour),
    mask = ggfx::ch_alpha("blob")) +
  scale_size_identity() +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
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
  here::here(glue::glue("img/{iteration_id}.png")),
  last_plot(), width = 4000, height = 4000, units = "px", dpi = 600)

beepr::beep(10)
