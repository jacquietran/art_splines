# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggforce)
library(ggplot2)

# Source custom functions ------------------------------------------------------

source(here::here("R/fx_squish.R"))
source(here::here("R/fx_throw_shapes2.R"))
source(here::here("R/fx_make_noise.R"))

# Set parameters ---------------------------------------------------------------

iteration_id <- "aflw_splines"
initial_seed <- 24568 # First AFLW game was a "lockout" with 24,568 in the crowd

bg_colour <- "#F1DCD0"
bg_splines_colour <- "#E3B9A1" # a couple shades darker than bg_colour

# For colour gradient - 3 to 5 colours seems to work well
# AFLW brand colour - #F16C51
colour_vec <- c(
  "#f16c51", "#F69F8E", "#FDEFEC")

n_shapes_filled <- 44 # 44 players took part in the first game

# For splines to be drawn
# n_splines_drawn <- 8 # 8 goals kicked in total

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

# Splines to draw
# 1 goal, 5 behinds, 11 points total
splines_pies_goals <- throw_shapes2(
  seed_num = 11, n_shapes = 1, palette = colour_vec)
splines_pies_points <- throw_shapes2(
  seed_num = 22, n_shapes = 5, palette = colour_vec)
# 7 goals, 4 behinds, 46 points total
splines_blues_goals <- throw_shapes2(
  seed_num = 46, n_shapes = 7, palette = colour_vec)
splines_blues_points <- throw_shapes2(
  seed_num = 92, n_shapes = 4, palette = colour_vec)

# Make noise layer
noise <- make_noise(
  seed_num = initial_seed, colours = noise_colours, lower_limit = -0.1,
  upper_limit = 1.1, frequency = 2000, noise_type = noise_type)
noise_data <- noise$noise
noise_gradient <- noise$noise_gradient

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  # Noise layer
  ggplot2::geom_raster(
    data = noise_data,
    ggplot2::aes(x, y, fill = noise),
    alpha = 0.05) +
  ggplot2::scale_fill_gradientn(colours = noise_gradient) +
  ggnewscale::new_scale_fill() +
  # Background splines
  geom_bspline(
    data = splines_pies_points,
    aes(x = x, y = y, group = seed_num),
    colour = "#000000", size = 0.2) +
  geom_bspline(
    data = splines_blues_points,
    aes(x = x, y = y, group = seed_num, size = spline_size),
    colour = "#BBBFC0", size = 0.2) +
  geom_bspline(
    data = splines_pies_goals,
    aes(x = x, y = y, group = seed_num),
    colour = "#000000", size = 0.8) +
  geom_bspline(
    data = splines_blues_goals,
    aes(x = x, y = y, group = seed_num, size = spline_size),
    colour = "#BBBFC0", size = 0.8) +
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
      sigma = 50),
    mask = ggfx::ch_alpha("blob")) +
  # Foreground splines
  geom_bspline(
    data = splines_pies_points,
    aes(x = x, y = y, group = seed_num),
    colour = "#FFFFFF", size = 0.2) +
  geom_bspline(
    data = splines_blues_points,
    aes(x = x, y = y, group = seed_num, size = spline_size),
    colour = "#051829", size = 0.2) +
  ggfx::with_mask(
    geom_bspline(
      data = splines_pies_goals,
      aes(x = x, y = y, group = seed_num),
      colour = "#FFFFFF", size = 0.8),
    mask = ggfx::ch_alpha("blob")) +
  ggfx::with_mask(
    geom_bspline(
      data = splines_blues_goals,
      aes(x = x, y = y, group = seed_num, size = spline_size),
      colour = "#051829", size = 0.8),
    mask = ggfx::ch_alpha("blob")) +
  scale_size_identity() +
  scale_fill_identity() +
  coord_equal(xlim = c(-0.1,1.1), ylim = c(-0.1,1.1), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(60, 60, 60, 60, unit = "pt"))

# Export to file
ggsave(
  here::here(glue::glue("misc/{iteration_id}.png")),
  last_plot(), width = 4000, height = 4000, units = "px", dpi = 600)

beepr::beep(10)
