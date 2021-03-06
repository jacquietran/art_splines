# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggforce)
library(ggplot2)

# Source custom functions ------------------------------------------------------

source(here::here("R/fx_squish.R"))
source(here::here("R/fx_throw_shapes2.R"))
source(here::here("R/fx_make_noise.R"))

# Set parameters ---------------------------------------------------------------

iteration_id <- "test_0031"
initial_seed <- 789431
bg_colour <- "#F1DCD0"
bg_splines_colour <- "#E3B9A1"

# For colour gradient
colour_vec1 <- c(
  "#edae49", "#d1495b", "#00798c")
n_shapes_filled <- 150

# For splines to be drawn
n_splines_drawn <- 20

# Create data ------------------------------------------------------------------

# Squishy blob
blob_form <- squish(seed_num = initial_seed)

# Colour gradient for blob
blob_colour <- throw_shapes2(
  seed_num = initial_seed, n_shapes = n_shapes_filled, palette = colour_vec1)

# Splines to draw
splines <- throw_shapes2(
  seed_num = initial_seed, n_shapes = n_splines_drawn, palette = colour_vec1)

# Make noise layer
noise <- make_noise(
  seed_num = initial_seed, colours = c("#FFFFFF", "#000000"),
  lower_limit = -0.1, upper_limit = 1.1, frequency = 2000, noise_type = "value")
noise_data <- noise$noise
noise_gradient <- noise$noise_gradient

# Build plot -------------------------------------------------------------------

p <- ggplot() +
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
      sigma = 50),
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
  here::here(glue::glue("img/tests/{iteration_id}.png")),
  last_plot(), width = 4000, height = 4000, units = "px", dpi = 600)

beepr::beep(10)
