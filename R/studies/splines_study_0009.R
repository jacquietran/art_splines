# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggforce)
library(ggplot2)

# Source custom functions ------------------------------------------------------

source(here::here("R/fx_squishies.R"))
source(here::here("R/fx_make_noise.R"))

# Set parameters ---------------------------------------------------------------

iteration_id <- "splines_study_0009"
initial_seed <- 457209

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
squishies <- squishies(
  seed_num = initial_seed, n_splines = n_splines_drawn)

# Make noise layer
noise <- make_noise(
  seed_num = initial_seed, colours = noise_colours, lower_limit = -0.1,
  upper_limit = 1.1, frequency = 2000, noise_type = noise_type)
noise_data <- noise$noise
noise_gradient <- noise$noise_gradient

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  # Splines
  geom_bspline_closed(
    data = squishies,
    aes(x = x, y = y, group = seed_num),
    fill = bg_splines_colour, alpha = 0.02) +
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
