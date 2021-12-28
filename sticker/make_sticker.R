### install package 'hexSticker'
remotes::install_github("GuangchuangYu/hexSticker")
library(hexSticker)
library(showtext)   # Using Fonts More Easily in R Graphs

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Inconsolata", "incon")

### create sticker
sticker(
  # Subplot (image)
  subplot = "sticker/graphic.jpg",
  s_y = 1.05,                          # Position of the sub plot (y)
  s_x = 0.975,                       # Position of the sub plot (x)
  s_width = 1.5,                    # Width of the sub plot
  s_height = 1.5,                   # Height of the sub plot
  # Font
  package = "alternating    optimization",
  p_size = 9,                      # Font size of the text
  p_y = 0.75,                        # Position of the font (y)
  p_x = 1,                        # Position of the font (x)
  p_family = "incon",               # Defines font
  p_color = "#2E3532",
  # Spotlight
  spotlight = TRUE,                 # Enables spotlight
  l_y = 1.2,                        # Position of spotlight (y)
  l_x = 1.2,                        # Position of spotlight (x)
  # Sticker colors
  h_fill = "#2E3532",               # Color for background
  h_color = "#2E3532",              # Color for border
  # Save
  white_around_sticker=TRUE,        # Puts white triangles in the corners
  filename="man/figures/logo.png"   # Sets file name and location where to store the sticker
)
