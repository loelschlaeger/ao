### install package 'hexSticker'
remotes::install_github("GuangchuangYu/hexSticker")
library(hexSticker)
library(showtext)   # Using Fonts More Easily in R Graphs

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Inconsolata", "incon")

### color palette (1. background, 2. border and pkgname, 3. plot)
cp = c("#C2F9BB","#2E3532","#2E3532")

### create subplot
library(ggplot2)
p = ggplot() +
  xlim(-1, 2.5) +
  geom_function(fun = ~ sin(10*pi*.x) / (2*.x) + (.x-1)^4, colour = cp[3], n = 1e4) +
  geom_point(aes(x=0.14, y=-2.8), colour="black", size = 2) +
  geom_point(aes(x=0, y=16.7), colour="black", size = 2) +
  theme_void() +
  theme_transparent()

### create sticker
sticker(
  # Subplot (image)
  subplot = p,
  s_y = 1,                          # Position of the sub plot (y)
  s_x = 1,                       # Position of the sub plot (x)
  s_width = 2,                    # Width of the sub plot
  s_height = 1.2,                   # Height of the sub plot
  # Font
  package = "ao",
  p_size = 70,                      # Font size of the text
  p_y = 1.2,                        # Position of the font (y)
  p_x = 1.3,                        # Position of the font (x)
  p_family = "incon",               # Defines font
  p_color = cp[2],
  # Spotlight
  spotlight = TRUE,                 # Enables spotlight
  l_y = 1.2,                        # Position of spotlight (y)
  l_x = 1.2,                        # Position of spotlight (x)
  # Sticker colors
  h_fill = cp[1],               # Color for background
  h_color = cp[2],              # Color for border
  # Save
  white_around_sticker=TRUE,        # Puts white triangles in the corners
  filename="man/figures/logo.png"   # Sets file name and location where to store the sticker
)
