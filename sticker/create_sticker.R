### define font
library("showtext")
font_add_google("Martel", "my_font")
showtext_auto()

### build plot
library("ggplot2")
p <- ggplot() +
  theme_void()

### build sticker
library("hexSticker")
sticker_file <- sticker(
  ### image
  subplot = p,
  s_x = 1,
  s_y = 1,
  s_width = 0.8,
  s_height = 1,
  ### package name
  package = "ao",
  p_x = 1,
  p_y = 1.5,
  p_color = "#000000",
  p_family = "my_font",
  p_fontface = "plain",
  p_size = 40,
  ### sticker
  h_size = 1.2,
  h_fill = "#C98986",
  h_color = "#000000",
  spotlight = FALSE,
  l_x = 0.9,
  l_y = 1.4,
  l_width = 2,
  l_height = 1,
  l_alpha = 0.8,
  white_around_sticker = FALSE,
  ### URL
  url = "loelschlaeger.de/ao",
  u_x = 1,
  u_y = 0.1,
  u_color = "#000000",
  u_family = "my_font",
  u_size = 7,
  u_angle = 30,
  ### save file
  filename = "sticker/ao_sticker.png",
  asp = 1,
  dpi = 300
)
plot(sticker_file)
