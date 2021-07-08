### install package 'hexSticker'
remotes::install_github("GuangchuangYu/hexSticker")
library(hexSticker)
library(ggplot2)

### graphic
p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() + theme_transparent()

### produce sticker
s = sticker(subplot = p,
            s_x=.8,
            s_y=.65,
            s_width=2.2,
            s_height=1.3,
            package="ao",
            p_color="darkblue",
            p_size=10,
            p_y = 1.45,
            h_fill="cornflowerblue",
            h_color="darkblue",
            url="alternating optimization",
            u_color="darkblue",
            u_size=2,
            filename="sticker/sticker.pdf")

### restart R session (necessary for some unknown reason)
.rs.restartR()
