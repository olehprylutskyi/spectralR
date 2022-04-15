# install.packages("hexSticker")
library(hexSticker)
library(ggplot2)

# Spectral curves
p1_1 <- p1 +
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  theme_void() +
  theme_transparent()

?sticker

# Package name above
sticker(p1_1, package="spectralR",
        p_size=20, p_color = "#2d2f31", p_y = 1.5,
        s_x=1, s_y=.85,
        s_width=1.3, s_height=1,
        h_fill="#f0f0f0", h_color="#2164bc",
        filename="inst/figures/curves.png")

# Package name below
sticker(p1_1, package="spectralR",
        p_size=20, p_color = "#2d2f31", p_y = 0.5,
        s_x=1, s_y=1.2,
        s_width=1.3, s_height=1,
        h_fill="#f0f0f0", h_color="#2164bc",
        filename="inst/figures/curves.png")

# Stat. summary
p2_1 <- p2 +
  guides(colour = FALSE) +
  theme_void() +
  theme_transparent()

sticker(p2_1, package="spectralR",
        p_size=20, p_color = "#2d2f31", p_y = 1.5,
        s_x=1, s_y=.85,
        s_width=1.3, s_height=1,
        h_fill="#ffffff", h_color="#2164bc",
        filename="inst/figures/stat.png")
