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

# Figures for flowchart
# Spectral curves
p1_2 <- p1 +
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p1_2

ggsave("./inst/figures/src_flowchart_usecase1.png", width=8, height=6, unit="cm", dpi=600)

# Stat. summary
p2_2 <- p2 +
  guides(colour = FALSE) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p2_2

ggsave("./inst/figures/statsum_flowchart_usecase1.png", width=8, height=6, unit="cm", dpi=600)

# Violins

dummy_wavelength <-  c(492.4, 559.8, 664.6, 704.1, 740.5, 782.8, 832.8, 864.7, 1613.7, 2202.4)
bands <-  c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")
waves <-  cbind(bands, dummy_wavelength)
colnames(waves)[1] <-  "variable"

# Reshape the dataframe to make it appropriate to ggplot2 syntax
p3_1 <- tibble::as_tibble(reflectance) %>%
  reshape2::melt(id = "label") %>%
  left_join(as.data.frame(waves)) %>%
  mutate(across(label, as.factor)) %>%
  mutate(across(dummy_wavelength, as.numeric)) %>%
  mutate(across(variable, as.factor)) %>%
  mutate(across(value, as.numeric)) %>%
  mutate(variable = factor(variable,
                           levels = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12"))) %>%
  na.omit() %>%
  filter(variable == "B2") %>%
  ggplot(aes(x=label, y= value, fill = label))+
  geom_violin(trim = FALSE)+
  # facet_wrap( ~ variable,
  #             ncol = 2,
  #             scales = "free") +
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p3_1

ggsave("./inst/figures/violin_flowchart_usecase1.png", width=8, height=6, unit="cm", dpi=600)
