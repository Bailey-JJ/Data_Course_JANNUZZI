# Final Ugly Plot ####
library(tidyverse)
library(dplyr)
library(ggimage)
library(patchwork)
library(palmerpenguins)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggtext")
library(ggtext)

pen <- penguins

oh_lord <- png::readPNG("./Assignments/Assignment_5/IMG_8859.png")

behold <- 
pen %>% 
  ggplot(aes(x = body_mass_g, y = bill_length_mm)) +
  background_image(oh_lord) +
  annotate("rect", 
           xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "white", alpha = .5) +
  theme(axis.text.x = element_text(angle = 152, face = 'bold.italic', color = c("yellow", "black", "red", "purple", "turquoise", "brown"), size = c(5, 8, 4, 9, 7, 12)),
        axis.text.y = element_text(angle = 45, color = c("blue", "red", "orange"), hjust = 0.7, vjust = 0.1, size = c(3, 5, 11)),
        axis.title.y.left = element_text(angle = 13, color = "salmon", size = 5, hjust = 1),
        axis.title.x.bottom = element_text(color = "brown", size = 9, angle = 349, hjust = 0.5, vjust = 0.3),
        plot.background = element_rect(fill = "chartreuse", colour = '#B81840', linewidth = 3.5, linetype = "246359"),
        strip.background = element_rect(fill = "#264EFF", color = "chartreuse",
                                        linewidth = 2, linetype = "twodash"),
        strip.text.y.right = element_text(color = "chartreuse", angle = 75, face = "bold")) +
  geom_col(aes(x = bill_length_mm, y = sex), width = 0.1, alpha = 0.05, inherit.aes = FALSE) +
  geom_point(aes(x = body_mass_g, y = bill_length_mm, color = flipper_length_mm), alpha = 0.5, shape = 8, size = 3, inherit.aes = FALSE) +
  scale_color_gradient(low = "green", high = "blue") +
  scale_x_continuous(breaks=seq(0,6000, by = 1198)) +
  facet_wrap(~sex, scales = "free_x", dir = "h", strip.position = "right") +
  theme(legend.position = "none") +
  labs(x = ~the^variables^were~the^friends~we~made^along^the[way],
       y = "so fun fact, 
            I had to remake
            most of this graph 
            three times, cause 
            R kept going unresponsive
            and deleting my work in 
            an pathetic attempt to 
            prevent me from making this 
            master piece")

ggsave("Assignments/Assignment_5/ugly_plot.png",
       plot = behold,
       width = 6.29, height = 2.94,
       units = "in",
       dpi = 105)
  
behold
