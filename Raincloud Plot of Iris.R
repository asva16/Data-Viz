data(iris)
library(ggplot2)
theme_set(theme_minimal())
library(ggdist)
library(tidyverse)
library(ragg)
library(colorspace)

str(iris)
ggplot(iris, aes(y=Species, x=Sepal.Length, fill=Species)) +
  stat_halfeye(justification = -.1, .width = 0) +
  geom_boxplot(width=0.1, outlier.colour = NA) +
  stat_summary(fun=mean, geom="point", shape=20, size = 3,color="black", fill="black") +
  geom_dots(side='bottom', justification = 1.1, binwidth = 5e-2) +
  theme(legend.position = 'none') + labs(y="")

ggplot(iris, aes(y=Species, x=Sepal.Length)) +
  stat_halfeye(aes(col = Species,
                   fill = after_scale(lighten(color, .5))),
               adjust = .5,
               width = .75,
               .width = 0,
               justification = -.3,
               point_color = NA) +
  geom_boxplot(aes(col = Species,
                   col = after_scale(darken(color, .1, space = "HLS")),
                   fill = after_scale(desaturate(lighten(color, .8), .4))),
               width = .42, 
               outlier.shape = NA) +
  geom_point(aes(color = Species,
                 color = after_scale(darken(color, .1, space = "HLS"))),
             fill = "white",
             shape = 21,
             stroke = .4,
             size = 2,
             position = position_jitter(seed = 1, height = .12)) +
  geom_point(aes(fill = Species),
             color = "transparent",
             shape = 21,
             stroke = .4,
             size = 2,
             alpha = .3,
             position = position_jitter(seed = 1, height = .12)) +
  stat_summary(geom = "text",
               fun = "median",
               aes(label = round(..x.., 2),
                   color = Species,
                   color = after_scale(darken(color, .1, space = "HLS"))),
               family = "Roboto Mono",
               fontface = "bold",
               size = 4.5,
               vjust = -3.5) +
  theme(legend.position = 'none')

iris %>% pivot_longer(!Species, names_to = 'var') %>%
  ggplot(aes(y=Species, x=value)) +
  stat_halfeye(aes(col = Species,
                   fill = after_scale(lighten(color, .5))),
               adjust = .5,
               width = .75,
               .width = 0,
               justification = -.3,
               point_color = NA) +
  geom_boxplot(aes(col = Species,
                   col = after_scale(darken(color, .1, space = "HLS")),
                   fill = after_scale(desaturate(lighten(color, .8), .4))),
               width = .42, 
               outlier.shape = NA) +
  geom_point(aes(color = Species,
                 color = after_scale(darken(color, .1, space = "HLS"))),
             fill = "white",
             shape = 21,
             stroke = .4,
             size = 2,
             position = position_jitter(seed = 1, height = .12)) +
  geom_point(aes(fill = Species),
             color = "transparent",
             shape = 21,
             stroke = .4,
             size = 2,
             alpha = .3,
             position = position_jitter(seed = 1, height = .12)) +
  stat_summary(geom = "text",
               fun = "median",
               aes(label = round(..x.., 2),
                   color = Species,
                   color = after_scale(darken(color, .1, space = "HLS"))),
               family = "Roboto Mono",
               fontface = "bold",
               size = 4.5,
               vjust = -3.5) +
  theme(legend.position = 'none') +
  facet_wrap(~var, nrow = 2, scales = 'free')

