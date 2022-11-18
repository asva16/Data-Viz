library(ggplot2)
theme_set(theme_minimal())
library(dplyr)

url <- "https://miro.medium.com/max/4800/1*rJTSE6CdajKcSw32xRhGDQ.png"
img <- magick::image_read((url))
pic <- grid::rasterGrob(img, interpolate = TRUE)

stat_iris = iris %>% group_by(Species) %>%
  summarise(mean.pl=mean(Petal.Length),
            sd.pl=sd(Petal.Length),
            mean.pw=mean(Petal.Width),
            sd.pw=sd(Petal.Width))

ggplot(iris, aes(Petal.Length, Petal.Width, col=Species)) + 
  geom_point(aes(size=Sepal.Length, col = stage(Species, after_scale = alpha(col, 0.6)))) +
  guides(col='none') +
  geom_segment(data = stat_iris, aes(x=mean.pl-2*sd.pl, xend=mean.pl+2*sd.pl, y=mean.pw, yend=mean.pw), show.legend = F) +
  geom_segment(data = stat_iris, aes(x=mean.pl, xend=mean.pl, y=mean.pw-2*sd.pw, yend=mean.pw+2*sd.pw), show.legend = F) +
  geom_label(data = stat_iris, aes(x=mean.pl, y=mean.pw, label=Species), nudge_x = 0, nudge_y = 0.5, show.legend = F) +
  annotation_custom(pic, ymin = 0, ymax = .8, xmin = 3.2, xmax = 7) +
  theme(legend.position = c(0.84,0.35),
        legend.direction = 'horizontal',
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  labs(x='Petal Length', y='Petal WIdth', title = 'Petal Dimension of Iris', 
       subtitle = 'Scatterplot of petal length vs petal width with mean +/- sd')
