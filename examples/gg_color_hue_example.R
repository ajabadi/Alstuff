library(ggplot2)
N <- nlevels(factor(mtcars$carb))
ggplot(mtcars, aes(wt, disp, col=factor(carb))) +
  geom_point() +
  scale_color_manual(values = gg_color_hue(N))
