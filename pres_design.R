library(ggplot2)
set.seed(5)

# Title Background
xmax <- 960/20
ymax <- 700/20

bg <- data.frame(x = rep(seq(1, xmax), ymax),
                 y = rep(seq(1, ymax), each = xmax),
                 miss = as.factor(rbinom(ymax * xmax, 1, prob = .1)))

ggplot(data = bg,
       aes(x, y, col = miss)) +
  geom_point(shape = 18) +
  scale_color_manual(values = c("#5E517B", "#36265B")) +
  theme_void() +
  coord_cartesian(xlim = c(2.5, xmax-1.5),
                  ylim = c(2.5, ymax-1.5)) +
  annotate("rect", 
           xmin = 0, xmax = xmax*2, 
           ymin = ymax/2 - 10, ymax = ymax/2 + 10,
           alpha = .75, fill = "#36265B") +
  theme(panel.background = element_rect(fill = "#36265B",
                                        color = "#36265B"),
        legend.position = "none")
ggsave("theme/title_background.png",
       width = 5,
       height = 5 * (700/960))

ggplot(data = bg,
       aes(x, y, col = miss)) +
  geom_point(shape = 18) +
  scale_color_manual(values = c("#5E517B", "#36265B")) +
  theme_void() +
  coord_cartesian(xlim = c(2.5, xmax-1.5),
                  ylim = c(2.5, ymax-1.5)) +
  annotate("rect", 
           xmin = 0, xmax = xmax*2, 
           ymin = 0, ymax = ymax*2,
           alpha = .75, fill = "#36265B") +
  theme(panel.background = element_rect(fill = "#36265B",
                                        color = "#36265B"),
        legend.position = "none")
ggsave("theme/normal_background.png",
       width = 5,
       height = 5 * (700/960))