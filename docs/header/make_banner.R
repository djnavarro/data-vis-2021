# create banner
library(tidyverse)
library(e1071)

set.seed(4)

n <- 100000
dat <- tibble(
  x = 1:n,
  y = c(rbridge(1, n))
)

for(i in 1:100) {
  end <- round(rbeta(1, 2, 2) * n)
  start <- round(rbeta(1, 2, 2) * n)
  if(start > end) {
    tmp <- start
    start <- end
    end <- tmp
  }
  m <- end - start
  dat$y[start:end] <- dat$y[start:end] + c(rbridge(1, m + 1))
}

pic <- ggplot(dat, aes(x,y)) +
  geom_path(colour = "#ff69b4") +
  theme_void() +
  theme(plot.background = element_rect(
    fill = "#333333",
    colour = "#333333"
  ))

ggsave(
  filename = here::here("static", "header", "banner.png"),
  plot = pic,
  width = 5000/300,
  height = 1000/300,
  dpi = 300
)
