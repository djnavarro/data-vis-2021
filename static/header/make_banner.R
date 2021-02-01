# create banner
library(tidyverse)
library(e1071)

generate_series <- function(seed) {

  set.seed(seed)

  n <- 100000
  dat <- tibble(
    x = 1:n,
    y = c(rbridge(1, n)),
    seed = seed
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

  return(dat)

}

dat <- generate_series(4)

long_dat <- map_dfr(100 + (1:5), generate_series)


pic <- ggplot(dat, aes(x,y, group = seed)) +
  geom_path(data = long_dat, colour = "#888888", size = .1) +
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
