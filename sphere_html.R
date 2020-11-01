library(tourr)
library(tidyverse)
library(plotly)
library(htmltools)

s4_data <- geozoo::sphere.solid.random(4, 500)$points
bases <- save_history(s4_data, max = 5)
tour_path <- tourr::interpolate(bases, 0.1)
d <- dim(tour_path)
s4_tour <- NULL
for (i in 1:d[3]) {
  d1 <- s4_data %*% matrix(tour_path[,,i], ncol=2)
  d1 <- apply(d1, 2, function(x) x-mean(x))
  s4_tour <- rbind(s4_tour, cbind(d1, rep(i+10, nrow(d1))))
}

colnames(s4_tour) <- c("x", "y", "i")

p <- ggplot() +
  geom_point(data = as_tibble(s4_tour), aes(x = x, y = y, frame = i), size=1) +
  coord_fixed() +
  ggtitle("p=4") +
  theme_void() +
  theme(legend.position="none")
p <- ggplotly(p, width=300, height=300) %>%
  animation_opts(200, redraw = FALSE, easing = "linear", transition=0) %>%
  style(hoverinfo = 'none') %>%
  config(displayModeBar = F)

save_html(p, file="4sphere.html")

s10_data <- geozoo::sphere.solid.random(10, 500)$points
bases <- save_history(s10_data, max = 5)
tour_path <- tourr::interpolate(bases, 0.1)
d <- dim(tour_path)
s10_tour <- NULL
for (i in 1:d[3]) {
  d1 <- s10_data %*% matrix(tour_path[,,i], ncol=2)
  d1 <- apply(d1, 2, function(x) x-mean(x))
  s10_tour <- rbind(s10_tour, cbind(d1, rep(i+10, nrow(d1))))
}

colnames(s10_tour) <- c("x", "y", "i")

p <- ggplot() +
  geom_point(data = as_tibble(s10_tour), aes(x = x, y = y, frame = i), size=1) +
  coord_fixed() +
  ggtitle("p=10") +
  theme_void() +
  theme(legend.position="none")
p <- ggplotly(p, width=300, height=300) %>%
  animation_opts(200, redraw = FALSE, easing = "linear", transition=0) %>%
  style(hoverinfo = 'none') %>%
  config(displayModeBar = F)

save_html(p, file="10sphere.html")
