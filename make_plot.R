library(tidyverse)

top_songs <- read_rds("clean_data.rds")

billboard_plot <- top_songs |>
  ggplot(aes(x = week_num, y = rank, color = song_artist)) +
  geom_line(linewidth = 0.8) +
  scale_y_reverse(breaks = c(1, 20, 40, 60, 80, 100)) +
  scale_x_continuous(breaks = seq(0, 52, by = 5)) +
  labs(
    title = "Billboard Hot 100 Trajectories (2000)",
    subtitle = "Weekly ranking of top July songs across 2000",
    x = "Week of 2000",
    y = "Chart Rank (1 is top)",
    color = "Song"
  ) +
  theme_minimal()

ggsave("billboard.png", billboard_plot)
