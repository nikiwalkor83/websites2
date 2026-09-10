library(tidyverse)

billboard <- billboard |>
  mutate(date.entered = as.Date(date.entered),
         week_offset = as.numeric(difftime(date.entered, as.Date("2000-01-01"), units = "weeks")))

jul_week <- 26

billboard_jul <- billboard |>
  filter(date.entered <= as.Date("2000-07-01")) |>
  mutate(wk_idx = jul_week - week_offset,
         wk_idx = if_else(wk_idx >= 1 & wk_idx <= 76, wk_idx, NA_real_),
         jul_rank = NA_real_)

for (i in seq_len(nrow(billboard_jul))) {
  wk_idx <- billboard_jul$wk_idx[i]
  if (!is.na(wk_idx)) {
    col_name <- paste0("wk", wk_idx)
    billboard_jul$jul_rank[i] <- as.numeric(billboard_jul[[col_name]][i])
  }
}

top_5_songs <- billboard_jul |>
  filter(!is.na(jul_rank)) |>
  arrange(jul_rank) |>
  slice(1:5) |>
  pull(track)

billboard_filtered <- billboard |>
  filter(track %in% top_5_songs)

top_songs <- billboard_filtered |>
  select(artist, track, wk1:wk76) |>
  pivot_longer(cols = starts_with("wk"),
               names_to = "week_num",
               values_to = "rank") |>
  mutate(week_num = as.numeric(gsub("wk", "", week_num))) |>
  left_join(billboard_filtered |> select(track, week_offset), by = "track") |>
  mutate(week_num = week_num + week_offset) |>
  filter(week_num >= 0 & week_num <= 52) |>
  filter(!is.na(rank)) |>
  mutate(song_artist = paste0(track, " (", artist, ")"))
write_rds(top_songs, file = "clean_data.rds")