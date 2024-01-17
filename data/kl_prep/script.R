library(tidyverse)
library(stringi)

lookup <- read_csv("lookup.csv", col_types = "ccc")

d <- c(
  file.path("task1", dir("task1")),
  file.path("task2", dir("task2"))
)

df <- map(d, function(v) {
  df <- read_csv(v)
  df$task <- stri_sub(v, 5L, 5L)
  df$file <- stri_sub(v, 15L, 27L)
  df  
}) |>
  bind_rows() |>
  filter(type %in% c("up", "down")) |>
  left_join(lookup, by = c("task", "file")) |>
  select(id, task, time, type, key, key_code)

df$key[is.na(df$key)] <- " "

df <- df |>
  group_by(id, task) |>
  mutate(time = round(time)) |>
  mutate(time = as.integer(time)) |>
  mutate(time = time - min(time))

df <- df |>
  filter(stri_sub(key_code, 1L, 3L) %in% c("Key", "Com", "Spa", "Sem", "Quo", "Per")) |>
  group_by(id, task, key_code) |>
  arrange(id, task, key_code, time) |>
  mutate(
    duration = lead(time) - time
  ) |>
  filter(type == "down", lead(type) == "up") |>
  ungroup() |>
  arrange(id, task, time) |>
  select(id, task, time, duration, key)

# extra metrics
df <- df |>
  group_by(id, task) |>
  mutate(gap1 = lead(time) - time) |>
  mutate(gap2 = lead(time) - (time + duration)) |>
  ungroup()

# some sanity checks
t.test(
  df$gap1[df$task == 1 & df$key == " "],
  df$gap1[df$task == 1 & df$key != " "]
)

write_csv(df, "../keylog.csv")

# words

df_word <- df |>
  group_by(id, task) |>
  mutate(iwd = lead(time) - lag(time + duration)) |>
  filter(key == " ") |>
  ungroup() |>
  filter(!is.na(iwd))



