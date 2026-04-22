library(tidyverse)

source("./scripts/00_helpers.R")
source("./scripts/01_load_clean.R")
source("./scripts/02_reshape_long.R")

df_clean <- load_and_clean_survey()
df_long <- reshape_burden_long(df_clean)

df_plot <- df_long |>
  filter(!is.na(burden), burden >= 0, burden <= 10) |>
  mutate(
    burden_int = as.integer(round(burden)),
    age_interval_order = as.integer(age_interval_order)
  )

df_tiles <- df_plot |>
  count(age_interval_order, burden_int, name = "n") |>
  tidyr::complete(age_interval_order = 1:8, burden_int = 0:10, fill = list(n = 0)) |>
  group_by(age_interval_order) |>
  mutate(pct = ifelse(sum(n) > 0, n / sum(n), 0)) |>
  ungroup()

nonzero <- df_tiles$pct[df_tiles$pct > 0]

cat("Unique pct (rounded to 6):", length(unique(round(df_tiles$pct, 6))), "\n")
cat("Unique nonzero pct (rounded to 6):", length(unique(round(nonzero, 6))), "\n")
cat("pct range:", paste(range(df_tiles$pct), collapse = " to "), "\n")

if (length(nonzero) > 0) {
  cat("Nonzero pct values (rounded, first 20):\n")
  print(head(sort(unique(round(nonzero, 6))), 20))
  cat("Nonzero pct values (rounded, last 20):\n")
  print(tail(sort(unique(round(nonzero, 6))), 20))
}

