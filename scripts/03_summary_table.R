make_age_interval_summary <- function(df_long) {
  # We do not impute; genuine missingness stays NA.
  # Summary stats are computed per interval using available (non-missing) values only.
  df_long |>
    group_by(age_interval, age_interval_order) |>
    summarise(
      n = sum(!is.na(burden)),
      mean = mean(burden, na.rm = TRUE),
      median = median(burden, na.rm = TRUE),
      sd = sd(burden, na.rm = TRUE),
      q25 = quantile(burden, 0.25, na.rm = TRUE, names = FALSE, type = 7),
      q75 = quantile(burden, 0.75, na.rm = TRUE, names = FALSE, type = 7),
      .groups = "drop"
    ) |>
    arrange(age_interval_order) |>
    select(age_interval, n, mean, median, sd, q25, q75)
}
