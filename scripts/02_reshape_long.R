reshape_burden_long <- function(df_clean) {
  specs <- get_age_interval_specs()

  df_clean |>
    mutate(respondent_id = row_number()) |>
    pivot_longer(
      cols = all_of(specs$burden_cols),
      names_to = "age_interval_col",
      values_to = "burden"
    ) |>
    mutate(
      age_interval = factor(
        age_interval_col,
        levels = specs$burden_cols,
        labels = specs$age_interval_labels
      ),
      # Keep chronological order for plotting and grouping.
      age_interval_order = as.integer(age_interval)
    ) |>
    select(
      respondent_id,
      timestamp,
      synnytitko_lapsi,
      mita_lasta,
      sisarukset_ennen_3v,
      age_interval,
      age_interval_order,
      burden
    )
}
