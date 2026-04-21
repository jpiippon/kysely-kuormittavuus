# Plotting functions for `scripts/run_pipeline.R`
# (Keeps visuals LinkedIn-friendly and avoids CI/error bars where not needed.)

plot_burden_responses_mean_ci <- function(df_long) {
  age_labels_ordered <- c(
    "0\u20133 vko",
    "3 vko\u20133 kk",
    "3\u20136 kk",
    "6\u201312 kk",
    "12\u201318 kk",
    "18\u201324 kk",
    "24\u201330 kk",
    "30\u201336 kk"
  )

  # Mean overlay by age interval order (1..8 in this dataset).
  df_mean <- df_long |>
    group_by(age_interval_order) |>
    summarise(
      n = sum(!is.na(burden)),
      mean = mean(burden, na.rm = TRUE),
      .groups = "drop"
    )

  # Median overlay by age interval order.
  df_median <- df_long |>
    group_by(age_interval_order) |>
    summarise(
      median = median(burden, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot(df_long, aes(x = age_interval_order, y = burden)) +
    geom_jitter(width = 0.15, height = 0, alpha = 0.18, size = 1.6, color = col_mid_blue) +
    geom_line(
      data = df_median,
      aes(x = age_interval_order, y = median, group = 1),
      color = "grey35",
      linewidth = 1.1,
      linetype = "dotted"
    ) +
    geom_line(
      data = df_mean,
      aes(x = age_interval_order, y = mean, group = 1),
      color = col_orange,
      linewidth = 1.2
    ) +
    geom_point(
      data = df_mean,
      aes(x = age_interval_order, y = mean),
      color = col_orange,
      size = 3.2
    ) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2.5)) +
    scale_x_continuous(
      breaks = seq_along(age_labels_ordered),
      labels = age_labels_ordered
    ) +
    labs(
      title = "Koettu kuormittavuus ensimm\u00e4isten 3 vuoden aikana",
      subtitle = "Yksil\u00f6lliset vastaukset sek\u00e4 keskiarvo (katkoviiva = mediaani)",
      x = "Ik\u00e4jakso",
      y = "Kuormitus (0\u201310)"
    ) +
    theme_linkedin() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

plot_burden_heatmap_mean_median <- function(df_long) {
  age_labels_ordered <- c(
    "0\u20133 vko",
    "3 vko\u20133 kk",
    "3\u20136 kk",
    "6\u201312 kk",
    "12\u201318 kk",
    "18\u201324 kk",
    "24\u201330 kk",
    "30\u201336 kk"
  )

  # Only valid (0\u201310) burden scores are used in the distribution tiles.
  df_plot <- df_long |>
    filter(!is.na(burden), burden >= 0, burden <= 10) |>
    mutate(
      burden_score = pmin(pmax(as.integer(round(burden)), 0), 10)
    )

  # Tile shares by age interval and (rounded) burden score.
  df_tiles <- df_plot |>
    count(age_interval_order, burden_score, name = "count") |>
    tidyr::complete(
      age_interval_order,
      burden_score = 0:10,
      fill = list(count = 0)
    ) |>
    group_by(age_interval_order) |>
    mutate(
      n = sum(count),
      share = ifelse(n > 0, count / n, 0)
    ) |>
    ungroup()

  df_mean <- df_plot |>
    group_by(age_interval_order) |>
    summarise(mean = mean(burden, na.rm = TRUE), .groups = "drop")

  df_median <- df_plot |>
    group_by(age_interval_order) |>
    summarise(median = median(burden, na.rm = TRUE), .groups = "drop")

  df_n <- df_plot |>
    count(age_interval_order, name = "n")

  ggplot(df_tiles, aes(x = age_interval_order, y = burden_score)) +
    geom_tile(width = 0.95, height = 0.95, aes(fill = share)) +
    scale_fill_gradient(low = col_heat_low, high = col_heat_high, guide = "none") +
    geom_line(
      data = df_mean,
      aes(x = age_interval_order, y = mean, group = 1),
      color = col_orange,
      linewidth = 1.6
    ) +
    geom_point(
      data = df_mean,
      aes(x = age_interval_order, y = mean),
      color = col_orange,
      size = 2.9
    ) +
    geom_line(
      data = df_median,
      aes(x = age_interval_order, y = median, group = 1),
      color = col_median_line,
      linewidth = 1.2,
      linetype = "dashed"
    ) +
    geom_point(
      data = df_median,
      aes(x = age_interval_order, y = median),
      color = col_median_line,
      size = 2.2
    ) +
    geom_text(
      data = df_n,
      aes(x = age_interval_order, y = 10, label = paste0("n=", n)),
      inherit.aes = FALSE,
      size = 3.4,
      color = col_dark_text,
      vjust = -0.7
    ) +
    scale_x_continuous(
      breaks = seq_along(age_labels_ordered),
      labels = age_labels_ordered
    ) +
    scale_y_continuous(
      limits = c(0, 10),
      breaks = seq(0, 10, 2.5)
    ) +
    coord_cartesian(clip = "off", ylim = c(0, 10)) +
  labs(
      title = "Kuormitus iän mukaan: jakauma",
      subtitle = "Väri = vastausten osuus (0\u201310); yhtenäinen = keskiarvo, katkoviiva = mediaani",
      x = "Ik\u00e4jakso",
      y = "Kuormitus (0\u201310)"
    ) +
    theme_linkedin_premium_heatmap()
}

plot_burden_responses_mean_ci_by_synnytitko <- function(df_long) {
  df_facets <- df_long |>
    mutate(
      synnytitko_label = dplyr::case_when(
        is.na(synnytitko_lapsi) ~ NA_character_,
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^Kyll", ignore_case = TRUE)) ~ "K\u0079ll\u00e4",
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^(Ei|En)$", ignore_case = TRUE)) ~ "Ei",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(synnytitko_label))

  age_labels_ordered <- c(
    "0\u20133 vko",
    "3 vko\u20133 kk",
    "3\u20136 kk",
    "6\u201312 kk",
    "12\u201318 kk",
    "18\u201324 kk",
    "24\u201330 kk",
    "30\u201336 kk"
  )

  df_mean <- df_facets |>
    group_by(synnytitko_label, age_interval_order) |>
    summarise(
      mean = mean(burden, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot(df_facets, aes(x = age_interval_order, y = burden)) +
    geom_jitter(width = 0.15, height = 0, alpha = 0.18, size = 1.6, color = col_mid_blue) +
    geom_line(
      data = df_mean,
      aes(x = age_interval_order, y = mean, group = 1),
      color = col_orange,
      linewidth = 1.2
    ) +
    geom_point(
      data = df_mean,
      aes(x = age_interval_order, y = mean),
      color = col_orange,
      size = 3.2
    ) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2.5)) +
    scale_x_continuous(
      breaks = seq_along(age_labels_ordered),
      labels = age_labels_ordered
    ) +
    facet_wrap(~synnytitko_label, ncol = 2) +
    labs(
      title = "Koettu kuormittavuus ensimm\u00e4isten 3 vuoden aikana",
      subtitle = "Verrataan vastauksia ryhmitt\u00e4in (synnytt\u00e4nyt vs. ei)",
      x = "Ik\u00e4jakso",
      y = "Kuormitus (0\u201310)"
    ) +
    theme_linkedin() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      strip.text = element_text(size = 12, face = "bold")
    )
}

plot_burden_scatter_by_synnytitko <- function(df_long) {
  age_labels_ordered <- c(
    "0\u20133 vko",
    "3 vko\u20133 kk",
    "3\u20136 kk",
    "6\u201312 kk",
    "12\u201318 kk",
    "18\u201324 kk",
    "24\u201330 kk",
    "30\u201336 kk"
  )

  df_facets <- df_long |>
    mutate(
      synnytitko_label = dplyr::case_when(
        is.na(synnytitko_lapsi) ~ NA_character_,
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^Kyll", ignore_case = TRUE)) ~ "K\u0079ll\u00e4",
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^(Ei|En)$", ignore_case = TRUE)) ~ "Ei",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(synnytitko_label))

  ggplot(df_facets, aes(x = age_interval_order, y = burden, color = synnytitko_label)) +
    geom_jitter(width = 0.15, height = 0, alpha = 0.35, size = 1.9) +
    # Make the categories visually distinct for scatter/jitter
    scale_color_manual(values = c("K\u0079ll\u00e4" = "#E63946", "Ei" = "#1D3557")) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2.5)) +
    scale_x_continuous(
      breaks = seq_along(age_labels_ordered),
      labels = age_labels_ordered
    ) +
    labs(
      title = "Kuormitus i\u00e4n mukaan: synnyttik\u00f6 vs. ei",
      subtitle = "Hajontakuva: yksil\u00f6lliset vastaukset (0\u201310)",
      x = "Ik\u00e4jakso",
      y = "Kuormitus (0\u201310)",
      color = NULL
    ) +
    theme_linkedin() +
    theme(
      panel.grid = element_blank(),
      legend.position = "bottom"
    )
}

plot_burden_scatter_by_mita_lasta_and_synnytitko <- function(df_long) {
  age_labels_ordered <- c(
    "0\u20133 vko",
    "3 vko\u20133 kk",
    "3\u20136 kk",
    "6\u201312 kk",
    "12\u201318 kk",
    "18\u201324 kk",
    "24\u201330 kk",
    "30\u201336 kk"
  )

  df_scatter <- df_long |>
    mutate(
      synnytitko_label = dplyr::case_when(
        is.na(synnytitko_lapsi) ~ NA_character_,
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^Kyll", ignore_case = TRUE)) ~ "K\u0079ll\u00e4",
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^(Ei|En)$", ignore_case = TRUE)) ~ "Ei",
        TRUE ~ NA_character_
      ),
      mita_lasta_label = dplyr::case_when(
        is.na(mita_lasta) ~ NA_character_,
        stringr::str_detect(as.character(mita_lasta), stringr::regex("^Ensimm", ignore_case = TRUE)) ~
          "Ensimm\u00e4ist\u00e4 lastani",
        stringr::str_detect(as.character(mita_lasta), stringr::regex("^Toista", ignore_case = TRUE)) ~
          "Toista tai my\u00f6hemp\u00e4\u00e4 lastani",
        stringr::str_detect(as.character(mita_lasta), stringr::regex("Lapsiani", ignore_case = TRUE)) ~
          "Lapsiani yleisesti",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(synnytitko_label) & !is.na(mita_lasta_label))

  # Put a stable order on facets.
  df_scatter <- df_scatter |>
    mutate(
      mita_lasta_label = factor(
        mita_lasta_label,
        levels = c(
          "Ensimm\u00e4ist\u00e4 lastani",
          "Toista tai my\u00f6hemp\u00e4\u00e4 lastani",
          "Lapsiani yleisesti"
        )
      )
    )

  ggplot(df_scatter, aes(x = age_interval_order, y = burden, color = synnytitko_label)) +
    geom_jitter(width = 0.15, height = 0, alpha = 0.35, size = 1.9) +
    # Make the categories visually distinct for scatter/jitter
    scale_color_manual(values = c("K\u0079ll\u00e4" = "#E63946", "Ei" = "#1D3557")) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2.5)) +
    scale_x_continuous(
      breaks = seq_along(age_labels_ordered),
      labels = age_labels_ordered
    ) +
    facet_wrap(~mita_lasta_label, ncol = 1) +
    labs(
      title = "Kuormitus i\u00e4n mukaan: synnyttik\u00f6 ja mihin lapsiin vastaus koskee",
      subtitle = "Hajontakuva: yksil\u00f6lliset vastaukset (0\u201310)",
      x = "Ik\u00e4jakso",
      y = "Kuormitus (0\u201310)",
      color = NULL
    ) +
    theme_linkedin() +
    theme(
      panel.grid = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(size = 12, face = "bold")
    )
}

plot_burden_variation_spaghetti <- function(df_long, df_summary) {
  # Alternative figure: show individual variation more directly.
  df_summary_plot <- df_summary %>%
    mutate(
      # Ensure the numeric x is present for all summary-based layers.
      age_interval_order = as.integer(age_interval)
    )

  age_labels <- levels(df_summary_plot$age_interval)
  n_str <- paste0(df_summary_plot$age_interval, ":", df_summary_plot$n, collapse = ", ")

  ggplot(df_long, aes(x = age_interval_order, y = burden, group = respondent_id)) +
    geom_line(color = col_mid_blue, linewidth = 0.6, alpha = 0.10) +
    geom_linerange(
      data = df_summary_plot,
      aes(x = age_interval_order, ymin = q25, ymax = q75),
      inherit.aes = FALSE,
      color = col_light_blue,
      linewidth = 3,
      alpha = 0.25
    ) +
    geom_line(
      data = df_summary_plot,
      aes(x = age_interval_order, y = median, group = 1),
      inherit.aes = FALSE,
      color = col_dark_blue,
      linewidth = 1.2
    ) +
    geom_point(
      data = df_summary_plot,
      aes(x = age_interval_order, y = median),
      inherit.aes = FALSE,
      color = col_orange,
      size = 2.4
    ) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2.5)) +
    scale_x_continuous(
      breaks = seq_along(age_labels),
      labels = age_labels
    ) +
    labs(
      title = "Miten yksil\u00f6lliset arviot vaihtelevat ik\u00e4jaksittain",
      subtitle = "Ohut viiva = yksil\u00f6n arvioidut jaksot; paksu viiva = mediaani",
      x = "Ik\u00e4jakso",
      y = "Kuormitus (0\u201310)",
      caption = paste0(
        "Ei-puuttuvien vastausten m\u00e4\u00e4r\u00e4 ik\u00e4jaksossa: ", n_str,
        ". My\u00f6hemm\u00e4t ik\u00e4jaksot voivat olla aidosti puuttuvia."
      )
    ) +
    theme_linkedin() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

plot_synnytitko_tarkasteltavan_lapsen_v2 <- function(df_clean) {
  categories <- c("K\u0079ll\u00e4", "Ei")

  df_counts <- df_clean |>
    mutate(
      synnytitko_label = dplyr::case_when(
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^Kyll", ignore_case = TRUE)) ~ categories[1],
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^(Ei|En)$", ignore_case = TRUE)) ~ "Ei",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(synnytitko_label)) |>
    count(synnytitko_label, name = "n") |>
    mutate(synnytitko_label = factor(synnytitko_label, levels = categories)) |>
    tidyr::complete(synnytitko_label, fill = list(n = 0)) |>
    mutate(
      total_n = sum(n),
      pct = ifelse(total_n > 0, (n / total_n) * 100, 0)
    )

  subtitle_n <- unique(df_counts$total_n)[1]
  max_n <- max(df_counts$n, na.rm = TRUE)

  ggplot(df_counts, aes(x = synnytitko_label, y = n, fill = synnytitko_label)) +
    geom_col(width = 0.62, color = "grey85") +
    geom_text(
      aes(label = paste0(n, " (", sprintf("%.0f", pct), "%)")),
      vjust = -0.25,
      size = 4,
      color = col_dark_text
    ) +
    scale_fill_manual(values = setNames(c(col_orange, col_mid_blue), categories[1:2])) +
    coord_cartesian(ylim = c(0, max_n * 1.25 + 0.5)) +
    labs(
      title = "Synnyttik\u00f6 vastaaja tarkasteltavan lapsen?",
      subtitle = paste0("Taustamuuttuja, n = ", subtitle_n),
      x = NULL,
      y = NULL
    ) +
    theme_linkedin() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11)
    )
}

plot_mita_lasta_tarkasteltavan_lapsen <- function(df_clean) {
  categories <- c(
    "Ensimm\u00e4ist\u00e4 lastani",
    "Toista tai my\u00f6hemp\u00e4\u00e4 lastani",
    "Lapsiani yleisesti"
  )

  df_counts <- df_clean |>
    mutate(
      mita_lasta_label = dplyr::case_when(
        is.na(mita_lasta) ~ NA_character_,
        stringr::str_detect(as.character(mita_lasta), stringr::regex("^Ensimm", ignore_case = TRUE)) ~ categories[1],
        stringr::str_detect(as.character(mita_lasta), stringr::regex("^Toista", ignore_case = TRUE)) ~ categories[2],
        stringr::str_detect(as.character(mita_lasta), stringr::regex("Lapsiani", ignore_case = TRUE)) ~ categories[3],
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(mita_lasta_label)) |>
    count(mita_lasta_label, name = "n") |>
    mutate(mita_lasta_label = factor(mita_lasta_label, levels = categories)) |>
    tidyr::complete(mita_lasta_label, fill = list(n = 0)) |>
    mutate(
      total_n = sum(n),
      pct = ifelse(total_n > 0, (n / total_n) * 100, 0)
    )

  subtitle_n <- unique(df_counts$total_n)[1]
  max_n <- max(df_counts$n, na.rm = TRUE)

  ggplot(df_counts, aes(x = mita_lasta_label, y = n, fill = mita_lasta_label)) +
    geom_col(width = 0.62, color = "grey85") +
    geom_text(
      aes(label = paste0(n, " (", sprintf("%.0f", pct), "%)")),
      vjust = -0.25,
      size = 4,
      color = col_dark_text
    ) +
    scale_fill_manual(values = c(col_orange, col_mid_blue, col_steel)) +
    coord_cartesian(ylim = c(0, max_n * 1.25 + 0.5)) +
    labs(
      title = "Mitä lasta tämä vastaus koskee?",
      subtitle = paste0("Taustamuuttuja, n = ", subtitle_n),
      x = NULL,
      y = NULL
    ) +
    theme_linkedin() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11)
    )
}

plot_syntyiko_lapselle_sisarus_ennen_3v <- function(df_clean) {
  categories <- c("K\u0079ll\u00e4", "Ei")

  df_counts <- df_clean |>
    mutate(
      sisarukset_label = dplyr::case_when(
        stringr::str_detect(as.character(sisarukset_ennen_3v), stringr::regex("^Kyll", ignore_case = TRUE)) ~ categories[1],
        stringr::str_detect(as.character(sisarukset_ennen_3v), stringr::regex("^(Ei|En)$", ignore_case = TRUE)) ~ "Ei",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(sisarukset_label)) |>
    count(sisarukset_label, name = "n") |>
    mutate(sisarukset_label = factor(sisarukset_label, levels = categories)) |>
    tidyr::complete(sisarukset_label, fill = list(n = 0)) |>
    mutate(
      total_n = sum(n),
      pct = ifelse(total_n > 0, (n / total_n) * 100, 0)
    )

  subtitle_n <- unique(df_counts$total_n)[1]
  max_n <- max(df_counts$n, na.rm = TRUE)

  ggplot(df_counts, aes(x = sisarukset_label, y = n, fill = sisarukset_label)) +
    geom_col(width = 0.62, color = "grey85") +
    geom_text(
      aes(label = paste0(n, " (", sprintf("%.0f", pct), "%)")),
      vjust = -0.25,
      size = 4,
      color = col_dark_text
    ) +
    scale_fill_manual(values = setNames(c(col_orange, col_mid_blue), categories[1:2])) +
    coord_cartesian(ylim = c(0, max_n * 1.25 + 0.5)) +
    labs(
      title = "Syntyik\u00f6 lapselle sisarus ennen 3 vuoden ik\u00e4\u00e4?",
      subtitle = paste0("Taustamuuttuja, n = ", subtitle_n),
      x = NULL,
      y = NULL
    ) +
    theme_linkedin() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11)
    )
}
