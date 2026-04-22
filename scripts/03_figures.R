# Plotting functions for `scripts/run_pipeline.R`
# Premium LinkedIn visual style focused on medians, IQR ribbons, and clear messages.

# 1. Pääkuva: Kuormitustrendi (mean + median reference)
plot_burden_responses_mean_ci_premium <- function(df_long) {
  age_labels_ordered <- c(
    "0\u20133 vko", "3 vko\u20133 kk", "3\u20136 kk", "6\u201312 kk",
    "12\u201318 kk", "18\u201324 kk", "24\u201330 kk", "30\u201336 kk"
  )

  df_stats <- df_long |>
    filter(!is.na(burden)) |>
    group_by(age_interval_order) |>
    summarise(
      mean = mean(burden),
      median = median(burden),
      q05 = quantile(burden, 0.05),
      q95 = quantile(burden, 0.95),
      n = n(),
      .groups = "drop"
    )

  # Haetaan dynaamisesti huippukohta annotaatiota varten
  peak <- df_stats |> slice_max(mean, n = 1, with_ties = FALSE)

  ggplot(df_stats, aes(x = age_interval_order)) +
    # 90 % vaihteluväli
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = col_accent_soft, alpha = 0.55) +
    # Mean ensisijainen signaali
    geom_line(aes(y = mean), color = col_accent, linewidth = 2.2) +
    # Median selkeänä referenssinä
    geom_line(aes(y = median), color = col_neutral, linewidth = 1.6, linetype = "dotted") +
    geom_point(aes(y = mean), shape = 21, size = 3.8, fill = col_bg, color = col_accent, stroke = 1.7) +
    annotate(
      "segment",
      x = peak$age_interval_order, xend = peak$age_interval_order,
      y = peak$mean + 0.6, yend = 9.3,
      color = col_rule, linewidth = 0.4
    ) +
    annotate(
      "text",
      x = peak$age_interval_order, y = 9.6,
      label = sprintf("Kuormitushuippu\n(Keskiarvo %.1f)", peak$mean),
      hjust = 0.5, vjust = 0, size = 3.8, color = col_ink, fontface = "bold", lineheight = 1.1
    ) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_continuous(breaks = seq_along(age_labels_ordered), labels = age_labels_ordered) +
    labs(
      title = "Kuormitus huipentuu vauvan ollessa 3\u20136 kuukautta",
      subtitle = "Keskiarvo (yhtenäinen viiva) ja 90 %:n vaihteluväli (vaalea nauha). Mediaani (katkoviiva).",
      x = NULL, y = "Kuormitus (0\u201310)",
      caption = sprintf("Kyselydata, n = %d vastaajaa.", max(df_stats$n))
    ) +
    theme_linkedin()
}

# 2. Pääkuva: kontrolloitu vaakalevitys + mean (primary) + median (dashed)
plot_burden_heatmap_mean_median <- function(df_long) {
  age_labels_ordered <- c(
    "0\u20133 vko", "3 vko\u20133 kk", "3\u20136 kk", "6\u201312 kk",
    "12\u201318 kk", "18\u201324 kk", "24\u201330 kk", "30\u201336 kk"
  )

  df_plot <- df_long |>
    filter(!is.na(burden), burden >= 0, burden <= 10)

  point_spread_bins <- 5
  point_spread <- 0.16 # aavistuksen enemmän kuin aiemmin, mutta hallitusti

  df_points <- df_plot |>
    group_by(age_interval_order) |>
    mutate(
      resp_idx = as.integer(factor(respondent_id)),
      bin = (resp_idx - 1) %% point_spread_bins + 1,
      x = age_interval_order + (bin - (point_spread_bins + 1) / 2) * point_spread
    ) |>
    ungroup()

  df_stats <- df_plot |>
    group_by(age_interval_order) |>
    summarise(
      mean = mean(burden),
      median = median(burden),
      .groups = "drop"
    )

  ggplot() +
    geom_point(
      data = df_points,
      aes(x = x, y = burden),
      color = col_dim,
      alpha = 0.18,
      size = 1.55
    ) +
    geom_line(
      data = df_stats,
      aes(x = age_interval_order, y = mean),
      color = col_accent,
      linewidth = 2.3
    ) +
    geom_point(
      data = df_stats,
      aes(x = age_interval_order, y = mean),
      shape = 21,
      size = 3.2,
      fill = col_bg,
      color = col_accent,
      stroke = 1.4
    ) +
    geom_line(
      data = df_stats,
      aes(x = age_interval_order, y = median),
      color = col_neutral,
      linewidth = 1.9,
      linetype = "dashed"
    ) +
    geom_point(
      data = df_stats,
      aes(x = age_interval_order, y = median),
      shape = 21,
      size = 2.7,
      fill = col_bg,
      color = col_neutral,
      stroke = 1.1,
      alpha = 0.95
    ) +
    scale_x_continuous(
      breaks = seq_along(age_labels_ordered),
      labels = age_labels_ordered,
      limits = c(0.6, length(age_labels_ordered) + 0.4),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      limits = c(0, 10),
      breaks = seq(0, 10, by = 2),
      expand = expansion(mult = c(0.02, 0.06))
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = "Kuormituskokemus vaihtelee iän myötä",
      subtitle = "Taustalla yksilölliset vastaukset (levitetty hallitusti). Keskiarvo (yhtenäinen) ja mediaani (katkoviiva).",
      x = "Ikäväli",
      y = "Kuormitus (0\u201310)"
    ) +
    theme_linkedin() +
    theme(legend.position = "none")
}

# 3. SIVUKUVA 2: Synnyttänyt vs. ei-synnyttänyt (mean + median)
plot_burden_responses_mean_ci_by_synnytitko <- function(df_long) {
  age_labels_ordered <- c(
    "0\u20133 vko", "3 vko\u20133 kk", "3\u20136 kk", "6\u201312 kk",
    "12\u201318 kk", "18\u201324 kk", "24\u201330 kk", "30\u201336 kk"
  )

  birth_labels <- c("Äidit (synnyttäneet)", "Isät (ei synnyttäneet)")

  df_clean <- df_long |>
    mutate(
      ryhma = case_when(
        str_detect(as.character(synnytitko_lapsi), regex("^Kyll", ignore_case = TRUE)) ~ birth_labels[1],
        str_detect(as.character(synnytitko_lapsi), regex("^(Ei|En)$", ignore_case = TRUE)) ~ birth_labels[2],
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(ryhma), !is.na(burden))

  df_stats <- df_clean |>
    group_by(ryhma, age_interval_order) |>
    summarise(
      mean = mean(burden),
      median = median(burden),
      q05 = quantile(burden, 0.05),
      q95 = quantile(burden, 0.95),
      .groups = "drop"
    )

  ggplot(df_stats, aes(x = age_interval_order, color = ryhma, fill = ryhma)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.25, color = NA) +
    geom_line(aes(y = mean), linewidth = 2.0) +
    geom_point(aes(y = mean), shape = 21, size = 3.5, fill = col_bg, stroke = 1.6) +
    scale_color_manual(values = setNames(c(col_accent, col_neutral), birth_labels)) +
    scale_fill_manual(values = setNames(c(col_accent_soft, col_neutral_soft), birth_labels)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_continuous(breaks = seq_along(age_labels_ordered), labels = age_labels_ordered) +
    labs(
      title = "Synnyttäneiden lisäkuorma tuntuu alussa",
      subtitle = "Keskiarvo (yhtenäinen viiva) ja 90 %:n vaihteluväli (nauha).",
      x = NULL, y = "Kuormitus (0\u201310)",
      caption = 'Ryhmittely perustuu kysymykseen: "Synnytitkö tarkasteltavan lapsen/lapset?"'
    ) +
    theme_linkedin()
}

# 4. Sivukuva 3: Rauhoitettu spagettikuva
plot_burden_variation_spaghetti <- function(df_long, df_summary) {
  df_summary_plot <- df_summary |>
    mutate(age_interval_order = as.integer(age_interval))

  age_labels <- levels(df_summary_plot$age_interval)
  n_str <- paste0(df_summary_plot$age_interval, ":", df_summary_plot$n, collapse = ", ")

  ggplot(df_long, aes(x = age_interval_order, y = burden, group = respondent_id)) +
    geom_line(color = col_dim, linewidth = 0.4, alpha = 0.15) +
    geom_linerange(
      data = df_summary_plot,
      aes(x = age_interval_order, ymin = q05, ymax = q95),
      inherit.aes = FALSE,
      color = col_accent_soft,
      linewidth = 7,
      alpha = 0.8
    ) +
    geom_line(data = df_summary_plot, aes(x = age_interval_order, y = mean, group = 1),
      inherit.aes = FALSE, color = col_accent, linewidth = 2.1
    ) +
    geom_point(data = df_summary_plot, aes(x = age_interval_order, y = mean),
      inherit.aes = FALSE, fill = col_bg, color = col_accent, shape = 21, size = 3.3, stroke = 1.6
    ) +
    geom_line(data = df_summary_plot, aes(x = age_interval_order, y = median, group = 1),
      inherit.aes = FALSE, color = col_neutral, linewidth = 1.5, linetype = "dotted"
    ) +
    geom_point(data = df_summary_plot, aes(x = age_interval_order, y = median),
      inherit.aes = FALSE, fill = col_bg, color = col_neutral, shape = 21, size = 2.8, stroke = 1.3, alpha = 0.95
    ) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
    scale_x_continuous(breaks = seq_along(age_labels), labels = age_labels) +
    labs(
      title = "Yksilöllinen hajonta on erittäin suurta",
      subtitle = "Ohut viiva = yksittäinen vastaaja; paksu yhtenäinen viiva = keskiarvo; katkoviiva = mediaani; laatikko = 90 % vastauksista",
      x = NULL, y = "Kuormitus (0\u201310)",
      caption = paste0("Ei-puuttuvien määrää: ", n_str)
    ) +
    theme_linkedin() +
    theme(legend.position = "none")
}

# 5. Taustamuuttujat: Palkkikuvat (vain jos haluat myöhemmin ne erikseen)
plot_synnytitko_tarkasteltavan_lapsen_v2 <- function(df_clean) {
  categories <- c("Äidit (synnyttäneet)", "isät (ei synnyttäneet)")

  df_counts <- df_clean |>
    mutate(
      synnytitko_label = case_when(
        str_detect(as.character(synnytitko_lapsi), regex("^Kyll", ignore_case = TRUE)) ~ categories[1],
        str_detect(as.character(synnytitko_lapsi), regex("^(Ei|En)$", ignore_case = TRUE)) ~ categories[2],
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(synnytitko_label)) |>
    count(synnytitko_label, name = "n") |>
    mutate(
      synnytitko_label = factor(synnytitko_label, levels = categories),
      total_n = sum(n),
      pct = ifelse(total_n > 0, (n / total_n) * 100, 0),
      highlight = n == max(n)
    )

  ggplot(df_counts, aes(x = synnytitko_label, y = n, fill = highlight)) +
    geom_col(width = 0.45) +
    geom_text(aes(label = sprintf("%.0f %%", pct)), vjust = -0.5, size = 4.5, color = col_ink, fontface = "bold") +
    scale_fill_manual(values = c(`TRUE` = col_accent, `FALSE` = col_neutral_soft)) +
    coord_cartesian(ylim = c(0, max(df_counts$n) * 1.2)) +
    labs(
      title = "Synnyttikö vastaaja tarkasteltavan lapsen?",
      subtitle = paste0("Taustamuuttuja, n = ", unique(df_counts$total_n)[1]),
      x = NULL, y = NULL,
      caption = 'Ryhmittely perustuu kysymykseen: "Synnytitkö tarkasteltavan lapsen/lapset?"'
    ) +
    theme_linkedin() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank())
}

plot_mita_lasta_tarkasteltavan_lapsen <- function(df_clean) {
  categories <- c("Ensimmäistä lastani", "Toista tai myöhempää", "Lapsiani yleisesti")

  df_counts <- df_clean |>
    mutate(
      mita_lasta_label = case_when(
        is.na(mita_lasta) ~ NA_character_,
        str_detect(as.character(mita_lasta), regex("^Ensimm", ignore_case = TRUE)) ~ categories[1],
        str_detect(as.character(mita_lasta), regex("^Toista", ignore_case = TRUE)) ~ categories[2],
        str_detect(as.character(mita_lasta), regex("Lapsiani", ignore_case = TRUE)) ~ categories[3],
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(mita_lasta_label)) |>
    count(mita_lasta_label, name = "n") |>
    mutate(
      mita_lasta_label = factor(mita_lasta_label, levels = categories),
      total_n = sum(n),
      pct = ifelse(total_n > 0, (n / total_n) * 100, 0),
      highlight = n == max(n)
    )

  ggplot(df_counts, aes(x = mita_lasta_label, y = n, fill = highlight)) +
    geom_col(width = 0.45) +
    geom_text(aes(label = sprintf("%.0f %%", pct)), vjust = -0.5, size = 4.5, color = col_ink, fontface = "bold") +
    scale_fill_manual(values = c(`TRUE` = col_accent, `FALSE` = col_neutral_soft)) +
    coord_cartesian(ylim = c(0, max(df_counts$n) * 1.2)) +
    labs(
      title = "Mitä lasta tämä vastaus koskee?",
      subtitle = paste0("Taustamuuttuja, n = ", unique(df_counts$total_n)[1]),
      x = NULL, y = NULL
    ) +
    theme_linkedin() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank())
}

plot_syntyiko_lapselle_sisarus_ennen_3v <- function(df_clean) {
  categories <- c("Kyllä", "Ei")

  df_counts <- df_clean |>
    mutate(
      sisarukset_label = case_when(
        str_detect(as.character(sisarukset_ennen_3v), regex("^Kyll", ignore_case = TRUE)) ~ categories[1],
        str_detect(as.character(sisarukset_ennen_3v), regex("^(Ei|En)$", ignore_case = TRUE)) ~ "Ei",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(sisarukset_label)) |>
    count(sisarukset_label, name = "n") |>
    mutate(
      sisarukset_label = factor(sisarukset_label, levels = categories),
      total_n = sum(n),
      pct = ifelse(total_n > 0, (n / total_n) * 100, 0),
      highlight = n == max(n)
    )

  ggplot(df_counts, aes(x = sisarukset_label, y = n, fill = highlight)) +
    geom_col(width = 0.45) +
    geom_text(aes(label = sprintf("%.0f %%", pct)), vjust = -0.5, size = 4.5, color = col_ink, fontface = "bold") +
    scale_fill_manual(values = c(`TRUE` = col_accent, `FALSE` = col_neutral_soft)) +
    coord_cartesian(ylim = c(0, max(df_counts$n) * 1.2)) +
    labs(
      title = "Syntyikö lapselle sisarus alle 3-vuotiaana?",
      subtitle = paste0("Taustamuuttuja, n = ", unique(df_counts$total_n)[1]),
      x = NULL, y = NULL
    ) +
    theme_linkedin() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank())
}

# 6. Taustaprofiili: yhdistelmäkuva (some-jako)
plot_taustaprofiili_yhdistetty <- function(df_clean, include_question_caption = TRUE) {
  specs <- tribble(
    ~muuttuja, ~kysymys,
    "synnytitko_lapsi", "Synnytit\u00f6 tarkasteltavan lapsen/lapset?",
    "mita_lasta", "Mit\u00e4 lasta t\u00e4m\u00e4 vastaus koskee?",
    "sisarukset_ennen_3v", "Syntyik\u00f6 sisaruksia ennen 3 vuoden ik\u00e4\u00e4?"
  )

  n_total <- nrow(df_clean)

  long <- df_clean |>
    select(all_of(specs$muuttuja)) |>
    pivot_longer(
      cols = everything(),
      names_to = "muuttuja",
      values_to = "arvo"
    ) |>
    mutate(
      arvo = if_else(is.na(arvo) | !nzchar(as.character(arvo)), "Ei vastausta", as.character(arvo))
    ) |>
    left_join(specs, by = "muuttuja")

  summary_df <- long |>
    group_by(muuttuja, kysymys, arvo) |>
    summarise(n = n(), .groups = "drop_last") |>
    mutate(
      n_total = sum(n),
      pct = n / n_total,
      label = sprintf("%.0f %% (n = %d)", pct * 100, n)
    ) |>
    ungroup() |>
    group_by(muuttuja, kysymys) |>
    mutate(arvo = fct_reorder(arvo, pct, .desc = FALSE)) |>
    ungroup()

  base_theme <- theme_linkedin()

  ggplot(summary_df, aes(x = pct, y = arvo)) +
    geom_col(fill = "#3D4A5C", width = 0.65) +
    geom_text(
      aes(label = label),
      hjust = -0.05,
      size = 3.6,
      color = "#1B1F24"
    ) +
    facet_wrap(~ kysymys, scales = "free_y", ncol = 1) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.13))
    ) +
    labs(
      title = "Vastaajajoukon taustaprofiili",
      subtitle = paste0(
        "Vastaajien kokonaismäärä n = ", n_total,
        "."
      ),
      x = "Osuus vastaajista",
      y = NULL,
      caption = NULL
    ) +
    base_theme +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", hjust = 0),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}

save_taustaprofiili_plot <- function(
  df_clean,
  path = file.path("output", "figures", "00_taustaprofiili_yhdistetty.png")
) {
  p <- plot_taustaprofiili_yhdistetty(df_clean, include_question_caption = TRUE)
  save_plot_png(p, path = path, width = 8, height = 10, dpi = 320)
  invisible(path)
}
