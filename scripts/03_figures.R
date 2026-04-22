# Plotting functions for `scripts/run_pipeline.R`
# Premium LinkedIn visual style focused on medians, IQR ribbons, and clear messages.

# 1. PÃ„Ã„KUVA: Kuormitustrendi (Mediaani + IQR-nauha)
plot_burden_responses_mean_ci_premium <- function(df_long) {
  age_labels_ordered <- c(
    "0\u20133 vko", "3 vko\u20133 kk", "3\u20136 kk", "6\u201312 kk",
    "12\u201318 kk", "18\u201324 kk", "24\u201330 kk", "30\u201336 kk"
  )
  
  df_stats <- df_long |>
    filter(!is.na(burden)) |>
    group_by(age_interval_order) |>
    summarise(
      mean   = mean(burden),
      median = median(burden),
      q25    = quantile(burden, 0.25),
      q75    = quantile(burden, 0.75),
      n      = n(),
      .groups = "drop"
    )
  
  # Haetaan dynaamisesti huippukohta annotaatiota varten
  peak <- df_stats |> slice_max(mean, n = 1, with_ties = FALSE)
  
  ggplot(df_stats, aes(x = age_interval_order)) +
    # IQR Nauha (50% vastauksista)
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = col_accent_soft, alpha = 0.55) +
    # Keskiarvo on pÃ¤Ã¤viiva (yhtenÃ¤inen).
    geom_line(aes(y = mean), color = col_accent, linewidth = 2.2) +
    # Mediaani paksuna pÃ¤Ã¤viivana
    geom_line(aes(y = median), color = col_neutral, linewidth = 1.6, linetype = "dotted") +
    geom_point(aes(y = mean), shape = 21, size = 3.8, fill = col_bg, color = col_accent, stroke = 1.7) +
    # Annotaatio huippukohtaan
    annotate("segment", x = peak$age_interval_order, xend = peak$age_interval_order, 
             y = peak$mean + 0.6, yend = 9.3, color = col_rule, linewidth = 0.4) +
    annotate("text", x = peak$age_interval_order, y = 9.6, 
             label = sprintf("Kuormitushuippu\n(Keskiarvo %.1f)", peak$mean),
             hjust = 0.5, vjust = 0, size = 3.8, color = col_ink, fontface = "bold", lineheight = 1.1) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_continuous(breaks = seq_along(age_labels_ordered), labels = age_labels_ordered) +
    labs(
      title    = "Kuormitus huipentuu vauvan ollessa 3\u20136 kuukautta",
      subtitle = "Keskiarvo (yhten\u00e4inen viiva) ja 50 %:n vastausv\u00e4li (vaalea nauha). Mediaani (katkoviiva).",
      x = NULL, y = "Kuormitus (0\u201310)",
      caption = sprintf("Kyselydata, n = %d vastaajaa.", max(df_stats$n))
    ) +
    theme_linkedin()
}

# 2. SIVUKUVA 1: Jakauma kuplamatriisina (Korvaa heatmapin)
plot_burden_heatmap_mean_median <- function(df_long) {
  age_labels_ordered <- c(
    "0\u20133 vko", "3 vko\u20133 kk", "3\u20136 kk", "6\u201312 kk",
    "12\u201318 kk", "18\u201324 kk", "24\u201330 kk", "30\u201336 kk"
  )
  
  df_plot <- df_long |>
    filter(!is.na(burden), burden >= 0, burden <= 10) |>
    mutate(burden_score = as.integer(round(burden)))
  
  df_bubbles <- df_plot |>
    count(age_interval_order, burden_score, name = "count") |>
    group_by(age_interval_order) |>
    mutate(share = count / sum(count)) |>
    ungroup() |>
    filter(count > 0)
  
  df_median <- df_plot |>
    group_by(age_interval_order) |>
    summarise(median = median(burden), .groups = "drop")
  
  df_mean <- df_plot |>
    group_by(age_interval_order) |>
    summarise(mean = mean(burden), .groups = "drop")
  
  df_n <- df_plot |> count(age_interval_order, name = "n")
  
  ggplot() +
    geom_point(data = df_bubbles, aes(x = age_interval_order, y = burden_score, size = share, color = share)) +
    geom_line(data = df_mean, aes(x = age_interval_order, y = mean), color = col_accent, linewidth = 1.8) +
    geom_point(data = df_mean, aes(x = age_interval_order, y = mean), shape = 21, size = 3.2, fill = col_bg, color = col_accent, stroke = 1.4) +
    # N-luvut siististi ylÃ¶s
    # Mediaani toissijaisena vertailuna (katkoviiva).
    geom_line(data = df_median, aes(x = age_interval_order, y = median), color = col_neutral, linewidth = 1.3, linetype = "dotted") +
    geom_point(data = df_median, aes(x = age_interval_order, y = median), shape = 21, size = 2.6, fill = col_bg, color = col_neutral, stroke = 1.1, alpha = 0.95) +
    geom_text(data = df_n, aes(x = age_interval_order, y = 10.8, label = paste0("n=", n)), size = 3.2, color = col_muted) +
    scale_size_continuous(range = c(2, 12), guide = "none") +
    scale_color_gradient(low = col_scale_lo, high = col_scale_hi, guide = "none") +
    scale_y_continuous(limits = c(0, 11), breaks = seq(0, 10, by = 2), expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_continuous(breaks = seq_along(age_labels_ordered), labels = age_labels_ordered) +
    coord_cartesian(clip = "off") +
    labs(
      title    = "Vastaukset hajautuvat laajasti \u2013 painopiste 4\u20138 v\u00e4liss\u00e4",
      subtitle = "Kuplan koko = vastausten osuus (%-osuus ryhm\u00e4st\u00e4). Keskiarvo (yhten\u00e4inen) ja mediaani (katkoviiva).",
      x = NULL, y = "Kuormitus (0\u201310)"
    ) +
    theme_linkedin()
}

# 3. SIVUKUVA 2: Synnyttänyt vs. ei-synnyttänyt (Samaan kuvaan)
plot_burden_responses_mean_ci_by_synnytitko <- function(df_long) {
  age_labels_ordered <- c(
    "0\u20133 vko", "3 vko\u20133 kk", "3\u20136 kk", "6\u201312 kk",
    "12\u201318 kk", "18\u201324 kk", "24\u201330 kk", "30\u201336 kk"
  )
  
  birth_labels <- c("Äidit (synnyttäneet)", "isät (ei synnyttäneet)")
  
  df_clean <- df_long |>
    mutate(
      ryhma = dplyr::case_when(
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^Kyll", ignore_case = TRUE)) ~ birth_labels[1],
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^(Ei|En)$", ignore_case = TRUE)) ~ birth_labels[2],
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(ryhma), !is.na(burden))
  
  df_stats <- df_clean |>
    group_by(ryhma, age_interval_order) |>
    summarise(
      mean = mean(burden),
      median = median(burden),
      q25 = quantile(burden, 0.25),
      q75 = quantile(burden, 0.75),
      .groups = "drop"
    )
  
  ggplot(df_stats, aes(x = age_interval_order, color = ryhma, fill = ryhma)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25, color = NA) +
    # Keskiarvo pÃ¤Ã¤viiva (yhtenÃ¤inen).
    geom_line(aes(y = mean), linewidth = 2.0) +
    geom_point(aes(y = mean), shape = 21, size = 3.5, fill = col_bg, stroke = 1.6) +
    # Mediaani toissijaisena vertailuna (katkoviiva).
    geom_line(aes(y = median), linewidth = 1.4, linetype = "dotted") +
    scale_color_manual(values = setNames(c(col_accent, col_neutral), birth_labels)) +
    scale_fill_manual(values = setNames(c(col_accent_soft, col_neutral_soft), birth_labels)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_continuous(breaks = seq_along(age_labels_ordered), labels = age_labels_ordered) +
    labs(
      title    = "Synnytt\u00e4neiden lis\u00e4kuorma tuntuu alussa",
      subtitle = "Keskiarvo (yhten\u00e4inen viiva) ja 50 %:n vastausv\u00e4li (nauha). Mediaani (katkoviiva).",
      x = NULL, y = "Kuormitus (0\u201310)",
      caption = 'Ryhmittely perustuu kysymykseen: "Synnytitkö tarkasteltavan lapsen/lapset?"'
    ) +
    theme_linkedin()
}

# 4. SIVUKUVA 3: Rauhoitettu spagettikuva
plot_burden_variation_spaghetti <- function(df_long, df_summary) {
  df_summary_plot <- df_summary %>%
    mutate(age_interval_order = as.integer(age_interval))
  
  age_labels <- levels(df_summary_plot$age_interval)
  n_str <- paste0(df_summary_plot$age_interval, ":", df_summary_plot$n, collapse = ", ")
  
  ggplot(df_long, aes(x = age_interval_order, y = burden, group = respondent_id)) +
    # Himmennetty yksilÃ¶iden taustakohina
    geom_line(color = col_dim, linewidth = 0.4, alpha = 0.15) +
    # Paksu laatikko IQR:lle
    geom_linerange(data = df_summary_plot, aes(x = age_interval_order, ymin = q25, ymax = q75),
                   inherit.aes = FALSE, color = col_accent_soft, linewidth = 7, alpha = 0.8) +
    # SelkeÃ¤ tumma mediaani
    geom_line(data = df_summary_plot, aes(x = age_interval_order, y = mean, group = 1),
              inherit.aes = FALSE, color = col_accent, linewidth = 2.1) +
    geom_point(data = df_summary_plot, aes(x = age_interval_order, y = mean),
               inherit.aes = FALSE, fill = col_bg, color = col_accent, shape = 21, size = 3.3, stroke = 1.6) +
    # Mediaani toissijaisena referenssinÃ¤ (katkoviiva).
    geom_line(data = df_summary_plot, aes(x = age_interval_order, y = median, group = 1),
              inherit.aes = FALSE, color = col_neutral, linewidth = 1.5, linetype = "dotted") +
    geom_point(data = df_summary_plot, aes(x = age_interval_order, y = median),
               inherit.aes = FALSE, fill = col_bg, color = col_neutral, shape = 21, size = 2.8, stroke = 1.3, alpha = 0.95) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
    scale_x_continuous(breaks = seq_along(age_labels), labels = age_labels) +
    labs(
      title = "Yksil\u00f6llinen hajonta on eritt\u00e4in suurta",
      subtitle = "Ohut viiva = yksitt\u00e4inen vastaaja; paksu yhten\u00e4inen viiva = keskiarvo; katkoviiva = mediaani; laatikko = 50 % vastauksista",
      x = NULL, y = "Kuormitus (0\u201310)",
      caption = paste0("Ei-puuttuvien m\u00e4\u00e4r\u00e4: ", n_str)
    ) +
    theme_linkedin() +
    theme(legend.position = "none")
}

# 5. TAUSTAMUUTTUJAT: Palkkikuvat (Ohuemmat palkit, vain prosentit, highlight)

plot_synnytitko_tarkasteltavan_lapsen_v2 <- function(df_clean) {
  categories <- c("Äidit (synnyttäneet)", "isät (ei synnyttäneet)")
  
  df_counts <- df_clean |>
    mutate(
      synnytitko_label = dplyr::case_when(
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^Kyll", ignore_case = TRUE)) ~ categories[1],
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^(Ei|En)$", ignore_case = TRUE)) ~ categories[2],
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
      title = "Synnyttik\u00f6 vastaaja tarkasteltavan lapsen?",
      subtitle = paste0("Taustamuuttuja, n = ", unique(df_counts$total_n)[1]),
      x = NULL, y = NULL,
      caption = 'Ryhmittely perustuu kysymykseen: "Synnytitkö tarkasteltavan lapsen/lapset?"'
    ) +
    theme_linkedin() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank())
}

plot_mita_lasta_tarkasteltavan_lapsen <- function(df_clean) {
  categories <- c("Ensimm\u00e4ist\u00e4 lastani", "Toista tai my\u00f6hemp\u00e4\u00e4", "Lapsiani yleisesti")
  
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
      title = "Mit\u00e4 lasta t\u00e4m\u00e4 vastaus koskee?",
      subtitle = paste0("Taustamuuttuja, n = ", unique(df_counts$total_n)[1]),
      x = NULL, y = NULL
    ) +
    theme_linkedin() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank())
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
      title = "Syntyik\u00f6 lapselle sisarus alle 3-vuotiaana?",
      subtitle = paste0("Taustamuuttuja, n = ", unique(df_counts$total_n)[1]),
      x = NULL, y = NULL
    ) +
    theme_linkedin() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank())
}

# 6. TAUSTAPROFIILI: Yhdistetty taustakuva (some-jako)
plot_taustaprofiili_yhdistetty <- function(df_clean, include_question_caption = TRUE) {
  specs <- tribble(
    ~muuttuja,              ~kysymys,
    "synnytitko_lapsi",     "Synnytitk\u00f6 tarkasteltavan lapsen/lapset?",
    "mita_lasta",           "Mit\u00e4 lasta t\u00e4m\u00e4 vastaus koskee?",
    "sisarukset_ennen_3v",  "Syntyik\u00f6 sisaruksia ennen 3 vuoden ik\u00e4\u00e4?"
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

  base_theme <- if (exists("theme_linkedin", mode = "function", inherits = TRUE)) {
    theme_linkedin()
  } else {
    theme_minimal(base_size = 12)
  }

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
        "Vastaajien kokonaismäärä n = ", n_total
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

  if (exists("save_plot_png", mode = "function", inherits = TRUE)) {
    save_plot_png(p, path = path, width = 8, height = 10, dpi = 320)
  } else {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    ggsave(path, p, width = 8, height = 10, dpi = 320, bg = "#FAFAF7")
  }

  invisible(path)
}
