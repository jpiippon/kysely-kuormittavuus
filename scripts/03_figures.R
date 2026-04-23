# Plotting functions sourced by scripts/run_pipeline.R

age_labels_ordered <- c(
  "0\u20133 vko", "3 vko\u20133 kk", "3\u20136 kk", "6\u201312 kk",
  "12\u201318 kk", "18\u201324 kk", "24\u201330 kk", "30\u201336 kk"
)

plot_burden_responses_mean_ci_premium <- function(df_long) {
  df_stats <- df_long |>
    dplyr::filter(!is.na(burden), burden >= 0, burden <= 10) |>
    dplyr::group_by(age_interval_order) |>
    dplyr::summarise(
      mean = mean(burden),
      q05 = stats::quantile(burden, 0.05, names = FALSE),
      q95 = stats::quantile(burden, 0.95, names = FALSE),
      n = dplyr::n(),
      .groups = "drop"
    )

  peak <- df_stats |> dplyr::slice_max(mean, n = 1, with_ties = FALSE)

  df_labels <- df_stats |>
    dplyr::mutate(
      mean_label = chartr(".", ",", sprintf("%.1f", mean)),
      n_label = sprintf("%s\n(n = %d)", age_labels_ordered, n)
    )

  ggplot2::ggplot(df_stats, ggplot2::aes(x = age_interval_order, y = mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q05, ymax = q95), fill = col_accent_soft, alpha = 0.55) +
    ggplot2::geom_line(color = col_accent, linewidth = 2.2) +
    ggplot2::geom_point(shape = 21, size = 3.6, fill = col_bg, color = col_accent, stroke = 1.5) +
    ggplot2::geom_text(data = df_labels, ggplot2::aes(y = pmin(mean + 0.35, 9.7), label = mean_label), size = 3.4, fontface = "bold", color = col_ink) +
    ggplot2::annotate(
      "segment",
      x = peak$age_interval_order, xend = peak$age_interval_order,
      y = peak$mean + 0.6, yend = 9.3,
      color = col_rule, linewidth = 1.0
    ) +
    ggplot2::annotate(
      "text",
      x = peak$age_interval_order, y = 9.6,
      label = "Kuormitushuippu",
      hjust = 0.5, vjust = 0, size = 3.8, color = col_ink, fontface = "bold", lineheight = 1.1
    ) +
    ggplot2::scale_x_continuous(breaks = seq_along(age_labels_ordered), labels = df_labels$n_label) +
    ggplot2::scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
    ggplot2::labs(
      title = "Kuormitus huipentuu vauvan ollessa 3\u20136 kuukautta",
      subtitle = "Keskiarvo (viiva) ja vaihteluv\u00E4li, johon 90 % vastauksista sijoittuu (punertava varjostus)",
      x = "Lapsen ik\u00E4",
      y = "Kuormitus (0\u201310)",
      caption = "Vastaa tutkimuskysymykseen: \"Kuinka kuormittavaksi koit arjen kussakin vaiheessa?\", johon vastattiin asteikolla 0\u201310."
    ) +
    theme_linkedin() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(lineheight = 0.95))
}

plot_burden_heatmap_mean_median <- function(df_long) {
  df_plot <- df_long |>
    dplyr::filter(!is.na(burden), burden >= 0, burden <= 10) |>
    dplyr::mutate(burden_score = pmax(pmin(round(burden), 10), 0))

  df_n <- df_plot |>
    dplyr::group_by(age_interval_order) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  df_stats <- df_plot |>
    dplyr::group_by(age_interval_order) |>
    dplyr::summarise(mean = mean(burden), median = stats::median(burden), .groups = "drop")

  df_dist <- df_plot |>
    dplyr::count(age_interval_order, burden_score, name = "count") |>
    dplyr::right_join(
      tidyr::expand_grid(age_interval_order = seq_along(age_labels_ordered), burden_score = 0:10),
      by = c("age_interval_order", "burden_score")
    ) |>
    dplyr::left_join(df_n, by = "age_interval_order") |>
    dplyr::mutate(
      count = dplyr::coalesce(count, 0L),
      n = dplyr::coalesce(n, 0L),
      share = dplyr::if_else(n > 0, count / n, 0)
    ) |>
    dplyr::filter(n > 0)

  ggplot2::ggplot(df_dist, ggplot2::aes(x = age_interval_order, y = burden_score, fill = share)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.45, width = 0.95, height = 0.95) +
    ggplot2::geom_line(data = df_stats, ggplot2::aes(x = age_interval_order, y = mean), color = col_accent, linewidth = 2.3, inherit.aes = FALSE) +
    ggplot2::geom_line(data = df_stats, ggplot2::aes(x = age_interval_order, y = median), color = col_neutral, linewidth = 1.8, linetype = "dashed", inherit.aes = FALSE) +
    ggplot2::geom_text(data = df_n, ggplot2::aes(x = age_interval_order, y = 10.65, label = sprintf("n = %d", n)), inherit.aes = FALSE, size = 3.0, color = col_text) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(age_labels_ordered),
      labels = age_labels_ordered,
      guide = ggplot2::guide_axis(n.dodge = 2)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 10.8), breaks = seq(0, 10, by = 2)) +
    ggplot2::scale_fill_gradientn(
      colours = c("#6BB5F2", "#3F90D6", "#1F79D1", "#0E5FAF", "#083B6B"),
      limits = c(0, 0.30), breaks = c(0, 0.10, 0.20, 0.30), labels = c("0 %", "10 %", "20 %", "30 %"),
      oob = scales::squish, name = "Osuus"
    ) +
    ggplot2::labs(
      title = "Kuormituskokemus vaihtelee i\u00E4n my\u00F6t\u00E4",
      subtitle = "Tummempi ruutu = suurempi osuus vastauksista kyseisell\u00E4 kuormitusarvolla. Oranssi viiva = keskiarvo, katkoviiva = mediaani.",
      x = "Ik\u00E4v\u00E4li",
      y = "Kuormitus (0\u201310)"
    ) +
    theme_linkedin() +
    ggplot2::theme(legend.position = "top")
}

plot_burden_responses_mean_ci_by_synnytitko <- function(df_long) {
  labels <- c("\u00C4idit (synnytt\u00E4neet)", "Is\u00E4t (ei synnytt\u00E4neet)")

  df_group <- df_long |>
    dplyr::mutate(
      ryhma = dplyr::case_when(
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^Kyll", ignore_case = TRUE)) ~ labels[1],
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^(Ei|En)$", ignore_case = TRUE)) ~ labels[2],
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(ryhma), !is.na(burden))

  df_stats <- df_group |>
    dplyr::group_by(ryhma, age_interval_order) |>
    dplyr::summarise(mean = mean(burden), .groups = "drop")

  df_labels <- df_stats |>
    dplyr::mutate(label = chartr(".", ",", sprintf("%.1f", mean)))

  ggplot2::ggplot(df_stats, ggplot2::aes(x = age_interval_order, color = ryhma, fill = ryhma)) +
    ggplot2::geom_line(ggplot2::aes(y = mean), linewidth = 2.0) +
    ggplot2::geom_point(ggplot2::aes(y = mean), shape = 21, size = 3.4, fill = col_bg, stroke = 1.4) +
    ggplot2::geom_text(
      data = df_labels,
      ggplot2::aes(y = pmin(mean + 0.28, 9.6), label = label),
      size = 3.0,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = stats::setNames(c(col_accent, col_neutral), labels)) +
    ggplot2::scale_fill_manual(values = stats::setNames(c(col_accent_soft, col_neutral_soft), labels)) +
    ggplot2::scale_x_continuous(breaks = seq_along(age_labels_ordered), labels = age_labels_ordered) +
    ggplot2::scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
    ggplot2::labs(
      title = "Synnytt\u00E4neiden lis\u00E4kuorma tuntuu eniten alkuvaiheessa",
      subtitle = "Keskiarvoviivat (0\u201310) ryhmitt\u00E4in.",
      x = "Lapsen ik\u00E4",
      y = "Kuormitus (0\u201310)",
      caption = "Ryhmittely perustuu kysymykseen: Synnytitk\u00F6 tarkasteltavan lapsen/lapset?"
    ) +
    theme_linkedin()
}

plot_burden_mean_by_mita_lasta <- function(df_long) {
  labels <- c("Ensimm\u00E4inen lapsi", "Toinen tai my\u00F6hempi lapsi", "Lapset yleisesti")

  df_group <- df_long |>
    dplyr::mutate(
      ryhma = dplyr::case_when(
        stringr::str_detect(as.character(mita_lasta), stringr::regex("^Ensimm", ignore_case = TRUE)) ~ labels[1],
        stringr::str_detect(as.character(mita_lasta), stringr::regex("^Toista", ignore_case = TRUE)) ~ labels[2],
        stringr::str_detect(as.character(mita_lasta), stringr::regex("Lapsiani", ignore_case = TRUE)) ~ labels[3],
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(ryhma), !is.na(burden))

  df_stats <- df_group |>
    dplyr::group_by(ryhma, age_interval_order) |>
    dplyr::summarise(mean = mean(burden), .groups = "drop")

  df_labels <- df_stats |>
    dplyr::mutate(label = chartr(".", ",", sprintf("%.1f", mean)))

  pal <- stats::setNames(c(col_accent, col_neutral, col_dim), labels)

  ggplot2::ggplot(df_stats, ggplot2::aes(x = age_interval_order, y = mean, color = ryhma, group = ryhma)) +
    ggplot2::geom_line(linewidth = 2.0) +
    ggplot2::geom_point(shape = 21, size = 3.4, fill = col_bg, stroke = 1.4) +
    ggplot2::geom_text(
      data = df_labels,
      ggplot2::aes(y = pmin(mean + 0.28, 9.6), label = label),
      size = 2.9,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::scale_x_continuous(breaks = seq_along(age_labels_ordered), labels = age_labels_ordered) +
    ggplot2::scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
    ggplot2::labs(
      title = "Kuormituksen taso riippuu lapsij\u00E4rjestyksest\u00E4",
      subtitle = "Keskiarvoviivat (0\u201310) sen mukaan, mit\u00E4 lasta vastaus koskee.",
      x = "Lapsen ik\u00E4",
      y = "Kuormitus (0\u201310)"
    ) +
    theme_linkedin()
}

plot_burden_histogram_faceted <- function(df_long) {
  df_plot <- df_long |>
    dplyr::filter(!is.na(burden), burden >= 0, burden <= 10) |>
    dplyr::mutate(age_interval = factor(age_interval, levels = age_labels_ordered))

  ggplot2::ggplot(df_plot, ggplot2::aes(x = burden)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = -0.5, fill = col_neutral, color = "white", alpha = 0.95) +
    ggplot2::facet_wrap(~ age_interval, ncol = 4) +
    ggplot2::scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
    ggplot2::labs(
      title = "Vastausjakauma ik\u00E4jaksoittain",
      subtitle = "Histogrammit n\u00E4ytt\u00E4v\u00E4t, miten kuormitusvastaukset (0\u201310) jakautuvat jokaisessa ik\u00E4vaiheessa.",
      x = "Kuormitus (0\u201310)",
      y = "Vastausten lukum\u00E4\u00E4r\u00E4"
    ) +
    theme_linkedin() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
}

plot_taustaprofiili_yhdistetty <- function(df_clean, include_question_caption = TRUE) {
  specs <- tibble::tribble(
    ~muuttuja, ~kysymys,
    "synnytitko_lapsi", "Synnyttik\u00F6 vastaaja tarkasteltavan lapsen?",
    "mita_lasta", "Mit\u00E4 lasta t\u00E4m\u00E4 vastaus koskee?",
    "sisarukset_ennen_3v", "Syntyik\u00F6 sisaruksia ennen 3 vuoden ik\u00E4\u00E4?"
  )

  n_total <- nrow(df_clean)

  summary_df <- df_clean |>
    dplyr::select(dplyr::all_of(specs$muuttuja)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "muuttuja", values_to = "arvo") |>
    dplyr::mutate(arvo = dplyr::if_else(is.na(arvo) | !nzchar(as.character(arvo)), "Ei vastausta", as.character(arvo))) |>
    dplyr::left_join(specs, by = "muuttuja") |>
    dplyr::count(muuttuja, kysymys, arvo, name = "n") |>
    dplyr::group_by(muuttuja, kysymys) |>
    dplyr::mutate(
      pct = n / sum(n),
      label = sprintf("%.0f %% (n = %d)", pct * 100, n),
      highlight = n == max(n),
      arvo = forcats::fct_reorder(arvo, pct)
    ) |>
    dplyr::ungroup()

  ggplot2::ggplot(summary_df, ggplot2::aes(x = pct, y = arvo)) +
    ggplot2::geom_col(ggplot2::aes(fill = highlight), width = 0.62) +
    ggplot2::geom_text(ggplot2::aes(label = label), hjust = -0.05, size = 3.4, color = col_ink) +
    ggplot2::facet_wrap(~ kysymys, scales = "free_y", ncol = 1) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = col_accent, `FALSE` = col_neutral_soft), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.14))) +
    ggplot2::labs(
      title = "Vastaajajoukon taustaprofiili",
      subtitle = paste0("Vastaajien kokonaism\u00E4\u00E4r\u00E4 n = ", n_total, "."),
      x = NULL,
      y = NULL,
      caption = if (isTRUE(include_question_caption)) "Mukana my\u00F6s ei-vastaukset omana kategorianaan." else NULL
    ) +
    theme_linkedin() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    )
}

save_taustaprofiili_plot <- function(df_clean, path = file.path("output", "figures", "taustakysymykset.png")) {
  p <- plot_taustaprofiili_yhdistetty(df_clean, include_question_caption = TRUE)
  save_plot_png(p, path = path, width = 8, height = 10, dpi = 320)
  invisible(path)
}
