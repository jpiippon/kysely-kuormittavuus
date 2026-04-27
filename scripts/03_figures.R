# Plotting functions sourced by scripts/run_pipeline.R

age_labels_ordered <- c(
  "0\u20133 vko", "3 vko\u20133 kk", "3\u20136 kk", "6\u201312 kk",
  "12\u201318 kk", "18\u201324 kk", "24\u201330 kk", "30\u201336 kk"
)

plot_burden_responses_mean_ci_premium <- function(df_long, extra_subtitle = NULL) {
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
      subtitle = if (is.null(extra_subtitle)) {
        "Keskiarvo (viiva) ja vaihteluv\u00E4li, johon 90 % vastauksista sijoittuu (punertava varjostus)"
      } else {
        paste0(
          "Keskiarvo (viiva) ja vaihteluv\u00E4li, johon 90 % vastauksista sijoittuu (punertava varjostus)\n",
          extra_subtitle
        )
      },
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
      guide = ggplot2::guide_axis(angle = 45)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 10.8), breaks = seq(0, 10, by = 2)) +
    ggplot2::scale_fill_gradientn(
      colours = c("#EAF2FB", "#D7E8F8", "#BCD8F1", "#8FBCE3", "#5F96C8", "#2E659D"),
      limits = c(0, 0.30), breaks = c(0, 0.10, 0.20, 0.30), labels = c("0 %", "10 %", "20 %", "30 %"),
      oob = scales::squish, name = "Osuus"
    ) +
    ggplot2::labs(
      title = "Sama ikävaihe voi tuntua vastaajasta riippuen\näärimmäisen raskaalta tai melko kevyeltä",
      subtitle = "Tummempi ruutu = suurempi osuus vastannut kyseisen kuormitustason\nOranssi viiva = keskiarvo, katkoviiva = mediaani.",
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

  ggplot2::ggplot(df_stats, ggplot2::aes(x = age_interval_order, color = ryhma, fill = ryhma)) +
    ggplot2::geom_line(ggplot2::aes(y = mean), linewidth = 2.0) +
    ggplot2::geom_point(ggplot2::aes(y = mean), shape = 21, size = 3.4, fill = col_bg, stroke = 1.4) +
    ggplot2::scale_color_manual(values = stats::setNames(c(col_accent, col_neutral), labels)) +
    ggplot2::scale_fill_manual(values = stats::setNames(c(col_accent_soft, col_neutral_soft), labels)) +
    ggplot2::scale_x_continuous(breaks = seq_along(age_labels_ordered), labels = age_labels_ordered) +
    ggplot2::scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
    ggplot2::labs(
      title = "Vanhempien kokemat erot alkuvaiheen kuormituksesta\nsaattavat yllättää",
      subtitle = "Keskiarvoviivat (0\u201310) ryhmitt\u00E4in.",
      x = "Lapsen ik\u00E4",
      y = "Kuormitus (0\u201310)",
      caption = "Ryhmittely perustuu kysymykseen: Synnytitk\u00F6 tarkasteltavan lapsen/lapset?"
    ) +
    theme_linkedin()
}

plot_burden_mean_by_sisarukset <- function(df_long) {
  sisarus_labels <- c("Sisarus syntyi", "Ei sisarusta")

  df_plot <- df_long |>
    dplyr::mutate(
      sisarus_ryhma = dplyr::case_when(
        stringr::str_detect(
          as.character(sisarukset_ennen_3v),
          stringr::regex("^Kyll", ignore_case = TRUE)
        ) ~ sisarus_labels[1],
        stringr::str_detect(
          as.character(sisarukset_ennen_3v),
          stringr::regex("^(Ei|En)$", ignore_case = TRUE)
        ) ~ sisarus_labels[2],
        TRUE ~ NA_character_
      ),
      sisarus_ryhma = factor(sisarus_ryhma, levels = sisarus_labels)
    ) |>
    dplyr::filter(
      !is.na(sisarus_ryhma),
      !is.na(burden),
      burden >= 0,
      burden <= 10
    )

  group_n <- df_plot |>
    dplyr::distinct(respondent_id, sisarus_ryhma) |>
    dplyr::count(sisarus_ryhma, name = "n_total")

  df_stats <- df_plot |>
    dplyr::group_by(sisarus_ryhma, age_interval_order) |>
    dplyr::summarise(
      mean = mean(burden),
      median = stats::median(burden),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::left_join(group_n, by = "sisarus_ryhma")

  df_end <- df_stats |>
    dplyr::group_by(sisarus_ryhma) |>
    dplyr::slice_max(age_interval_order, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::arrange(mean) |>
    dplyr::mutate(
      label_y = mean + dplyr::case_when(
        dplyr::row_number() == 1 ~ -0.10,
        TRUE ~ 0.10
      ),
      label = paste0(as.character(sisarus_ryhma), " (n = ", n_total, ")")
    )

  pal <- stats::setNames(
    c(col_accent, col_neutral),
    sisarus_labels
  )

  line_types <- stats::setNames(
    c("solid", "22"),
    sisarus_labels
  )

  ggplot2::ggplot(
    df_stats,
    ggplot2::aes(
      x = age_interval_order,
      y = mean,
      color = sisarus_ryhma,
      linetype = sisarus_ryhma,
      group = sisarus_ryhma
    )
  ) +
    ggplot2::geom_line(linewidth = 2.1, lineend = "round") +
    ggplot2::geom_point(
      shape = 21,
      size = 3.4,
      fill = col_bg,
      stroke = 1.4
    ) +
    ggplot2::geom_text(
      data = df_end,
      ggplot2::aes(
        x = age_interval_order + 0.20,
        y = label_y,
        label = label
      ),
      inherit.aes = FALSE,
      hjust = 0,
      size = 3.5,
      fontface = "bold",
      color = col_ink
    ) +
    ggplot2::scale_color_manual(
      values = pal,
      breaks = sisarus_labels,
      name = NULL
    ) +
    ggplot2::scale_linetype_manual(
      values = line_types,
      breaks = sisarus_labels,
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(age_labels_ordered),
      labels = age_labels_ordered,
      limits = c(1, 8.85),
      expand = ggplot2::expansion(mult = c(0.01, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 10),
      breaks = seq(0, 10, by = 2),
      expand = ggplot2::expansion(mult = c(0.02, 0.05))
    ) +
    ggplot2::labs(
      title = "Kuormitus sen mukaan, syntyikö sisarus ennen 3 vuoden ikää",
      subtitle = "Keskiarvoviivat (0–10) ryhmittäin. Ei-vastaukset puuttuvaan sisarustietoon rajattu pois.",
      x = "Lapsen ikä",
      y = "Kuormitus (0–10)",
      caption = "Ryhmittely perustuu kysymykseen: Syntyikö sisaruksia ennen 3 vuoden ikää? Sisaruksen tarkkaa syntymäajankohtaa ei kysytty."
    ) +
    theme_linkedin() +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(18, 55, 18, 18)
    ) +
    ggplot2::coord_cartesian(clip = "off")
}

save_burden_mean_by_sisarukset_plot <- function(
  df_long,
  path = file.path("output", "figures", "sisarus_vs_ei_sisarusta.png")
) {
  p <- plot_burden_mean_by_sisarukset(df_long)

  save_plot_png(
    plot = p,
    path = path,
    width = 8,
    height = 10,
    dpi = 320
  )

  invisible(path)
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

  pal <- stats::setNames(c(col_accent, col_neutral, col_dim), labels)
  df_general <- dplyr::filter(df_stats, ryhma == labels[3])
  df_specific <- dplyr::filter(df_stats, ryhma != labels[3])

  ggplot2::ggplot(df_stats, ggplot2::aes(x = age_interval_order, y = mean, color = ryhma, group = ryhma)) +
    ggplot2::geom_line(data = df_specific, linewidth = 2.0) +
    ggplot2::geom_line(data = df_general, linewidth = 1.2, linetype = "dashed") +
    ggplot2::geom_point(shape = 21, size = 3.4, fill = col_bg, stroke = 1.4) +
    ggplot2::scale_color_manual(values = pal, breaks = labels, limits = labels, name = "Lapsen järjestys") +
    ggplot2::scale_x_continuous(breaks = seq_along(age_labels_ordered), labels = age_labels_ordered) +
    ggplot2::scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
    ggplot2::labs(
      title = "Eka kierros tuntuu alkuvaiheessa kuormittavimmalta,\nuseamman lapsen arki näkyy myöhemmin?",
      subtitle = "Keskiarvoviivat (0\u201310) sen mukaan, mit\u00E4 lasta vastaus koskee.",
      x = "Lapsen ik\u00E4",
      y = "Kuormitus (0\u201310)",
      caption = "Ryhmittely perustuu kysymykseen: Mitä lasta tämä vastaus koskee?"
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(order = 1)) +
    theme_linkedin()
}

plot_suurin_vs_pienin_pistemaara <- function(df_long) {
  age_labels_ordered <- c(
    "0\u20133 vko", "3 vko\u20133 kk", "3\u20136 kk", "6\u201312 kk",
    "12\u201318 kk", "18\u201324 kk", "24\u201330 kk", "30\u201336 kk"
  )

  n_periods <- length(age_labels_ordered)

  respondent_summary <- df_long |>
    dplyr::filter(!is.na(burden)) |>
    dplyr::mutate(
      vanhempi = dplyr::case_when(
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^Kyll", ignore_case = TRUE)) ~ "Äiti",
        stringr::str_detect(as.character(synnytitko_lapsi), stringr::regex("^(Ei|En)$", ignore_case = TRUE)) ~ "Isä",
        TRUE ~ "Tieto puuttuu"
      ),
      lapsijarjestys = dplyr::case_when(
        stringr::str_detect(as.character(mita_lasta), stringr::regex("^Ensimm", ignore_case = TRUE)) ~ "Ensimmäinen lapsi",
        stringr::str_detect(as.character(mita_lasta), stringr::regex("^Toista", ignore_case = TRUE)) ~ "Toinen tai myöhempi lapsi",
        stringr::str_detect(as.character(mita_lasta), stringr::regex("Lapsiani", ignore_case = TRUE)) ~ "Lapset yleisesti",
        TRUE ~ "Tieto puuttuu"
      )
    ) |>
    dplyr::group_by(respondent_id) |>
    dplyr::summarise(
      total_score = sum(burden),
      n_answers = dplyr::n(),
      vanhempi = dplyr::first(vanhempi[vanhempi != "Tieto puuttuu"], default = "Tieto puuttuu"),
      lapsijarjestys = dplyr::first(lapsijarjestys[lapsijarjestys != "Tieto puuttuu"], default = "Tieto puuttuu"),
      .groups = "drop"
    ) |>
    dplyr::filter(n_answers == n_periods)

  if (nrow(respondent_summary) < 6) {
    stop("Tarvitaan vähintään 6 vastaajaa, joilla on kuormitusvastaus kaikista 8 ikävaiheesta.")
  }

  top3 <- respondent_summary |>
    dplyr::arrange(dplyr::desc(total_score), respondent_id) |>
    dplyr::slice_head(n = 3) |>
    dplyr::mutate(ryhma = "Suurin summa", sijoitus = dplyr::row_number())

  bottom3 <- respondent_summary |>
    dplyr::anti_join(top3, by = "respondent_id") |>
    dplyr::arrange(total_score, respondent_id) |>
    dplyr::slice_head(n = 3) |>
    dplyr::mutate(ryhma = "Pienin summa", sijoitus = dplyr::row_number())

  selected_ids <- dplyr::bind_rows(top3, bottom3) |>
    dplyr::mutate(
      viivalabel = dplyr::case_when(
        ryhma == "Suurin summa" & lapsijarjestys == "Ensimmäinen lapsi" ~ paste0(sijoitus, ". suurin, eka lapsi"),
        ryhma == "Suurin summa" & lapsijarjestys == "Toinen tai myöhempi lapsi" ~ paste0(sijoitus, ". suurin, toinen+"),
        ryhma == "Suurin summa" & lapsijarjestys == "Lapset yleisesti" ~ paste0(sijoitus, ". suurin, yleisesti"),
        ryhma == "Pienin summa" & lapsijarjestys == "Ensimmäinen lapsi" ~ paste0(sijoitus, ". pienin, eka lapsi"),
        ryhma == "Pienin summa" & lapsijarjestys == "Toinen tai myöhempi lapsi" ~ paste0(sijoitus, ". pienin, toinen+"),
        ryhma == "Pienin summa" & lapsijarjestys == "Lapset yleisesti" ~ paste0(sijoitus, ". pienin, yleisesti"),
        TRUE ~ paste0(sijoitus, ". ", ifelse(ryhma == "Suurin summa", "suurin", "pienin"), ", ", lapsijarjestys)
      ),
      vanhempi = factor(vanhempi, levels = c("Äiti", "Isä", "Tieto puuttuu")),
      lapsijarjestys = factor(
        lapsijarjestys,
        levels = c("Ensimmäinen lapsi", "Toinen tai myöhempi lapsi", "Lapset yleisesti", "Tieto puuttuu")
      ),
      ryhma = factor(ryhma, levels = c("Pienin summa", "Suurin summa"))
    )

  plot_data <- df_long |>
    dplyr::filter(!is.na(burden)) |>
    dplyr::semi_join(selected_ids, by = "respondent_id") |>
    dplyr::left_join(
      selected_ids |>
        dplyr::select(respondent_id, vanhempi, lapsijarjestys, ryhma, viivalabel),
      by = "respondent_id"
    )

  label_df <- plot_data |>
    dplyr::group_by(respondent_id, viivalabel, ryhma) |>
    dplyr::filter(age_interval_order == max(age_interval_order, na.rm = TRUE)) |>
    dplyr::summarise(
      burden = dplyr::first(burden),
      .groups = "drop"
    ) |>
    dplyr::group_by(ryhma) |>
    dplyr::arrange(burden, .by_group = TRUE) |>
    dplyr::mutate(label_y = pmin(pmax(burden + c(-0.45, 0, 0.45), 0.25), 9.75)) |>
    dplyr::ungroup()

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = age_interval_order,
      y = burden,
      group = respondent_id,
      color = vanhempi,
      linetype = ryhma
    )
  ) +
    ggplot2::geom_line(linewidth = 1.2, alpha = 0.95) +
    ggplot2::geom_point(size = 2.1, alpha = 0.85) +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(x = 8.8, y = label_y, label = viivalabel),
      inherit.aes = FALSE,
      hjust = 0,
      size = 3.0,
      color = col_ink
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Äiti" = col_accent,
        "Isä" = col_neutral,
        "Tieto puuttuu" = col_dim
      ),
      breaks = c("Äiti", "Isä"),
      limits = c("Äiti", "Isä", "Tieto puuttuu"),
      name = "Vanhempi"
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        "Suurin summa" = "solid",
        "Pienin summa" = "dotted"
      ),
      breaks = c("Suurin summa", "Pienin summa"),
      limits = c("Pienin summa", "Suurin summa"),
      name = "Pistemääräryhmä"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(age_labels_ordered),
      labels = age_labels_ordered,
      limits = c(1, 9.35),
      expand = ggplot2::expansion(mult = c(0.01, 0.12))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 10),
      breaks = seq(0, 10, by = 2),
      expand = ggplot2::expansion(mult = c(0.02, 0.05))
    ) +
    ggplot2::labs(
      title = "Kuormitus jakautuu jyrkästi: kolme raskaimmaksi ja\nkolme kevyimmäksi kokenutta vastaajaa",
      subtitle = "Väri = äiti/isä, viivatyyppi = suurin vs. pienin kokonaispistemäärä.",
      x = "Lapsen ikä",
      y = "Kuormitus (0\u201310)",
      caption = "Mukana vain vastaajat, joilla oli kuormitusvastaus kaikista 8 ikävaiheesta."
    ) +
    theme_linkedin() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = ggplot2::margin(20, 40, 16, 20)
    ) +
    ggplot2::coord_cartesian(clip = "off")
}

save_suurin_vs_pienin_pistemaara_plot <- function(df_long, path = file.path("output", "figures", "suurin_vs_pienin_pistemaara.png")) {
  p <- plot_suurin_vs_pienin_pistemaara(df_long)
  save_plot_png(p, path = path, width = 11.8, height = 7.5, dpi = 320)
  invisible(path)
}

plot_burden_histogram_faceted <- function(df_long) {
  df_plot <- df_long |>
    dplyr::filter(!is.na(burden), burden >= 0, burden <= 10) |>
    dplyr::mutate(age_interval = factor(age_interval, levels = age_labels_ordered))

  ggplot2::ggplot(df_plot, ggplot2::aes(x = burden)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = -0.5, fill = "#8FBCE3", color = "#2E659D", alpha = 0.95) +
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
