# Alternative script for figures for comparison
# scripts
# Premium LinkedIn -visualisoinnit kuormituskyselylle.
#
# Filosofia:
#   - Keskiarvo on pääsignaali (tuttu kaikille, ei kognitiivista taukoa)
#   - Mediaania ei näytetä ollenkaan
#   - Pistepilvi kertoo hajonnan ilman erillisiä hajontakuvia
#   - Yksi huomioväri (terrakotta) per kuva
#   - Jokaisella kuvalla väittävä otsikko + selittävä alaotsikko
#   - Numeroarvot näkyvät suoraan kuvassa

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(stringr); library(forcats); library(scales)
})

# Ikäjaksojen nimet järjestyksessä (käytetään kaikissa funktioissa)
AGE_LABELS <- c(
  "0–3 vko", "3 vko–3 kk", "3–6 kk", "6–12 kk",
  "12–18 kk", "18–24 kk", "24–30 kk", "30–36 kk"
)

# Sisäinen apuri: kontrolloitu pistepilvi (ei satunnainen jitter)
# Jakaa pisteet siisteihin pystyriveihin sen sijaan että ne kasautuisivat kokonaisluvuille.
.make_jitter_points <- function(df, n_bins = 6, spread = 0.18) {
  df |>
    filter(!is.na(burden), burden >= 0, burden <= 10) |>
    group_by(age_interval_order) |>
    mutate(
      resp_idx = row_number(),
      bin      = (resp_idx - 1L) %% n_bins + 1L,
      x_jitter = age_interval_order + (bin - (n_bins + 1) / 2) * spread
    ) |>
    ungroup()
}

# Sisäinen apuri: keskiarvo-summary
.mean_summary <- function(df, group_vars = NULL) {
  df |>
    filter(!is.na(burden)) |>
    group_by(across(all_of(c(group_vars, "age_interval_order")))) |>
    summarise(
      n    = n(),
      mean = mean(burden),
      .groups = "drop"
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# 1) PÄÄKUVA: Kontrolloitu pistepilvi + keskiarvoviiva + annotaatio
# ─────────────────────────────────────────────────────────────────────────────
plot_burden_responses_mean_ci_premium <- function(df_long) {

  df_points <- .make_jitter_points(df_long)
  df_stats  <- .mean_summary(df_long)

  peak     <- df_stats |> slice_max(mean, n = 1, with_ties = FALSE)
  first_pt <- df_stats |> slice_min(age_interval_order, n = 1)
  last_pt  <- df_stats |> slice_max(age_interval_order, n = 1)

  ggplot() +
    # Pistepilvi taustalla — kertoo hajonnan
    geom_point(
      data  = df_points,
      aes(x = x_jitter, y = burden),
      color = col_dim, alpha = 0.28, size = 1.9
    ) +
    # Kevyet vaakaviivat 4–6 alueelle (missä data tiivistyy)
    geom_hline(yintercept = c(4, 5, 6), color = col_grid, linewidth = 0.5, linetype = "solid") +
    # Keskiarvoviiva — pääsignaali
    geom_line(
      data = df_stats, aes(x = age_interval_order, y = mean, group = 1),
      color = col_accent, linewidth = 2.2
    ) +
    geom_point(
      data = df_stats, aes(x = age_interval_order, y = mean),
      shape = 21, size = 4.0, fill = col_bg, color = col_accent, stroke = 1.8
    ) +
    # Numeroarvot alku- ja loppupisteisiin
    geom_text(
      data = first_pt, aes(x = age_interval_order - 0.12, y = mean, label = sprintf("%.1f", mean)),
      hjust = 1, size = 3.6, color = col_muted, fontface = "bold"
    ) +
    geom_text(
      data = last_pt, aes(x = age_interval_order + 0.12, y = mean, label = sprintf("%.1f", mean)),
      hjust = 0, size = 3.6, color = col_muted, fontface = "bold"
    ) +
    # Huippuannotaatio
    annotate("segment", x = peak$age_interval_order, xend = peak$age_interval_order, y = peak$mean + 0.7, yend = 9.2, color = col_rule, linewidth = 0.4) +
    annotate("text", x = peak$age_interval_order, y = 9.65, label = sprintf("Kuormitushuippu\nkeskiarvo %.1f / 10", peak$mean), hjust = 0.5, vjust = 0, size = 3.8, color = col_ink, fontface = "bold", lineheight = 1.1) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), expand = expansion(mult = c(0.02, 0.08))) +
    scale_x_continuous(breaks = seq_along(AGE_LABELS), labels = AGE_LABELS, expand = expansion(mult = c(0.04, 0.05))) +
    coord_cartesian(clip = "off") +
    labs(
      title    = "Kuormitus huipentuu, kun vauva on 3–6 kuukauden ikäinen",
      subtitle = "Pisteet = yksittäiset vastaukset. Oranssi viiva = ikävaiheen keskiarvo (0–10).",
      x = NULL, y = "Koettu kuormitus (0–10)",
      caption = sprintf("Kysely, n = %d vastaajaa.", max(df_stats$n))
    ) +
    theme_linkedin() +
    theme(panel.grid.major.y = element_blank(), legend.position = "none")
}

# ─────────────────────────────────────────────────────────────────────────────
# 2) Synnyttänyt vs. ei — yksi paneeli, kaksi viivaa, ei mediaania
# ─────────────────────────────────────────────────────────────────────────────
plot_burden_responses_mean_ci_by_synnytitko <- function(df_long) {

  lbl <- c("Äidit (synnyttäneet)", "Isät (ei-synnyttäneet)")

  df_clean <- df_long |>
    mutate(ryhma = case_when(
      str_detect(as.character(synnytitko_lapsi), regex("^Kyll", ignore_case = TRUE)) ~ lbl[1],
      str_detect(as.character(synnytitko_lapsi), regex("^(Ei|En)$", ignore_case = TRUE)) ~ lbl[2],
      TRUE ~ NA_character_
    )) |>
    filter(!is.na(ryhma), !is.na(burden)) |>
    mutate(ryhma = factor(ryhma, levels = lbl))

  df_stats <- .mean_summary(df_clean, group_vars = "ryhma")

  # Laske ero viimeisessä pisteessä annotaatiota varten
  df_diff <- df_stats |>
    select(ryhma, age_interval_order, mean) |>
    pivot_wider(names_from = ryhma, values_from = mean) |>
    mutate(diff = .data[[lbl[1]]] - .data[[lbl[2]]]) |>
    filter(!is.na(diff))

  last_diff <- df_diff |> slice_max(age_interval_order, n = 1)

  ggplot(df_stats, aes(x = age_interval_order, y = mean, color = ryhma, group = ryhma)) +
    geom_line(linewidth = 2.0) +
    geom_point(shape = 21, size = 3.6, fill = col_bg, stroke = 1.6) +
    # Ero viimeisessä pisteessä
    annotate("text", x = last_diff$age_interval_order + 0.15, y = mean(c(last_diff[[lbl[1]]], last_diff[[lbl[2]]])), label = sprintf("Ero\n%.1f yks.", abs(last_diff$diff)), hjust = 0, size = 3.4, color = col_muted, lineheight = 1.0) +
    scale_color_manual(values = setNames(c(col_accent, col_neutral), lbl)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_continuous(breaks = seq_along(AGE_LABELS), labels = AGE_LABELS, expand = expansion(mult = c(0.04, 0.12))) +
    coord_cartesian(clip = "off") +
    labs(
      title    = "Äidit ja isät kokevat kuormituksen samankaltaisesti",
      subtitle = "Keskiarvo (0–10) ryhmittäin. Terrakotta = synnyttäneet, siniharmaa = ei-synnyttäneet.",
      x = NULL, y = "Kuormitus (0–10)",
      caption = 'Ryhmittely: "Synnyttikö tarkasteltavan lapsen?"'
    ) +
    theme_linkedin()
}

# ─────────────────────────────────────────────────────────────────────────────
# 3) Yksilöllinen vaihtelu — spagetti + keskiarvo
# ─────────────────────────────────────────────────────────────────────────────
plot_burden_variation_spaghetti <- function(df_long, df_summary) {

  df_s <- df_summary |> mutate(age_interval_order = as.integer(age_interval))
  age_labels <- levels(df_summary$age_interval)
  df_stats <- .mean_summary(df_long)

  ggplot(df_long, aes(x = age_interval_order, y = burden, group = respondent_id)) +
    # Yksittäiset polut — hyvin hiljaa taustalla
    geom_line(color = col_dim, linewidth = 0.3, alpha = 0.10) +
    # Kevyet vaakaviivat 4–6 alueelle
    geom_hline(yintercept = c(4, 5, 6), color = col_grid, linewidth = 0.5) +
    # Keskiarvo päälle — pääsignaali
    geom_line(data = df_stats, aes(x = age_interval_order, y = mean, group = 1), inherit.aes = FALSE, color = col_accent, linewidth = 2.2) +
    geom_point(data = df_stats, aes(x = age_interval_order, y = mean), inherit.aes = FALSE, shape = 21, size = 3.6, fill = col_bg, color = col_accent, stroke = 1.7) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
    scale_x_continuous(breaks = seq_along(age_labels), labels = age_labels) +
    labs(
      title    = "Yksilöllinen hajonta on valtavaa – kokonaiskuva silti selvä",
      subtitle = "Ohut harmaa viiva = yksittäinen vastaaja. Oranssi viiva = kaikkien keskiarvo.",
      x = NULL, y = "Kuormitus (0–10)",
      caption = sprintf("Vastaajia per ikäjakso %d–%d.", min(df_stats$n), max(df_stats$n))
    ) +
    theme_linkedin() +
    theme(panel.grid.major.y = element_blank(), legend.position = "none")
}

# ─────────────────────────────────────────────────────────────────────────────
# 4) Monennesta lapsesta — kolme ryhmää
# ─────────────────────────────────────────────────────────────────────────────
plot_burden_by_mita_lasta <- function(df_long) {

  lbl <- c("Ensimmäinen lapsi", "Toinen tai myöhempi", "Lapset yleisesti")

  df_clean <- df_long |>
    mutate(ryhma = case_when(
      str_detect(as.character(mita_lasta), regex("^Ensimm", ignore_case = TRUE)) ~ lbl[1],
      str_detect(as.character(mita_lasta), regex("^Toista",  ignore_case = TRUE)) ~ lbl[2],
      str_detect(as.character(mita_lasta), regex("Lapsiani", ignore_case = TRUE)) ~ lbl[3],
      TRUE ~ NA_character_
    )) |>
    filter(!is.na(ryhma), !is.na(burden)) |>
    mutate(ryhma = factor(ryhma, levels = lbl))

  df_stats <- .mean_summary(df_clean, group_vars = "ryhma")

  pal <- c(col_accent, col_neutral, col_dim)
  names(pal) <- lbl

  ggplot(df_stats, aes(x = age_interval_order, y = mean, color = ryhma, group = ryhma)) +
    geom_line(linewidth = 1.9) +
    geom_point(shape = 21, size = 3.2, fill = col_bg, stroke = 1.5) +
    scale_color_manual(values = pal) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_continuous(breaks = seq_along(AGE_LABELS), labels = AGE_LABELS) +
    labs(
      title    = "Ensimmäisen lapsen kanssa kuormitus huipentuu terävämmin",
      subtitle = "Keskiarvo (0–10) sen mukaan, mitä lasta vastaus koskee.",
      x = NULL, y = "Kuormitus (0–10)",
      caption = 'Ryhmittely: "Mitä lasta tämä vastaus koskee?"'
    ) +
    theme_linkedin()
}

# ─────────────────────────────────────────────────────────────────────────────
# 5) Sisarusvaikutus
# ─────────────────────────────────────────────────────────────────────────────
plot_burden_by_sisarus <- function(df_long) {

  lbl <- c("Sisarus syntyi ennen 3 v", "Ei sisarusta ennen 3 v")

  df_clean <- df_long |>
    mutate(ryhma = case_when(
      str_detect(as.character(sisarukset_ennen_3v), regex("^Kyll", ignore_case = TRUE)) ~ lbl[1],
      str_detect(as.character(sisarukset_ennen_3v), regex("^(Ei|En)$", ignore_case = TRUE)) ~ lbl[2],
      TRUE ~ NA_character_
    )) |>
    filter(!is.na(ryhma), !is.na(burden)) |>
    mutate(ryhma = factor(ryhma, levels = lbl))

  df_stats <- .mean_summary(df_clean, group_vars = "ryhma")

  ggplot(df_stats, aes(x = age_interval_order, y = mean, color = ryhma, group = ryhma)) +
    geom_line(linewidth = 1.9) +
    geom_point(shape = 21, size = 3.2, fill = col_bg, stroke = 1.5) +
    scale_color_manual(values = setNames(c(col_accent, col_neutral), lbl)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
    scale_x_continuous(breaks = seq_along(AGE_LABELS), labels = AGE_LABELS) +
    labs(
      title    = "Uusi sisarus näkyy kuormituksessa loppusuoralla",
      subtitle = "Keskiarvo (0–10) sen mukaan, syntyikö sisarus ennen 3-vuotispäivää.",
      x = NULL, y = "Kuormitus (0–10)"
    ) +
    theme_linkedin()
}

# ─────────────────────────────────────────────────────────────────────────────
# 6) Taustaprofiili — yhdistetty The Economist -tyylinen palkkikuva
# ─────────────────────────────────────────────────────────────────────────────
plot_taustaprofiili_yhdistetty <- function(df_clean) {

  specs <- tribble(
    ~muuttuja,             ~kysymys,
    "synnytitko_lapsi",    "Synnyttikö vastaaja tarkasteltavan lapsen?",
    "mita_lasta",          "Mitä lasta tämä vastaus koskee?",
    "sisarukset_ennen_3v", "Syntyikö sisaruksia ennen 3 vuoden ikää?"
  )

  n_total <- nrow(df_clean)

  summary_df <- df_clean |>
    select(all_of(specs$muuttuja)) |>
    pivot_longer(everything(), names_to = "muuttuja", values_to = "arvo") |>
    mutate(arvo = if_else(is.na(arvo) | !nzchar(as.character(arvo)), "Ei vastausta", as.character(arvo))) |>
    left_join(specs, by = "muuttuja") |>
    count(muuttuja, kysymys, arvo, name = "n") |>
    group_by(muuttuja, kysymys) |>
    mutate(
      n_total   = sum(n),
      pct       = n / n_total,
      highlight = pct == max(pct),
      label     = sprintf("%d · %.0f %%", n, pct * 100),
      arvo      = fct_reorder(arvo, pct)
    ) |>
    ungroup() |>
    mutate(kysymys = factor(kysymys, levels = specs$kysymys))

  ggplot(summary_df, aes(x = pct, y = arvo, fill = highlight)) +
    geom_col(width = 0.58) +
    # Älykäs label: sisällä jos palkki riittävän pitkä, muuten ulkona
    geom_text(
      aes(
        label = label,
        hjust = ifelse(pct > 0.18, 1.07, -0.06),
        color = case_when(
          pct > 0.18 & highlight ~ "in_accent",
          pct > 0.18             ~ "in_neutral",
          TRUE                   ~ "out"
        )
      ),
      size = 3.8, fontface = "bold"
    ) +
    scale_fill_manual(values = c(`TRUE` = col_accent, `FALSE` = col_neutral_soft), guide = "none") +
    scale_color_manual(values = c(in_accent = col_bg, in_neutral = col_ink, out = col_ink), guide = "none") +
    scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), breaks = c(0, 0.5, 1), expand = expansion(mult = c(0, 0.12))) +
    facet_wrap(~ kysymys, scales = "free_y", ncol = 1) +
    labs(
      title    = "Kyselyn taustaprofiili",
      subtitle = paste0("Vastauksia yhteensä n = ", n_total, ". Yleisin vastaus korostettu terrakotalla."),
      x = NULL, y = NULL
    ) +
    theme_linkedin() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = col_grid, linewidth = 0.35),
      axis.text.x  = element_text(size = 9, color = col_muted),
      axis.text.y  = element_text(size = 11, color = col_ink),
      strip.text   = element_text(face = "bold", hjust = 0, size = 11, margin = margin(t = 12, b = 4))
    )
}

save_taustaprofiili_plot <- function(df_clean, path = file.path("output", "figures", "00_taustaprofiili_yhdistetty.png")) {
  p <- plot_taustaprofiili_yhdistetty(df_clean)
  save_plot_png(p, path = path, width = 8, height = 9, dpi = 320)
  invisible(path)
}

# ─────────────────────────────────────────────────────────────────────────────
# Yksittäiset taustamuuttujat (yhteensopivuus)
# ─────────────────────────────────────────────────────────────────────────────
.plot_single_bg_bar <- function(df_counts, lbl_levels, title, subtitle = NULL) {
  df_counts <- df_counts |>
    mutate(
      label     = factor(label, levels = lbl_levels),
      total_n   = sum(n),
      pct       = n / total_n,
      highlight = n == max(n),
      bar_label = sprintf("%d · %.0f %%", n, pct * 100)
    )
  
  # Ratkaisu perus-R:llä ilman rlangin %||% operaattoria
  sub_text <- if (!is.null(subtitle)) subtitle else paste0("n = ", unique(df_counts$total_n))

  ggplot(df_counts, aes(x = pct, y = fct_reorder(label, pct), fill = highlight)) +
    geom_col(width = 0.52) +
    geom_text(
      aes(
        label = bar_label,
        hjust = ifelse(pct > 0.18, 1.07, -0.06),
        color = ifelse(pct > 0.18 & highlight, "in_accent", ifelse(pct > 0.18, "in_neutral", "out"))
      ),
      size = 4.0, fontface = "bold"
    ) +
    scale_fill_manual(values = c(`TRUE` = col_accent, `FALSE` = col_neutral_soft), guide = "none") +
    scale_color_manual(values = c(in_accent = col_bg, in_neutral = col_ink, out = col_ink), guide = "none") +
    scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), breaks = c(0, 0.5, 1), expand = expansion(mult = c(0, 0.14))) +
    labs(title = title, subtitle = sub_text, x = NULL, y = NULL) +
    theme_linkedin() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = col_grid, linewidth = 0.35),
      axis.text.y = element_text(size = 11, color = col_ink),
      axis.text.x = element_text(size = 9, color = col_muted)
    )
}

plot_synnytitko_tarkasteltavan_lapsen_v2 <- function(df_clean) {
  lbl <- c("Äidit (synnyttäneet)", "Isät (ei-synnyttäneet)")
  df_counts <- df_clean |>
    mutate(label = case_when(
      str_detect(as.character(synnytitko_lapsi), regex("^Kyll", ignore_case = TRUE)) ~ lbl[1],
      str_detect(as.character(synnytitko_lapsi), regex("^(Ei|En)$", ignore_case = TRUE)) ~ lbl[2],
      TRUE ~ NA_character_
    )) |> filter(!is.na(label)) |> count(label, name = "n")
  .plot_single_bg_bar(df_counts, lbl, "Synnyttikö vastaaja tarkasteltavan lapsen?")
}

plot_mita_lasta_tarkasteltavan_lapsen <- function(df_clean) {
  lbl <- c("Ensimmäistä lastani", "Toista tai myöhempää lastani", "Lapsiani yleisesti")
  df_counts <- df_clean |>
    mutate(label = case_when(
      str_detect(as.character(mita_lasta), regex("^Ensimm", ignore_case = TRUE)) ~ lbl[1],
      str_detect(as.character(mita_lasta), regex("^Toista",  ignore_case = TRUE)) ~ lbl[2],
      str_detect(as.character(mita_lasta), regex("Lapsiani", ignore_case = TRUE)) ~ lbl[3],
      TRUE ~ NA_character_
    )) |> filter(!is.na(label)) |> count(label, name = "n")
  .plot_single_bg_bar(df_counts, lbl, "Mitä lasta tämä vastaus koskee?")
}

plot_syntyiko_lapselle_sisarus_ennen_3v <- function(df_clean) {
  lbl <- c("Kyllä, syntyi", "Ei syntynyt")
  df_counts <- df_clean |>
    mutate(label = case_when(
      str_detect(as.character(sisarukset_ennen_3v), regex("^Kyll", ignore_case = TRUE)) ~ lbl[1],
      str_detect(as.character(sisarukset_ennen_3v), regex("^(Ei|En)$", ignore_case = TRUE)) ~ lbl[2],
      TRUE ~ NA_character_
    )) |> filter(!is.na(label)) |> count(label, name = "n")
  .plot_single_bg_bar(df_counts, lbl, "Syntyikö lapselle sisarus ennen 3 vuoden ikää?")
}