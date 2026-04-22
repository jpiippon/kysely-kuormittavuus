library(tidyverse)

# Blue tile-grid heatmap:
# - Color = burden value proportion within each age interval (0-10)
# - Solid orange = mean
# - Dashed gray = median
# - Top labels = n per age interval
generate_alt_distribution_heatmap_01 <- function(df_long, out_path = file.path("output", "figures_alt", "01 alternative.png")) {
  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)

  age_labels <- c(
    "0–3 vko", "3 vko–3 kk", "3–6 kk", "6–12 kk",
    "12–18 kk", "18–24 kk", "24–30 kk", "30–36 kk"
  )

  df_plot <- df_long |>
    filter(!is.na(burden), burden >= 0, burden <= 10) |>
    mutate(
      burden_int = as.integer(round(burden)),
      age_interval_order = as.integer(age_interval_order)
    )

  # Proportion per integer burden value (full 0..10 x 1..8 grid so missing bins still show tiles)
  df_tiles <- df_plot |>
    count(age_interval_order, burden_int, name = "n") |>
    tidyr::complete(
      age_interval_order = 1:8,
      burden_int = 0:10,
      fill = list(n = 0)
    ) |>
    group_by(age_interval_order) |>
    mutate(
      pct = ifelse(sum(n) > 0, n / sum(n), 0)
    ) |>
    ungroup()

  df_stats <- df_plot |>
    group_by(age_interval_order) |>
    summarise(
      n = n(),
      mean = mean(burden),
      median = median(burden),
      .groups = "drop"
    )

  # Styling close to the existing legacy PNG:
  # - no legend
  # - white tile grid
  p <- ggplot(df_tiles, aes(x = age_interval_order, y = burden_int, fill = n)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_line(
      data = df_stats,
      aes(x = age_interval_order, y = mean),
      color = col_accent,
      linewidth = 2.6,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = df_stats,
      aes(x = age_interval_order, y = mean),
      shape = 21,
      size = 4.2,
      fill = col_bg,
      color = col_accent,
      stroke = 1.9,
      inherit.aes = FALSE
    ) +
    geom_line(
      data = df_stats,
      aes(x = age_interval_order, y = median),
      color = col_neutral,
      linewidth = 1.6,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    geom_point(
      data = df_stats,
      aes(x = age_interval_order, y = median),
      shape = 21,
      size = 3.8,
      fill = col_bg,
      color = col_neutral,
      stroke = 1.6,
      inherit.aes = FALSE
    ) +
    geom_text(
      data = df_stats,
      aes(x = age_interval_order, y = 10.75, label = paste0("n=", n)),
      size = 4.0,
      color = col_neutral,
      vjust = 0,
      inherit.aes = FALSE
    ) +
    scale_x_continuous(
      breaks = 1:8,
      labels = age_labels,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      breaks = 0:10,
      limits = c(0, 11),
      expand = expansion(mult = c(0.0, 0.0))
    ) +
    scale_fill_gradient(
      low = "#D9ECFA",
      high = "#2F5F8E",
      limits = c(0, NA),
      guide = "none"
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = "Kuormitus iän mukaan: jakauma",
      subtitle = "Väri = vastausten määrä (0–10); yhtenäinen = keskiarvo,\nkatkoviiva = mediaani",
      x = "Ikäjakso",
      y = "Kuormitus (0–10)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      axis.title.x = element_text(size = 13, face = "bold", color = col_ink, margin = margin(t = 8)),
      axis.title.y = element_text(size = 13, face = "bold", color = col_ink, margin = margin(r = 8)),
      axis.text.x = element_text(size = 10, color = col_ink),
      axis.text.y = element_text(size = 10, color = col_ink),
      plot.title = element_text(face = "bold", size = 22, color = "#000000", hjust = 0),
      plot.subtitle = element_text(size = 12, color = "#6B7280", hjust = 0, margin = margin(b = 14)),
      plot.margin = margin(10, 10, 10, 10)
    )

  ggsave(filename = out_path, plot = p, width = 6, height = 9, dpi = 320, bg = "white")
  invisible(out_path)
}

# Standalone entrypoint (so you can run: Rscript .\\scripts\\01_generate_alt_distribution_heatmap.R)
main <- function() {
  this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(e) NA_character_)
  script_dir <- if (is.na(this_file) || !nzchar(this_file)) {
    candidate <- file.path(getwd(), "scripts")
    if (dir.exists(candidate)) candidate else getwd()
  } else {
    dirname(this_file)
  }
  project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/")

  source(file.path(script_dir, "00_helpers.R"), local = FALSE)
  source(file.path(script_dir, "01_load_clean.R"), local = FALSE)
  source(file.path(script_dir, "02_reshape_long.R"), local = FALSE)

  df_clean <- load_and_clean_survey()
  df_long <- reshape_burden_long(df_clean)

  generate_alt_distribution_heatmap_01(
    df_long,
    out_path = file.path(project_root, "output", "figures_alt", "01 alternative.png")
  )
}

main()
