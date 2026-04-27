# ------------------------------------------------------------------------------
# Shared helpers: style, saving, and summaries
# ------------------------------------------------------------------------------

col_bg <- "#FAFAF7"
col_ink <- "#1B1F24"
col_text <- "#2E3440"
col_muted <- "#6B7280"
col_grid <- "#E7E5E0"
col_rule <- "#CBC9C2"

col_accent <- "#B4532A"
col_accent_soft <- "#E9C9B5"
col_neutral <- "#3D4A5C"
col_neutral_soft <- "#C9CED6"
col_dim <- "#9AA0A6"

ensure_utf8_locale <- function() {
  try(options(encoding = "UTF-8"), silent = TRUE)

  candidate_locales <- c(
    "fi_FI.UTF-8",
    "Finnish_Finland.utf8",
    "en_US.UTF-8",
    "C.UTF-8"
  )

  for (loc in candidate_locales) {
    ok <- tryCatch(!is.na(Sys.setlocale("LC_CTYPE", loc)), error = function(e) FALSE)
    if (ok) break
  }

  invisible(NULL)
}

theme_linkedin <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = col_bg, color = NA),
      panel.background = ggplot2::element_rect(fill = col_bg, color = NA),
      plot.title = ggplot2::element_text(face = "bold", size = 18, hjust = 0, color = col_ink, margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0, color = col_muted, margin = ggplot2::margin(b = 14)),
      plot.caption = ggplot2::element_text(size = 9, hjust = 0, color = col_muted, margin = ggplot2::margin(t = 14)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.title.x = ggplot2::element_text(size = 10, face = "bold", color = col_muted, margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(size = 10, face = "bold", color = col_muted, margin = ggplot2::margin(r = 10)),
      axis.text = ggplot2::element_text(size = 10, color = col_text),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = col_grid, linewidth = 0.4),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 10, color = col_text),
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 11, face = "bold", color = col_ink, hjust = 0),
      plot.margin = ggplot2::margin(20, 24, 16, 20)
    )
}

save_table_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path)
}

save_plot_png <- function(plot, path, width = 8, height = 10, dpi = 450) {
  ensure_utf8_locale()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (requireNamespace("ragg", quietly = TRUE)) {
    ggplot2::ggsave(
      filename = path,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      bg = col_bg,
      device = ragg::agg_png
    )
  } else {
    ggplot2::ggsave(
      filename = path,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      bg = col_bg
    )
  }
}

make_age_interval_summary <- function(df_long) {
  df_long |>
    dplyr::group_by(age_interval, age_interval_order) |>
    dplyr::summarise(
      n = sum(!is.na(burden)),
      mean = mean(burden, na.rm = TRUE),
      median = median(burden, na.rm = TRUE),
      sd = stats::sd(burden, na.rm = TRUE),
      q05 = stats::quantile(burden, 0.05, na.rm = TRUE, names = FALSE),
      q95 = stats::quantile(burden, 0.95, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) |>
    dplyr::arrange(age_interval_order)
}
