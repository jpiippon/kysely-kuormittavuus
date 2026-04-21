# ------------------------------------------------------------------------------
# Shared styling helpers (kept intentionally simple for readable LinkedIn plots)
# ------------------------------------------------------------------------------

col_navy <- "#002d5a"
col_dark_blue <- "#2f4a73"
col_steel <- "#4a7ba7"
col_mid_blue <- "#6c8eb5"
col_light_blue <- "#a3c1d9"
col_pale_blue <- "#d0e1ef"

col_orange <- "#CC5500"
col_amber <- "#e8a317"

col_dark_text <- "#2a2a2a"

# Premium heatmap palette (cool tones on white background)
col_heat_low <- "#CFE9FF"
col_heat_mid <- "#57A6D9"
col_heat_high <- "#083B63"
col_median_line <- "#5B6777"

theme_linkedin <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0),
      plot.subtitle = element_text(color = "grey40", size = 11, hjust = 0),
      plot.caption = element_text(color = "grey50", size = 9),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11, color = col_dark_text),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(color = "grey85"),
      axis.line.y = element_line(color = "grey85"),
      legend.position = "none",
      plot.margin = margin(12, 14, 10, 10)
    )
}

theme_linkedin_premium_heatmap <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0),
      plot.subtitle = element_text(color = "grey40", size = 11, hjust = 0),
      plot.caption = element_text(color = "grey50", size = 9),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11, color = col_dark_text),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#EEF2F6", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 10),
      plot.margin = margin(12, 14, 18, 10)
    )
}

save_table_csv <- function(df, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(df, path)
}

save_plot_png <- function(plot, path, width = 12, height = 6, dpi = 320) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi)
}
