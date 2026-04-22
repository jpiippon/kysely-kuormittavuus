# ------------------------------------------------------------------------------
# Premium LinkedIn Styling Helpers
# ------------------------------------------------------------------------------

# Tausta & tekstit (Lämmin, editoriaalinen ilme)
col_bg           <- "#FAFAF7"  # Lämmin off-white
col_ink          <- "#1B1F24"  # Otsikkojen lähes-musta
col_text         <- "#2E3440"  # Leipäteksti
col_muted        <- "#6B7280"  # Alaotsikot ja apu-tekstit
col_grid         <- "#E7E5E0"  # Hillitty viivasto
col_rule         <- "#CBC9C2"  # Erotinviivat

# Datan värit (Yksi huomioväri, yksi neutraali)
col_accent       <- "#B4532A"  # Terrakotta (päähuomio)
col_accent_soft  <- "#E9C9B5"  # Terrakotan vaalea nauha (IQR)
col_neutral      <- "#3D4A5C"  # Teräksensininen (vertailuryhmä / mediaani)
col_neutral_soft <- "#C9CED6"  # Teräksen vaalea nauha
col_dim          <- "#9AA0A6"  # Yksittäisten havaintojen kohina

# Gradientti kuplamatriisille
col_scale_lo     <- "#EDE7DD"
col_scale_mid    <- "#B89B7A"
col_scale_hi     <- "#4A3B2A"

# Säilytetään vanhat aliakset, jotta muu mahdollinen koodi ei hajoa
col_orange       <- col_accent
col_mid_blue     <- col_neutral
col_dark_blue    <- col_neutral
col_steel        <- col_neutral
col_light_blue   <- col_neutral_soft
col_pale_blue    <- col_accent_soft
col_navy         <- col_ink
col_amber        <- col_accent
col_dark_text    <- col_text
col_heat_low     <- col_scale_lo
col_heat_mid     <- col_scale_mid
col_heat_high    <- col_scale_hi
col_median_line  <- col_neutral
col_border       <- col_rule

theme_linkedin <- function() {
  theme_minimal(base_size = 13) +
    theme(
      # Taustat
      plot.background  = element_rect(fill = col_bg, color = NA),
      panel.background = element_rect(fill = col_bg, color = NA),
      
      # Typografia
      plot.title       = element_text(face = "bold", size = 18, hjust = 0, color = col_ink, margin = margin(b = 6)),
      plot.subtitle    = element_text(size = 12, hjust = 0, color = col_muted, margin = margin(b = 20)),
      plot.caption     = element_text(size = 9, hjust = 0, color = col_muted, margin = margin(t = 15)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      
      # Akselit
      axis.title.x = element_text(size = 10, face = "bold", color = col_muted, margin = margin(t = 10)),
      axis.title.y = element_text(size = 10, face = "bold", color = col_muted, margin = margin(r = 10)),
      axis.text    = element_text(size = 10, color = col_text),
      
      # Gridit
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = col_grid, linewidth = 0.4),
      
      axis.line   = element_blank(),
      axis.ticks  = element_blank(),
      
      # Legendit
      legend.position   = "top",
      legend.justification = "left",
      legend.title      = element_blank(),
      legend.text       = element_text(size = 11, color = col_text),
      legend.key        = element_blank(),
      legend.background = element_blank(),
      
      # Facet-otsikot
      strip.text = element_text(size = 12, face = "bold", color = col_ink, hjust = 0, margin = margin(b = 8)),
      
      # Marginaalit
      plot.margin = margin(20, 24, 16, 20)
    )
}

theme_linkedin_premium_heatmap <- theme_linkedin

save_table_csv <- function(df, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(df, path)
}

# 9x7 tuumaa toimii erinomaisesti LinkedIn-feedissä (lähes neliö mobiilissa)
save_plot_png <- function(plot, path, width = 9, height = 7, dpi = 320) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi, bg = col_bg)
}

make_age_interval_summary <- function(df_long) {
  # We do not impute; genuine missingness stays NA.
  # Summary stats are computed per interval using available (non-missing) values only.
  df_long |>
    group_by(age_interval, age_interval_order) |>
    summarise(
      n = sum(!is.na(burden)),
      mean = mean(burden, na.rm = TRUE),
      median = median(burden, na.rm = TRUE),
      sd = sd(burden, na.rm = TRUE),
      q05 = quantile(burden, 0.05, na.rm = TRUE, names = FALSE, type = 7),
      q95 = quantile(burden, 0.95, na.rm = TRUE, names = FALSE, type = 7),
      q25 = quantile(burden, 0.25, na.rm = TRUE, names = FALSE, type = 7),
      q75 = quantile(burden, 0.75, na.rm = TRUE, names = FALSE, type = 7),
      .groups = "drop"
    ) |>
    arrange(age_interval_order) |>
    select(age_interval, n, mean, median, sd, q05, q95, q25, q75)
}
