library(tidyverse)
library(flextable)
library(officer)

# Orchestrator-only file: run_pipeline.R is the single place that sources the modular scripts.
# Use absolute paths so the script works regardless of the current working directory
# (e.g., when called from Quarto/render).
this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(e) NA_character_)
script_dir <- if (is.na(this_file) || !nzchar(this_file)) {
  # Fallback for odd execution contexts where $ofile may be missing.
  candidate <- file.path(getwd(), "scripts")
  if (dir.exists(candidate)) candidate else getwd()
} else {
  dirname(this_file)
}

source(file.path(script_dir, "00_helpers.R"), local = FALSE)
source(file.path(script_dir, "01_load_clean.R"), local = FALSE)
source(file.path(script_dir, "02_reshape_long.R"), local = FALSE)
source(file.path(script_dir, "03_figures.R"), local = FALSE)

# Sanity checks: fail fast if sourcing didn't bring the expected plotting functions.
required_fns <- c(
  "plot_burden_heatmap_mean_median",
  "plot_burden_responses_mean_ci_premium",
  "plot_burden_variation_spaghetti",
  "plot_burden_responses_mean_ci_by_synnytitko",
  "plot_burden_mean_by_mita_lasta",
  "save_taustaprofiili_plot"
)
fn_exists <- logical(length(required_fns))
for (i in seq_along(required_fns)) {
  fn_exists[i] <- exists(required_fns[i], mode = "function", inherits = FALSE)
}
missing_fns <- required_fns[!fn_exists]
if (length(missing_fns) > 0) {
  stop(
    "After sourcing scripts, these plotting functions are missing: ",
    paste(missing_fns, collapse = ", "),
    "\nSourced scripts from: ", script_dir
  )
}


main <- function() {
  # ---------------------------
  # Load + clean
  # ---------------------------
  df_clean <- load_and_clean_survey()

  # ---------------------------
  # Reshape to long format
  # ---------------------------
  df_long <- reshape_burden_long(df_clean)

  # ---------------------------
  # Summary table (by age interval)
  # ---------------------------
  df_summary <- make_age_interval_summary(df_long)

  # ---------------------------
  # Figures
  # ---------------------------
  out_figures_dir <- file.path("output", "figures")
  p1_baseline <- if (exists("plot_burden_responses_mean_ci", mode = "function", inherits = FALSE)) {
    plot_burden_responses_mean_ci(df_long)
  } else {
    NULL
  }
  p1_premium <- plot_burden_responses_mean_ci_premium(df_long)
  p0 <- plot_burden_heatmap_mean_median(df_long)
  p2 <- plot_burden_variation_spaghetti(df_long, df_summary)
  p6 <- plot_burden_mean_by_mita_lasta(df_long)
  p7 <- plot_burden_responses_mean_ci_by_synnytitko(df_long)
  p8 <- if (exists("plot_burden_scatter_by_synnytitko", mode = "function", inherits = FALSE)) {
    plot_burden_scatter_by_synnytitko(df_long)
  } else {
    NULL
  }
  p9 <- if (exists("plot_burden_scatter_by_mita_lasta_and_synnytitko", mode = "function", inherits = FALSE)) {
    plot_burden_scatter_by_mita_lasta_and_synnytitko(df_long)
  } else {
    NULL
  }

  # Save an additional baseline (if available) and a premium version for safe comparison,
  # then overwrite the main "01" figure with the premium one.
  if (!is.null(p1_baseline)) {
    save_plot_png(
      p1_baseline,
      file.path(out_figures_dir, "01_burden_trajectory_median_iqr_baseline.png"),
      width = 9,
      height = 6
    )
  }

  save_plot_png(
    p1_premium,
    file.path(out_figures_dir, "01_burden_trajectory_median_iqr.png"),
    width = 9,
    height = 6
  )
  save_plot_png(
    p1_premium,
    file.path(out_figures_dir, "01 pääkuva.png"),
    width = 9,
    height = 6
  )
  save_plot_png(p0, file.path(out_figures_dir, "02 lämpökartta.png"), width = 6, height = 9)
  save_plot_png(p2, file.path(out_figures_dir, "02_burden_variation_spaghetti.png"), width = 12, height = 7)
  save_plot_png(p6, file.path(out_figures_dir, "06_burden_mean_by_mita_lasta.png"), width = 12, height = 6)
  save_plot_png(p7, file.path(out_figures_dir, "07_burden_by_synnyttiko_facet.png"), width = 12, height = 6)
  save_taustaprofiili_plot(
    df_clean,
    path = file.path(out_figures_dir, "00_taustaprofiili_yhdistetty.png")
  )
  if (!is.null(p8)) {
    save_plot_png(p8, file.path(out_figures_dir, "08_burden_scatter_by_synnytitko.png"), width = 12, height = 6)
  }
  if (!is.null(p9)) {
    save_plot_png(p9, file.path(out_figures_dir, "09_burden_scatter_by_mita_lasta_and_synnytitko.png"), width = 12, height = 10)
  }

  # ---------------------------
  # Alternative figures (03.1) - saved separately so we don't overwrite current outputs.
  # ---------------------------
  alt_figures_path <- file.path(script_dir, "03.1 alternative for figures.r")
  if (file.exists(alt_figures_path)) {
    out_figures_dir_alt <- file.path("output", "figures_alt")

    # Source into an isolated environment to avoid function name collisions with 03_figures.R.
    alt_env <- new.env(parent = .GlobalEnv)
    source(alt_figures_path, local = alt_env)

    alt_required_fns <- c(
      "plot_burden_responses_mean_ci_premium",
      "plot_burden_variation_spaghetti",
      "plot_synnytitko_tarkasteltavan_lapsen_v2",
      "plot_mita_lasta_tarkasteltavan_lapsen",
      "plot_syntyiko_lapselle_sisarus_ennen_3v",
      "plot_burden_responses_mean_ci_by_synnytitko",
      "save_taustaprofiili_plot"
    )

    alt_fn_exists <- logical(length(alt_required_fns))
    for (i in seq_along(alt_required_fns)) {
      alt_fn_exists[i] <- exists(alt_required_fns[i], mode = "function", inherits = FALSE, envir = alt_env)
    }

    missing_alt_fns <- alt_required_fns[!alt_fn_exists]
    if (length(missing_alt_fns) > 0) {
      stop(
        "After sourcing alternative figures script, these functions are missing: ",
        paste(missing_alt_fns, collapse = ", "),
        "\nSourced scripts from: ",
        alt_figures_path
      )
    }

    p1_alt <- alt_env$plot_burden_responses_mean_ci_premium(df_long)
    # The alternative script doesn't define plot_burden_heatmap_mean_median; use the primary figure as a stand-in.
    p0_alt <- p1_alt
    p2_alt <- alt_env$plot_burden_variation_spaghetti(df_long, df_summary)
    p3_alt <- alt_env$plot_synnytitko_tarkasteltavan_lapsen_v2(df_clean)
    p4_alt <- alt_env$plot_mita_lasta_tarkasteltavan_lapsen(df_clean)
    p5_alt <- alt_env$plot_syntyiko_lapselle_sisarus_ennen_3v(df_clean)
    p7_alt <- alt_env$plot_burden_responses_mean_ci_by_synnytitko(df_long)

    p8_alt <- NULL
    if (exists("plot_burden_scatter_by_synnytitko", mode = "function", inherits = FALSE, envir = alt_env)) {
      p8_alt <- alt_env$plot_burden_scatter_by_synnytitko(df_long)
    }

    p9_alt <- NULL
    if (exists("plot_burden_scatter_by_mita_lasta_and_synnytitko", mode = "function", inherits = FALSE, envir = alt_env)) {
      p9_alt <- alt_env$plot_burden_scatter_by_mita_lasta_and_synnytitko(df_long)
    }

    save_plot_png(
      p1_alt,
      file.path(out_figures_dir_alt, "01_burden_trajectory_mean_alt.png"),
      width = 9,
      height = 6
    )
    save_plot_png(
      p0_alt,
      file.path(out_figures_dir_alt, "01_burden_heatmap_mean_median_alt.png"),
      width = 6,
      height = 9
    )
    save_plot_png(p2_alt, file.path(out_figures_dir_alt, "02_burden_variation_spaghetti_alt.png"), width = 12, height = 7)
    save_plot_png(p7_alt, file.path(out_figures_dir_alt, "07_burden_by_synnyttiko_facet_alt.png"), width = 12, height = 6)

    alt_env$save_taustaprofiili_plot(
      df_clean,
      path = file.path(out_figures_dir_alt, "00_taustaprofiili_yhdistetty_alt.png")
    )

    if (!is.null(p8_alt)) {
      save_plot_png(p8_alt, file.path(out_figures_dir_alt, "08_burden_scatter_by_synnytitko_alt.png"), width = 12, height = 6)
    }
    if (!is.null(p9_alt)) {
      save_plot_png(p9_alt, file.path(out_figures_dir_alt, "09_burden_scatter_by_mita_lasta_and_synnytitko_alt.png"), width = 12, height = 10)
    }
  } else {
    message("Alternative figures script not found: ", alt_figures_path)
  }
}

main()
