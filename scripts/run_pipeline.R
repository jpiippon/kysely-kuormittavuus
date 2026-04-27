library(tidyverse)
library(flextable)
library(officer)

# Orchestrator-only file: run_pipeline.R is the single place that sources the modular scripts.
# Use absolute paths so the script works regardless of the current working directory.
this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) NA_character_
)
script_dir <- if (is.na(this_file) || !nzchar(this_file)) {
  candidate <- file.path(getwd(), "scripts")
  if (dir.exists(candidate)) candidate else getwd()
} else {
  dirname(this_file)
}

source(file.path(script_dir, "00_helpers.R"), local = FALSE)
source(file.path(script_dir, "01_load_clean.R"), local = FALSE)
source(file.path(script_dir, "02_reshape_long.R"), local = FALSE)
source(file.path(script_dir, "03_figures.R"), local = FALSE)

required_fns <- c(
  "plot_burden_heatmap_mean_median",
  "plot_burden_responses_mean_ci_premium",
  "plot_burden_responses_mean_ci_by_synnytitko",
  "plot_burden_mean_by_sisarukset",
  "plot_burden_mean_by_mita_lasta",
  "plot_burden_histogram_faceted",
  "save_taustaprofiili_plot",
  "save_burden_mean_by_sisarukset_plot",
  "save_suurin_vs_pienin_pistemaara_plot"
)

missing_fns <- required_fns[!vapply(required_fns, exists, logical(1), mode = "function", inherits = TRUE)]
if (length(missing_fns) > 0) {
  stop(
    "After sourcing scripts, these plotting functions are missing: ",
    paste(missing_fns, collapse = ", "),
    "\nSourced scripts from: ", script_dir
  )
}

save_main_figures <- function(df_clean, df_long, out_figures_dir = file.path("output", "figures")) {
  dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)

  # Keep output deterministic: remove old PNG files before saving current final set.
  old_pngs <- list.files(out_figures_dir, pattern = "\\.png$", full.names = TRUE)
  if (length(old_pngs) > 0) {
    unlink(old_pngs, force = TRUE)
  }

  complete_ids <- df_long |>
    dplyr::filter(!is.na(burden)) |>
    dplyr::group_by(respondent_id) |>
    dplyr::summarise(n_periods = dplyr::n_distinct(age_interval_order), .groups = "drop") |>
    dplyr::filter(n_periods == 8) |>
    dplyr::pull(respondent_id)

  df_long_complete <- df_long |>
    dplyr::semi_join(tibble::tibble(respondent_id = complete_ids), by = "respondent_id")

  plot_specs <- list(
    list(
      plot = plot_burden_responses_mean_ci_premium(df_long),
      path = file.path(out_figures_dir, "01_paakuva.png")
    ),
    list(
      plot = plot_burden_responses_mean_ci_premium(
        df_long_complete,
        extra_subtitle = "Herkkyystarkastelu: valittu vain vastaajat, jotka vastasivat jokaiseen ikävaiheeseen"
      ),
      path = file.path(out_figures_dir, "herkkyys_paakuva.png")
    ),
    list(
      plot = plot_burden_heatmap_mean_median(df_long),
      path = file.path(out_figures_dir, "02_heatmap.png")
    ),
    list(
      plot = plot_burden_responses_mean_ci_by_synnytitko(df_long),
      path = file.path(out_figures_dir, "03_aidit_vs_isat.png")
    ),
    list(
      plot = plot_burden_mean_by_mita_lasta(df_long),
      path = file.path(out_figures_dir, "04_monesko_lapsi.png")
    ),
    list(
      plot = plot_taustaprofiili_yhdistetty(df_clean, include_question_caption = TRUE),
      path = file.path(out_figures_dir, "05_taustaprofiili.png")
    ),
    list(
      plot = plot_burden_histogram_faceted(df_long),
      path = file.path(out_figures_dir, "histogrammi.png")
    ),
    list(
      plot = plot_burden_mean_by_sisarukset(df_long),
      path = file.path(out_figures_dir, "sisarus_vs_ei_sisarusta.png")
    ),
    list(
      plot = plot_suurin_vs_pienin_pistemaara(df_long),
      path = file.path(out_figures_dir, "suurin_vs_pienin_pistemaara.png")
    )
  )

  saved_paths <- character(0)
  for (spec in plot_specs) {
    save_plot_png(spec$plot, spec$path, width = 8, height = 10, dpi = 450)
    saved_paths <- c(saved_paths, normalizePath(spec$path, winslash = "/", mustWork = FALSE))
  }

  message(
    "Tallennetut LinkedIn-kuvat:\n",
    paste(sprintf("- %s", saved_paths), collapse = "\n")
  )

  invisible(out_figures_dir)
}

main <- function() {
  message("Running scripts/run_pipeline.R (clean-figures mode, updated 2026-04-23)")
  df_clean <- load_and_clean_survey()
  df_long <- reshape_burden_long(df_clean)

  save_main_figures(df_clean, df_long)

  invisible(NULL)
}

main()
