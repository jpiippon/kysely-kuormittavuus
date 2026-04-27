library(magick)

# Build a single multi-page PDF carousel from the final LinkedIn PNG figures.
# This script does not modify the source images.

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

figures_dir <- file.path(dirname(script_dir), "output", "figures")
out_path <- file.path(dirname(script_dir), "output", "linkedin", "linkedin_karuselli_lapsiarki.pdf")

figure_files <- c(
  file.path(figures_dir, "01_paakuva.png"),
  file.path(figures_dir, "02_heatmap.png"),
  file.path(figures_dir, "03_aidit_vs_isat.png"),
  file.path(figures_dir, "04_monesko_lapsi.png"),
  file.path(figures_dir, "05_taustaprofiili.png")
)

missing_files <- figure_files[!file.exists(figure_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing figure file(s): ",
    paste(missing_files, collapse = ", ")
  )
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

imgs <- image_read(figure_files)
image_write(imgs, path = out_path, format = "pdf")

info <- file.info(out_path)
message("PDF created: ", normalizePath(out_path, winslash = "/", mustWork = FALSE))
message("Pages: ", length(figure_files))
message("File size (bytes): ", info$size)

