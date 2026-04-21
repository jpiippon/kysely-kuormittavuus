load_and_clean_survey <- function(
  raw_path = file.path("data", "raw", "form_responses.csv")
) {
  if (!file.exists(raw_path)) {
    stop("Could not find raw CSV at: ", raw_path)
  }

  df_raw <- readr::read_csv(
    raw_path,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )

  # Trim whitespace from imported raw column names before any matching.
  names(df_raw) <- str_trim(names(df_raw))
  cols <- names(df_raw)

  # Expected Finnish raw column names (from your prompt).
  expected_raw <- list(
    timestamp = "Timestamp",
    synnytitko_lapsi = "Synnytitkö tarkasteltavan lapsen/lapset?",
    mita_lasta = "Mitä lasta tämä vastaus koskee?",
    sisarukset_ennen_3v = "Syntyikö arvioimillesi lapsille(lapselle) sisaruksia(sisarus) ennen 3v ikää? Tämä voi sekoittaa pakkaa..",
    kuormitus_0_3vko = "Syntymästä 3 viikon ikään",
    kuormitus_3vko_3kk = "3 viikon iästä 3 kuukauden ikään",
    kuormitus_3_6kk = "3–6 kk",
    kuormitus_6_12kk = "6–12 kk",
    kuormitus_12_18kk = "1–1,5 vuotta (12-18kk)",
    kuormitus_18_24kk = "1,5–2 vuotta (18-24kk)",
    kuormitus_24_30kk = "2–2,5 vuotta (24-30kk)",
    kuormitus_30_36kk = "2,5–3 vuotta (30kk-36kk)"
  )

  present_all <- function(x) all(purrr::map_lgl(x, ~ .x %in% cols))
  found_exact <- present_all(expected_raw)

  if (!found_exact) {
    missing <- names(expected_raw)[!purrr::map_lgl(expected_raw, ~ .x %in% cols)]
    message("Column matching did not find all expected Finnish raw column names.")
    message("Missing expected raw columns: ", paste(missing, collapse = ", "))
    message("Raw column names actually present (after UTF-8 import + trim):")
    message(paste(cols, collapse = " | "))

    # Fallback positional mapping (known questionnaire order):
    # 1 Timestamp, 2 synnytitkö, 3 mitä lasta koskee, 4-11 burden (8 cols), 12 sisarukset.
    if (length(cols) < 12) {
      stop("Fallback positional mapping failed: fewer than 12 columns detected.")
    }

    timestamp_col <- cols[1]
    birth_col <- cols[2]
    response_about_col <- cols[3]
    siblings_col <- cols[length(cols)]
    burden_cols_in_order <- cols[4:11]
  } else {
    timestamp_col <- expected_raw$timestamp
    birth_col <- expected_raw$synnytitko_lapsi
    response_about_col <- expected_raw$mita_lasta
    siblings_col <- expected_raw$sisarukset_ennen_3v
    burden_cols_in_order <- c(
      expected_raw$kuormitus_0_3vko,
      expected_raw$kuormitus_3vko_3kk,
      expected_raw$kuormitus_3_6kk,
      expected_raw$kuormitus_6_12kk,
      expected_raw$kuormitus_12_18kk,
      expected_raw$kuormitus_18_24kk,
      expected_raw$kuormitus_24_30kk,
      expected_raw$kuormitus_30_36kk
    )
  }

  normalize_yesno <- function(x) {
    x_chr <- as.character(x)
    dplyr::case_when(
      str_detect(x_chr, regex("^Kyll", ignore_case = TRUE)) ~ "Kyllä",
      str_detect(x_chr, regex("^(Ei|En)$", ignore_case = TRUE)) ~ "Ei",
      TRUE ~ x_chr
    )
  }

  normalize_mita_lasta <- function(x) {
    x_chr <- as.character(x)
    dplyr::case_when(
      str_detect(x_chr, regex("Ensimm", ignore_case = TRUE)) ~ "Ensimmäistä lastani",
      str_detect(x_chr, regex("Toista", ignore_case = TRUE)) ~ "Toista tai myöhempää lastani",
      str_detect(x_chr, regex("Lapsiani", ignore_case = TRUE)) ~ "Lapsiani yleisesti",
      TRUE ~ x_chr
    )
  }

  df_raw |>
    rename(
      timestamp = all_of(timestamp_col),
      synnytitko_lapsi = all_of(birth_col),
      mita_lasta = all_of(response_about_col),
      sisarukset_ennen_3v = all_of(siblings_col),
      kuormitus_0_3vko = all_of(burden_cols_in_order[[1]]),
      kuormitus_3vko_3kk = all_of(burden_cols_in_order[[2]]),
      kuormitus_3_6kk = all_of(burden_cols_in_order[[3]]),
      kuormitus_6_12kk = all_of(burden_cols_in_order[[4]]),
      kuormitus_12_18kk = all_of(burden_cols_in_order[[5]]),
      kuormitus_18_24kk = all_of(burden_cols_in_order[[6]]),
      kuormitus_24_30kk = all_of(burden_cols_in_order[[7]]),
      kuormitus_30_36kk = all_of(burden_cols_in_order[[8]])
    ) |>
    mutate(
      timestamp = as.POSIXct(timestamp),
      across(
        starts_with("kuormitus_"),
        ~ suppressWarnings(as.numeric(.x))
      ),
      synnytitko_lapsi = normalize_yesno(synnytitko_lapsi),
      sisarukset_ennen_3v = normalize_yesno(sisarukset_ennen_3v),
      mita_lasta = normalize_mita_lasta(mita_lasta)
    )
}

get_age_interval_specs <- function() {
  list(
    burden_cols = c(
      "kuormitus_0_3vko",
      "kuormitus_3vko_3kk",
      "kuormitus_3_6kk",
      "kuormitus_6_12kk",
      "kuormitus_12_18kk",
      "kuormitus_18_24kk",
      "kuormitus_24_30kk",
      "kuormitus_30_36kk"
    ),
    age_interval_labels = c(
      "0–3 vko",
      "3 vko–3 kk",
      "3–6 kk",
      "6–12 kk",
      "12–18 kk",
      "18–24 kk",
      "24–30 kk",
      "30–36 kk"
    )
  )
}
