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

  names(df_raw) <- stringr::str_trim(names(df_raw))
  cols <- names(df_raw)

  normalize_key <- function(x) {
    x_ascii <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
    x_low <- tolower(x_ascii)
    gsub("[^a-z0-9]+", "", x_low)
  }

  key_map <- stats::setNames(cols, normalize_key(cols))

  pick_col <- function(patterns, fallback_index = NULL, required = TRUE) {
    hits <- unique(unlist(lapply(patterns, function(p) grep(p, names(key_map), value = TRUE))))

    if (length(hits) >= 1) {
      return(unname(key_map[[hits[[1]]]]))
    }

    if (!is.null(fallback_index) && fallback_index <= length(cols)) {
      return(cols[[fallback_index]])
    }

    if (required) {
      stop("Could not map a required raw column. Patterns: ", paste(patterns, collapse = " | "))
    }

    NA_character_
  }

  timestamp_col <- pick_col(c("^timestamp$", "aikaleima"), fallback_index = 1)
  birth_col <- pick_col(c("synnytitko", "synnytitko.*tarkasteltavan"), fallback_index = 2)
  response_about_col <- pick_col(c("mitalasta", "vastauskoskee"), fallback_index = 3)
  siblings_col <- pick_col(c("sisaruksia", "sisarus", "ennen3v"), fallback_index = length(cols))

  if (length(cols) < 12) {
    stop("Expected at least 12 columns in raw data, found: ", length(cols))
  }

  burden_cols_in_order <- cols[4:11]

  normalize_yesno <- function(x) {
    x_chr <- as.character(x)
    dplyr::case_when(
      stringr::str_detect(x_chr, stringr::regex("^Kyll", ignore_case = TRUE)) ~ "Kyllä",
      stringr::str_detect(x_chr, stringr::regex("^(Ei|En)$", ignore_case = TRUE)) ~ "Ei",
      TRUE ~ x_chr
    )
  }

  normalize_mita_lasta <- function(x) {
    x_chr <- as.character(x)
    dplyr::case_when(
      stringr::str_detect(x_chr, stringr::regex("Ensimm", ignore_case = TRUE)) ~ "Ensimmäistä lastani",
      stringr::str_detect(x_chr, stringr::regex("Toista", ignore_case = TRUE)) ~ "Toista tai myöhempää lastani",
      stringr::str_detect(x_chr, stringr::regex("Lapsiani", ignore_case = TRUE)) ~ "Lapsiani yleisesti",
      TRUE ~ x_chr
    )
  }

  df_raw |>
    dplyr::rename(
      timestamp = dplyr::all_of(timestamp_col),
      synnytitko_lapsi = dplyr::all_of(birth_col),
      mita_lasta = dplyr::all_of(response_about_col),
      sisarukset_ennen_3v = dplyr::all_of(siblings_col),
      kuormitus_0_3vko = dplyr::all_of(burden_cols_in_order[[1]]),
      kuormitus_3vko_3kk = dplyr::all_of(burden_cols_in_order[[2]]),
      kuormitus_3_6kk = dplyr::all_of(burden_cols_in_order[[3]]),
      kuormitus_6_12kk = dplyr::all_of(burden_cols_in_order[[4]]),
      kuormitus_12_18kk = dplyr::all_of(burden_cols_in_order[[5]]),
      kuormitus_18_24kk = dplyr::all_of(burden_cols_in_order[[6]]),
      kuormitus_24_30kk = dplyr::all_of(burden_cols_in_order[[7]]),
      kuormitus_30_36kk = dplyr::all_of(burden_cols_in_order[[8]])
    ) |>
    dplyr::mutate(
      timestamp = suppressWarnings(as.POSIXct(timestamp)),
      dplyr::across(dplyr::starts_with("kuormitus_"), ~ suppressWarnings(as.numeric(.x))),
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
