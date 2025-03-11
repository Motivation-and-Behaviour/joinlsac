#' Join LSAC Survey Data
#'
#' Reads and joins the survey data from the LSAC dataset.
#'
#' @param datadir The file path to the LSAC dataset.
#' @param cohorts A character vector of the cohorts to include (i.e., either "b"
#'  or "k", or bothks).
#' @param ages A character vector of the ages to include (e.g., "0", "2"...).
#'  You can also specify "all" to include all available ages.
#' @param vars (Not implemented) A character vector of variables to include in
#' the output. Note that this is optional and that all variables are returned
#' by default.
#'
#' @return A dataframe in long format.
#'
#' @examples
#' # ADD_EXAMPLES_HERE
joinsurveys <- function(datadir, cohorts = c("b", "k"), ages = "all", vars) {
  cohorts <- rlang::arg_match(cohorts, multiple = TRUE)

  ages_vals <- c(
    "all", "0", "2", "3", "4", "5", "6", "7", "8", "9", "10", "12", "14", "16",
    "17", "18", "20", "21"
  )
  ages <- rlang::arg_match(ages, values = ages_vals, multiple = TRUE)
  ages <- if ("all" %in% ages) ages_vals[-1] else ages

  survey_dir <- file.path(datadir, "Survey Data", "SPSS")

  file_combinations <- expand.grid(
    cohort = cohorts, age = ages, stringsAsFactors = FALSE
  )
  files_to_read <- c()
  for (i in seq_len(nrow(file_combinations))) {
    cohort <- file_combinations$cohort[i]
    age <- file_combinations$age[i]

    file_path <- file.path(survey_dir, glue::glue("lsacgr{cohort}{age}.sav"))

    if (file.exists(file_path)) {
      files_to_read <- c(files_to_read, file_path)
    }
  }

  df_list <- list()
  factor_vars <- c()
  char_vars <- c()

  pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent) eta: :eta",
    total = length(files_to_read),
    show_after = 0
  )
  pb$tick(0)
  for (file_to_read in files_to_read) {
    df <- foreign::read.spss(file_to_read, to.data.frame = TRUE)
    cohort <- df$cohort[1]
    wave <- df$wave[1]
    letter <-
      lsac_ids[lsac_ids$cohort == cohort & lsac_ids$wave == wave, "letter"]
    ignore_cols <- c("hicid", "stratum", "cohort", "pcodes")
    # There are some columns which don't follow normal prefixing conventions
    odd_vars <- c(
      "pcodeccrp", "stateccrp", "resmovec", "pcodeacrp", "stateacrp",
      "resmovea", "pc83c2", "pw48c1a", "pw48c1b", "pw48c1c", "pw48c1d",
      "pw48c1e", "pw48c1f", "fn21c1", "fn21c2", "pw48a1a", "pw48a1b", "pw48a1c",
      "pw48a1d", "pw48a1e", "pw48a1f", "pw51a1", "pw51a2", "pw51a3", "pw51a4",
      "pw51a5", "pw48a4", "fn21a1", "fn21a2"
    )
    prefixes <- c("i1", "i2", "k1", "k2")
    ignore_cols <- c(ignore_cols, outer(prefixes, odd_vars, paste0))

    names(df) <- ifelse(names(df) %in% ignore_cols, names(df),
      gsub(paste0("^", letter), "", names(df))
    )

    df <- df |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.character), trimws),
        dplyr::across(dplyr::everything(), ~ dplyr::case_when(
          . == -9 ~ NA,
          . == "-9" ~ NA,
          TRUE ~ .
        )),
        dplyr::across(
          dplyr::any_of(c("pcodei", "pcoden", "pcodes")),
          as.numeric
        )
      )

    factor_vars <- union(factor_vars, names(df)[sapply(df, is.factor)])
    char_vars <- union(char_vars, names(df)[sapply(df, is.character)])

    df_list[[length(df_list) + 1]] <- df
    pb$tick()
  }

  for (i in seq_len(length(df_list))) {
    df_list[[i]] <- df_list[[i]] |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(factor_vars), as.character),
        dplyr::across(dplyr::any_of(char_vars), as.character)
      )
  }

  data.table::rbindlist(df_list, fill = TRUE)
}
