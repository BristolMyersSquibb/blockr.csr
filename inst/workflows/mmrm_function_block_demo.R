# MMRM Function Block Demo
# Demonstrates converting hardcoded MMRM analysis to function_block with exposed parameters

library(blockr)
library(blockr.csr)

# MMRM Analysis as a function with exposed parameters
mmrm_analysis <- function(
    data,
    method = c("Kenward-Roger", "Satterthwaite", "Residual"),
    covariance = c("Unstructured", "Compound Symmetry", "AR(1)"),
    include_strata = TRUE,
    include_region = TRUE,
    title = "MMRM Analysis"
) {
  method <- match.arg(method)
  covariance <- match.arg(covariance)

  # Map covariance choice to mmrm function
  cov_struct <- switch(covariance,
    "Unstructured" = "us",
    "Compound Symmetry" = "cs",
    "AR(1)" = "ar1"
  )

  # Prepare data

  merged <- data |>
    dplyr::filter(
      !is.na(AVISIT), AVISIT != "", !is.na(CHG),
      !AVISIT %in% c("BASELINE", "SCREENING")
    ) |>
    dplyr::mutate(
      TRTN = factor(TRT02P),
      AVISITN = factor(AVISITN),
      USUBJID = factor(USUBJID),
      STRATA1 = factor(STRATA1),
      REGION1 = factor(REGION1)
    )

  # Build formula dynamically based on covariate selection
  base_formula <- "CHG ~ TRTN + AVISITN + TRTN:AVISITN + BASE"
  if (include_strata) base_formula <- paste(base_formula, "+ STRATA1")
  if (include_region) base_formula <- paste(base_formula, "+ REGION1")

  # Add covariance structure
  full_formula <- paste0(base_formula, " + ", cov_struct, "(AVISITN | USUBJID)")

  # Fit model
  fit <- mmrm::mmrm(
    formula = stats::as.formula(full_formula),
    data = merged,
    method = method
  )

  # Get estimates
  em <- emmeans::emmeans(fit, ~ TRTN | AVISITN)
  lsm <- as.data.frame(em)
  pairs_em <- graphics::pairs(em, reverse = TRUE)
  diffs <- as.data.frame(stats::confint(pairs_em))
  pairs_df <- as.data.frame(summary(pairs_em))

  visits <- sort(unique(as.numeric(as.character(lsm$AVISITN))))
  trts <- levels(merged$TRTN)

  # Build results table
  results <- lapply(visits, function(v) {
    trt_rows <- lapply(trts, function(trt) {
      row <- lsm |> dplyr::filter(AVISITN == v, TRTN == trt)
      if (nrow(row) > 0) {
        tibble::tibble(
          Visit = paste("Week", v), Treatment = trt,
          `LS Mean (SE)` = sprintf("%.2f (%.2f)", row$emmean, row$SE),
          `95% CI` = sprintf("(%.2f, %.2f)", row$lower.CL, row$upper.CL),
          `P-value` = ""
        )
      }
    }) |> dplyr::bind_rows()
    diff <- diffs |> dplyr::filter(AVISITN == v)
    pval <- pairs_df |> dplyr::filter(AVISITN == v)
    if (nrow(diff) > 0) {
      diff_row <- tibble::tibble(
        Visit = paste("Week", v), Treatment = "Difference",
        `LS Mean (SE)` = sprintf("%.2f (%.2f)", diff$estimate[1], diff$SE[1]),
        `95% CI` = sprintf("(%.2f, %.2f)", diff$lower.CL[1], diff$upper.CL[1]),
        `P-value` = sprintf("%.4f", pval$p.value[1])
      )
      dplyr::bind_rows(trt_rows, diff_row)
    } else trt_rows
  }) |> dplyr::bind_rows()

  # Build subtitle showing settings
  cov_info <- paste0("Covariance: ", covariance)
  covariates <- c()
  if (include_strata) covariates <- c(covariates, "Strata")
  if (include_region) covariates <- c(covariates, "Region")
  cov_text <- if (length(covariates) > 0) paste("Covariates:", paste(covariates, collapse = ", ")) else "No covariates"

  # Return GT table
  results |>
    gt::gt(groupname_col = "Visit") |>
    gt::tab_header(
      title = title,
      subtitle = paste(cov_info, "|", cov_text, "|", "Method:", method)
    ) |>
    gt::cols_align(align = "center", columns = c("LS Mean (SE)", "95% CI", "P-value")) |>
    gt::tab_style(style = gt::cell_text(weight = "bold"), locations = gt::cells_row_groups())
}


# Demographics as a function
demographics_table <- function(
    data,
    title = "Table 14.1.1",
    show_age = TRUE,
    show_sex = TRUE,
    show_race = TRUE
) {
  n_by_trt <- data |>
    dplyr::count(TRT01P) |>
    dplyr::rename(N = n)

  all_stats <- list()

  if (show_age) {
    age_stats <- data |>
      dplyr::group_by(TRT01P) |>
      dplyr::summarise(
        value = sprintf("%.1f (%.2f)", mean(AGE), stats::sd(AGE)),
        .groups = "drop"
      ) |>
      dplyr::mutate(variable = "Age (years)", category = "Mean (SD)") |>
      tidyr::pivot_wider(names_from = TRT01P, values_from = value)
    all_stats <- c(all_stats, list(age_stats))
  }

  if (show_sex) {
    sex_stats <- data |>
      dplyr::count(TRT01P, SEX) |>
      dplyr::left_join(n_by_trt, by = "TRT01P") |>
      dplyr::mutate(value = sprintf("%d (%.1f%%)", n, 100 * n / N)) |>
      dplyr::select(TRT01P, SEX, value) |>
      tidyr::pivot_wider(names_from = TRT01P, values_from = value) |>
      dplyr::rename(category = SEX) |>
      dplyr::mutate(variable = "Sex")
    all_stats <- c(all_stats, list(sex_stats))
  }

  if (show_race) {
    race_stats <- data |>
      dplyr::count(TRT01P, RACE) |>
      dplyr::left_join(n_by_trt, by = "TRT01P") |>
      dplyr::mutate(value = sprintf("%d (%.1f%%)", n, 100 * n / N)) |>
      dplyr::select(TRT01P, RACE, value) |>
      tidyr::pivot_wider(names_from = TRT01P, values_from = value) |>
      dplyr::rename(category = RACE) |>
      dplyr::mutate(variable = "Race")
    all_stats <- c(all_stats, list(race_stats))
  }

  table_data <- dplyr::bind_rows(all_stats) |>
    dplyr::select(variable, category, dplyr::everything())

  gt::gt(table_data, groupname_col = "variable") |>
    gt::tab_header(
      title = title,
      subtitle = "Demographics by Treatment Arm"
    ) |>
    gt::cols_label(category = "") |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_row_groups()
    )
}


# Run the app
run_app(
  blocks = c(
    # Data sources
    adsl = new_random_adsl_block(
      seed = 1, n_subjects = 100,
      block_name = "Subject Data (ADSL)"
    ),

    adqs = new_random_adam_block(
      dataset = "qs", seed = 1,
      block_name = "Questionnaire (ADQS)"
    ),

    filter = blockr.dplyr::new_filter_block(
      conditions = list(list(column = "PARAMCD", values = "FKSI-FWB", mode = "include")),
      block_name = "Filter FKSI-FWB"
    ),

    # Demographics using function_block
    demographics = new_function_block(
      fn = demographics_table,
      block_name = "Demographics"
    ),

    # MMRM using function_block - parameters exposed in UI!
    mmrm = new_function_block(
      fn = mmrm_analysis,
      block_name = "MMRM Analysis"
    )
  ),
  links = c(
    new_link("adsl", "demographics", "data"),
    new_link("adsl", "adqs", "data"),
    new_link("adqs", "filter", "data"),
    new_link("filter", "mmrm", "data")
  )
)
