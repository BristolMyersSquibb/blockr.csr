# Combined MMRM Workflow
# All analyses share the same subject data (ADSL)

library(blockr)
library(blockr.csr)
pkgload::load_all("../blockr.ai")

options(
  blockr.chat_function = list(
    "o4-mini" = function(system_prompt = NULL, params = NULL) {
      ellmer::chat_openai(
        model = "o4-mini",
        system_prompt = system_prompt,
        params = params
      )
    },
    "gpt-4o" = function(system_prompt = NULL, params = NULL) {
      ellmer::chat_openai(
        model = "gpt-4o",
        system_prompt = system_prompt,
        params = params
      )
    }
  )
)

# ============================================================================
# GT Code blocks (reused from individual workflows)
# ============================================================================

# Demographics table code
demographics_code <- '
n_by_trt <- data |>
  dplyr::count(TRT01P) |>
  dplyr::rename(N = n)

age_stats <- data |>
  dplyr::group_by(TRT01P) |>
  dplyr::summarise(
    value = sprintf("%.1f (%.2f)", mean(AGE), sd(AGE)),
    .groups = "drop"
  ) |>
  dplyr::mutate(variable = "Age (years)", category = "Mean (SD)") |>
  tidyr::pivot_wider(names_from = TRT01P, values_from = value)

sex_stats <- data |>
  dplyr::count(TRT01P, SEX) |>
  dplyr::left_join(n_by_trt, by = "TRT01P") |>
  dplyr::mutate(value = sprintf("%d (%.1f%%)", n, 100 * n / N)) |>
  dplyr::select(TRT01P, SEX, value) |>
  tidyr::pivot_wider(names_from = TRT01P, values_from = value) |>
  dplyr::rename(category = SEX) |>
  dplyr::mutate(variable = "Sex")

race_stats <- data |>
  dplyr::count(TRT01P, RACE) |>
  dplyr::left_join(n_by_trt, by = "TRT01P") |>
  dplyr::mutate(value = sprintf("%d (%.1f%%)", n, 100 * n / N)) |>
  dplyr::select(TRT01P, RACE, value) |>
  tidyr::pivot_wider(names_from = TRT01P, values_from = value) |>
  dplyr::rename(category = RACE) |>
  dplyr::mutate(variable = "Race")

table_data <- dplyr::bind_rows(age_stats, sex_stats, race_stats) |>
  dplyr::select(variable, category, dplyr::everything())

gt::gt(table_data, groupname_col = "variable") |>
  gt::tab_header(
    title = "Table 14.1.1",
    subtitle = "Demographics by Treatment Arm"
  ) |>
  gt::cols_label(category = "") |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups()
  )
'

# MMRM Analysis code
mmrm_analysis_code <- '
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

fit <- mmrm::mmrm(
  formula = CHG ~ TRTN + AVISITN + TRTN:AVISITN + BASE + STRATA1 + REGION1 +
    us(AVISITN | USUBJID),
  data = merged,
  method = "Kenward-Roger"
)

em <- emmeans::emmeans(fit, ~ TRTN | AVISITN)
lsm <- as.data.frame(em)
pairs_em <- pairs(em, reverse = TRUE)
diffs <- as.data.frame(confint(pairs_em))
pairs_df <- as.data.frame(summary(pairs_em))

visits <- sort(unique(as.numeric(as.character(lsm$AVISITN))))
trts <- levels(merged$TRTN)

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

results |>
  gt::gt(groupname_col = "Visit") |>
  gt::tab_header(title = "MMRM Analysis", subtitle = "Change from Baseline by Visit") |>
  gt::cols_align(align = "center", columns = c("LS Mean (SE)", "95% CI", "P-value")) |>
  gt::tab_style(style = gt::cell_text(weight = "bold"), locations = gt::cells_row_groups())
'

# Subgroup Analysis code
subgroup_code <- '
subgroups <- list(
  list(name = "Sex", var = "SEX"),
  list(name = "Region", var = "REGION1"),
  list(name = "Biomarker 2", var = "BMRKR2")
)

merged <- data |>
  dplyr::filter(!is.na(AVISIT), AVISIT != "", !is.na(CHG), !AVISIT %in% c("BASELINE", "SCREENING"))

last_visit <- max(as.numeric(as.character(merged$AVISITN)), na.rm = TRUE)

run_subgroup_mmrm <- function(data, subgroup_var, subgroup_val) {
  subdata <- data |>
    dplyr::filter(.data[[subgroup_var]] == subgroup_val) |>
    dplyr::mutate(
      TRTN = factor(TRT02P), AVISITN = factor(AVISITN), USUBJID = factor(USUBJID)
    )
  n_subjects <- dplyr::n_distinct(subdata$USUBJID)
  if (n_subjects < 10) return(list(success = FALSE))
  tryCatch({
    fit <- mmrm::mmrm(
      formula = CHG ~ TRTN + AVISITN + TRTN:AVISITN + BASE + us(AVISITN | USUBJID),
      data = subdata, method = "Kenward-Roger"
    )
    em <- emmeans::emmeans(fit, ~ TRTN | AVISITN)
    lsm <- as.data.frame(em) |> dplyr::filter(AVISITN == last_visit)
    pairs_em <- pairs(em, reverse = TRUE)
    diffs <- as.data.frame(confint(pairs_em)) |> dplyr::filter(AVISITN == last_visit)
    pairs_df <- as.data.frame(summary(pairs_em)) |> dplyr::filter(AVISITN == last_visit)
    list(success = TRUE, lsmeans = lsm, differences = diffs, pvalue = pairs_df$p.value[1], n_subjects = n_subjects)
  }, error = function(e) list(success = FALSE))
}

all_results <- list()
for (sg in subgroups) {
  subgroup_vals <- merged |>
    dplyr::filter(!is.na(.data[[sg$var]]), .data[[sg$var]] != "") |>
    dplyr::pull(sg$var) |> unique() |> sort()
  for (val in subgroup_vals) {
    result <- run_subgroup_mmrm(merged, sg$var, val)
    if (result$success) {
      trts <- levels(factor(merged$TRT02P))
      trt_lsm <- lapply(trts, function(trt) {
        row <- result$lsmeans |> dplyr::filter(TRTN == trt)
        if (nrow(row) > 0) sprintf("%.2f (%.2f)", row$emmean, row$SE) else NA
      })
      names(trt_lsm) <- paste0(trts, " LSM (SE)")
      row_data <- tibble::tibble(Subgroup = sg$name, Level = as.character(val), N = result$n_subjects)
      for (nm in names(trt_lsm)) row_data[[nm]] <- trt_lsm[[nm]]
      row_data$`Diff (SE)` <- sprintf("%.2f (%.2f)", result$differences$estimate[1], result$differences$SE[1])
      row_data$`P-value` <- sprintf("%.4f", result$pvalue)
      all_results[[length(all_results) + 1]] <- row_data
    }
  }
}

results_df <- dplyr::bind_rows(all_results)
results_df |>
  gt::gt(groupname_col = "Subgroup") |>
  gt::tab_header(title = "Subgroup Analysis", subtitle = paste("Week", last_visit)) |>
  gt::cols_align(align = "center", columns = dplyr::everything()) |>
  gt::cols_align(align = "left", columns = "Level") |>
  gt::tab_style(style = gt::cell_text(weight = "bold"), locations = gt::cells_row_groups())
'

# Biomarker Analysis code
biomarker_code <- '
mmrm_data <- data |>
  dplyr::filter(
    !is.na(AVISIT), AVISIT != "", !is.na(CHG),
    !AVISIT %in% c("BASELINE", "SCREENING"), !is.na(BMRKR2)
  ) |>
  dplyr::mutate(
    TRTN = factor(TRT02P), AVISITN = factor(AVISITN),
    USUBJID = factor(USUBJID), BMRKR2 = factor(BMRKR2)
  )

last_visit <- max(as.numeric(as.character(mmrm_data$AVISITN)), na.rm = TRUE)

fit <- mmrm::mmrm(
  formula = CHG ~ TRTN + AVISITN + TRTN:AVISITN + BASE +
    BMRKR2 + BMRKR2:TRTN + BMRKR2:AVISITN + BMRKR2:TRTN:AVISITN +
    us(AVISITN | USUBJID),
  data = mmrm_data, method = "Kenward-Roger"
)

em <- emmeans::emmeans(fit, ~ BMRKR2 * TRTN | AVISITN)
lsm <- as.data.frame(em) |> dplyr::filter(AVISITN == last_visit)
em_last <- emmeans::emmeans(fit, ~ BMRKR2 * TRTN, at = list(AVISITN = as.character(last_visit)))
pairs_em <- pairs(em_last, by = "BMRKR2", reverse = TRUE)
diffs <- as.data.frame(confint(pairs_em))
pairs_df <- as.data.frame(summary(pairs_em))

biomarker_levels <- levels(mmrm_data$BMRKR2)
trts <- levels(mmrm_data$TRTN)

results <- lapply(biomarker_levels, function(bm) {
  trt_rows <- lapply(trts, function(trt) {
    row <- lsm |> dplyr::filter(BMRKR2 == bm, TRTN == trt)
    if (nrow(row) > 0) {
      tibble::tibble(`Biomarker` = bm, Treatment = trt,
        `LS Mean (SE)` = sprintf("%.2f (%.2f)", row$emmean, row$SE),
        `95% CI` = sprintf("(%.2f, %.2f)", row$lower.CL, row$upper.CL), `P-value` = "")
    }
  }) |> dplyr::bind_rows()
  diff_row <- diffs |> dplyr::filter(BMRKR2 == bm)
  pval_row <- pairs_df |> dplyr::filter(BMRKR2 == bm)
  if (nrow(diff_row) > 0) {
    diff_tibble <- tibble::tibble(`Biomarker` = bm, Treatment = "Difference",
      `LS Mean (SE)` = sprintf("%.2f (%.2f)", diff_row$estimate[1], diff_row$SE[1]),
      `95% CI` = sprintf("(%.2f, %.2f)", diff_row$lower.CL[1], diff_row$upper.CL[1]),
      `P-value` = sprintf("%.4f", pval_row$p.value[1]))
    dplyr::bind_rows(trt_rows, diff_tibble)
  } else trt_rows
}) |> dplyr::bind_rows()

results |>
  gt::gt(groupname_col = "Biomarker") |>
  gt::tab_header(title = "Biomarker Interaction", subtitle = paste("Week", last_visit)) |>
  gt::cols_align(align = "center", columns = c("LS Mean (SE)", "95% CI", "P-value")) |>
  gt::tab_style(style = gt::cell_text(weight = "bold"), locations = gt::cells_row_groups())
'

# Propensity-weighted MMRM code
propensity_code <- '
merged <- data |>
  dplyr::filter(
    !is.na(AVISIT), AVISIT != "", !is.na(CHG),
    !AVISIT %in% c("BASELINE", "SCREENING"), !is.na(PSIPTW)
  ) |>
  dplyr::mutate(
    TRTN = factor(TRT02P), AVISITN = factor(AVISITN),
    USUBJID = factor(USUBJID), STRATA1 = factor(STRATA1), REGION1 = factor(REGION1)
  )

fit <- mmrm::mmrm(
  formula = CHG ~ TRTN + AVISITN + TRTN:AVISITN + BASE + STRATA1 + REGION1 +
    us(AVISITN | USUBJID),
  data = merged, weights = merged$PSIPTW, method = "Kenward-Roger"
)

em <- emmeans::emmeans(fit, ~ TRTN | AVISITN)
lsm <- as.data.frame(em)
pairs_em <- pairs(em, reverse = TRUE)
diffs <- as.data.frame(confint(pairs_em))
pairs_df <- as.data.frame(summary(pairs_em))

visits <- sort(unique(as.numeric(as.character(lsm$AVISITN))))
trts <- levels(merged$TRTN)

results <- lapply(visits, function(v) {
  trt_rows <- lapply(trts, function(trt) {
    row <- lsm |> dplyr::filter(AVISITN == v, TRTN == trt)
    if (nrow(row) > 0) {
      tibble::tibble(Visit = paste("Week", v), Treatment = trt,
        `LS Mean (SE)` = sprintf("%.2f (%.2f)", row$emmean, row$SE),
        `95% CI` = sprintf("(%.2f, %.2f)", row$lower.CL, row$upper.CL), `P-value` = "")
    }
  }) |> dplyr::bind_rows()
  diff <- diffs |> dplyr::filter(AVISITN == v)
  pval <- pairs_df |> dplyr::filter(AVISITN == v)
  if (nrow(diff) > 0) {
    diff_row <- tibble::tibble(Visit = paste("Week", v), Treatment = "Difference",
      `LS Mean (SE)` = sprintf("%.2f (%.2f)", diff$estimate[1], diff$SE[1]),
      `95% CI` = sprintf("(%.2f, %.2f)", diff$lower.CL[1], diff$upper.CL[1]),
      `P-value` = sprintf("%.4f", pval$p.value[1]))
    dplyr::bind_rows(trt_rows, diff_row)
  } else trt_rows
}) |> dplyr::bind_rows()

results |>
  gt::gt(groupname_col = "Visit") |>
  gt::tab_header(title = "MMRM (Propensity Weighted)", subtitle = "Change from Baseline") |>
  gt::cols_align(align = "center", columns = c("LS Mean (SE)", "95% CI", "P-value")) |>
  gt::tab_style(style = gt::cell_text(weight = "bold"), locations = gt::cells_row_groups())
'

# Descriptive Statistics code
descriptive_code <- '
treatment_var <- "TRT02P"
analysis_data <- data |>
  dplyr::filter(!is.na(AVISITN)) |>
  dplyr::mutate(TRT = .data[[treatment_var]])

total_data <- analysis_data |> dplyr::mutate(TRT = "Total")
all_data <- dplyr::bind_rows(analysis_data, total_data)
trt_levels <- c(sort(as.character(unique(analysis_data$TRT))), "Total")
all_data$TRT <- factor(all_data$TRT, levels = trt_levels)

baseline_stats <- all_data |>
  dplyr::filter(AVISIT == "BASELINE" | AVISITN == 0) |>
  dplyr::filter(!is.na(AVAL)) |>
  dplyr::group_by(AVISITN, TRT) |>
  dplyr::summarise(n = dplyr::n(), mean = mean(AVAL), sd = stats::sd(AVAL), .groups = "drop") |>
  dplyr::mutate(section = "Baseline", visit_label = "Baseline")

post_data <- all_data |> dplyr::filter(AVISIT != "BASELINE", AVISITN > 0)

change_stats <- post_data |>
  dplyr::filter(!is.na(CHG)) |>
  dplyr::group_by(AVISITN, TRT) |>
  dplyr::summarise(n = dplyr::n(), mean = mean(CHG), sd = stats::sd(CHG), .groups = "drop") |>
  dplyr::mutate(section = "Change", visit_label = paste("Week", AVISITN))

all_stats <- dplyr::bind_rows(baseline_stats, change_stats) |>
  dplyr::mutate(stat = sprintf("%.1f (%.2f)", mean, sd))

stats_wide <- all_stats |>
  dplyr::select(visit_label, section, TRT, n, stat) |>
  tidyr::pivot_wider(names_from = TRT, values_from = c(n, stat), names_glue = "{TRT}_{.value}")

stats_wide |>
  dplyr::mutate(row_group = paste(visit_label, "-", section)) |>
  dplyr::select(-visit_label, -section) |>
  gt::gt(groupname_col = "row_group") |>
  gt::tab_header(title = "Descriptive Statistics", subtitle = "ADQS Summary") |>
  gt::tab_spanner_delim(delim = "_") |>
  gt::tab_style(style = gt::cell_text(weight = "bold"), locations = gt::cells_row_groups())
'

# Vital Signs code
vital_signs_code <- '
analysis_data <- data |>
  dplyr::filter(!is.na(CHG), !AVISIT %in% c("BASELINE", "SCREENING"))

stats <- analysis_data |>
  dplyr::group_by(TRT01P, AVISIT, AVISITN) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(CHG, na.rm = TRUE),
    sd = sd(CHG, na.rm = TRUE),
    median = median(CHG, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    mean_sd = sprintf("%.1f (%.2f)", mean, sd)
  ) |>
  dplyr::arrange(AVISITN)

table_data <- stats |>
  dplyr::select(AVISIT, TRT01P, n, mean_sd) |>
  tidyr::pivot_wider(
    names_from = TRT01P,
    values_from = c(n, mean_sd),
    names_glue = "{TRT01P}_{.value}"
  )

gt::gt(table_data) |>
  gt::tab_header(
    title = "Vital Signs Summary",
    subtitle = "Change from Baseline in Systolic BP"
  ) |>
  gt::cols_label(AVISIT = "Visit") |>
  gt::tab_spanner_delim(delim = "_")
'

# ============================================================================
# Combined Workflow
# ============================================================================

run_app(
  blocks = c(
    # Shared subject data
    adsl = new_random_adsl_block(
      seed = 1, n_subjects = 100,
      block_name = "Subject Data (ADSL)"
    ),

    # Branch 1: Demographics (direct from ADSL)
    demographics = new_llm_gt_block(
      messages = "Demographics table",
      code = demographics_code,
      block_name = "Demographics Table"
    ),

    # Branch 2: ADQS for questionnaire analyses
    adqs = new_random_adam_block(
      dataset = "qs", seed = 1,
      block_name = "Questionnaire (ADQS)"
    ),
    filter = blockr.dplyr::new_filter_block(
      conditions = list(list(column = "PARAMCD", values = "FKSI-FWB", mode = "include")),
      block_name = "Filter FKSI-FWB"
    ),
    mmrm_analysis = new_llm_gt_block(
      messages = "MMRM analysis",
      code = mmrm_analysis_code,
      block_name = "MMRM Analysis"
    ),
    subgroup = new_llm_gt_block(
      messages = "Subgroup analysis",
      code = subgroup_code,
      block_name = "Subgroup Analysis"
    ),
    biomarker = new_llm_gt_block(
      messages = "Biomarker interaction analysis",
      code = biomarker_code,
      block_name = "Biomarker Analysis"
    ),
    descriptive = new_llm_gt_block(
      messages = "Descriptive statistics",
      code = descriptive_code,
      block_name = "Descriptive Stats"
    ),

    # Propensity branch (needs propensity weights)
    propensity = blockr.dplyr::new_mutate_expr_block(
      exprs = list(
        PSIPTW = "dplyr::case_when(TRT02P == 'A: Drug X' ~ runif(dplyr::n(), 0.7, 1.3), TRT02P == 'B: Placebo' ~ runif(dplyr::n(), 0.8, 1.2), TRT02P == 'C: Combination' ~ runif(dplyr::n(), 0.6, 1.4), TRUE ~ 1)"
      ),
      block_name = "Add Propensity Weights"
    ),
    mmrm_propensity = new_llm_gt_block(
      messages = "MMRM with propensity weights",
      code = propensity_code,
      block_name = "MMRM Propensity Weighted"
    ),

    # Branch 3: ADVS for vital signs
    advs = new_random_adam_block(
      dataset = "vs", seed = 1,
      block_name = "Vital Signs (ADVS)"
    ),
    advs_filter = blockr.dplyr::new_filter_block(
      conditions = list(list(column = "PARAMCD", values = "SYSBP", mode = "include")),
      block_name = "Filter Systolic BP"
    ),
    vital_signs = new_llm_gt_block(
      messages = "Vital signs summary",
      code = vital_signs_code,
      block_name = "Vital Signs Summary"
    )
  ),
  links = c(
    # Demographics from ADSL
    new_link("adsl", "demographics", "data"),

    # ADQS branch - main analyses
    new_link("adsl", "adqs", "data"),
    new_link("adqs", "filter", "data"),
    new_link("filter", "mmrm_analysis", "data"),
    new_link("filter", "subgroup", "data"),
    new_link("filter", "biomarker", "data"),
    new_link("filter", "descriptive", "data"),

    # Propensity weighted branch
    new_link("filter", "propensity", "data"),
    new_link("propensity", "mmrm_propensity", "data"),

    # ADVS branch
    new_link("adsl", "advs", "data"),
    new_link("advs", "advs_filter", "data"),
    new_link("advs_filter", "vital_signs", "data")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)
