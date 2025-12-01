# MMRM Biomarker Interaction with Propensity Weights (adapted from mmrm_biomarker_propensity)
# Uses random.cdisc.data with simulated propensity weights

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

# Adapted from mmrm_biomarker_propensity function
# Uses BMRKR2 as biomarker and PSIPTW propensity weights
gt_code <- '
# data is ADQS with ADSL columns and PSIPTW already merged

# Prepare data - use BMRKR2 as biomarker
mmrm_data <- data |>
  dplyr::filter(
    !is.na(AVISIT),
    AVISIT != "",
    !is.na(CHG),
    !AVISIT %in% c("BASELINE", "SCREENING"),
    !is.na(BMRKR2),
    !is.na(PSIPTW)
  ) |>
  dplyr::mutate(
    TRTN = factor(TRT02P),
    AVISITN = factor(AVISITN),
    USUBJID = factor(USUBJID),
    BMRKR2 = factor(BMRKR2)
  )

# Get the last visit
last_visit <- max(as.numeric(as.character(mmrm_data$AVISITN)), na.rm = TRUE)

# Fit MMRM with full biomarker interaction and propensity weights
fit <- mmrm::mmrm(
  formula = CHG ~ TRTN + AVISITN + TRTN:AVISITN + BASE +
    BMRKR2 + BMRKR2:TRTN + BMRKR2:AVISITN + BMRKR2:TRTN:AVISITN +
    us(AVISITN | USUBJID),
  data = mmrm_data,
  weights = mmrm_data$PSIPTW,
  method = "Kenward-Roger"
)

# Extract LS means at last visit
em <- emmeans::emmeans(fit, ~ BMRKR2 * TRTN | AVISITN)
lsm <- as.data.frame(em) |>
  dplyr::filter(AVISITN == last_visit)

# Get differences by biomarker level
em_last <- emmeans::emmeans(fit, ~ BMRKR2 * TRTN,
  at = list(AVISITN = as.character(last_visit)))
pairs_em <- pairs(em_last, by = "BMRKR2", reverse = TRUE)
diffs <- as.data.frame(confint(pairs_em))
pairs_df <- as.data.frame(summary(pairs_em))

# Format results
biomarker_levels <- levels(mmrm_data$BMRKR2)
trts <- levels(mmrm_data$TRTN)

results <- lapply(biomarker_levels, function(bm) {
  trt_rows <- lapply(trts, function(trt) {
    row <- lsm |> dplyr::filter(BMRKR2 == bm, TRTN == trt)
    if (nrow(row) > 0) {
      tibble::tibble(
        `Biomarker Status` = bm,
        Treatment = trt,
        `LS Mean (SE)` = sprintf("%.2f (%.2f)", row$emmean, row$SE),
        `95% CI` = sprintf("(%.2f, %.2f)", row$lower.CL, row$upper.CL),
        `P-value` = ""
      )
    }
  }) |> dplyr::bind_rows()

  diff_row <- diffs |> dplyr::filter(BMRKR2 == bm)
  pval_row <- pairs_df |> dplyr::filter(BMRKR2 == bm)

  if (nrow(diff_row) > 0) {
    diff_tibble <- tibble::tibble(
      `Biomarker Status` = bm,
      Treatment = "Difference",
      `LS Mean (SE)` = sprintf("%.2f (%.2f)", diff_row$estimate[1], diff_row$SE[1]),
      `95% CI` = sprintf("(%.2f, %.2f)", diff_row$lower.CL[1], diff_row$upper.CL[1]),
      `P-value` = sprintf("%.4f", pval_row$p.value[1])
    )
    dplyr::bind_rows(trt_rows, diff_tibble)
  } else {
    trt_rows
  }
}) |> dplyr::bind_rows()

# Build GT table
results |>
  gt::gt(groupname_col = "Biomarker Status") |>
  gt::tab_header(
    title = "Table 14.2.4.3.1",
    subtitle = paste("Change from Baseline at Week", last_visit, "by Biomarker Status - MMRM Analysis (Propensity Weighted)")
  ) |>
  gt::tab_source_note(
    source_note = "Model: CHG = TRTN + AVISITN + TRTN*AVISITN + BASE + BMRKR2 + BMRKR2*TRTN + BMRKR2*AVISITN + BMRKR2*TRTN*AVISITN. Unstructured covariance with propensity weights, Kenward-Roger DF."
  ) |>
  gt::cols_align(align = "center", columns = c("LS Mean (SE)", "95% CI", "P-value")) |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups()
  )
'

run_app(
  blocks = c(
    adsl = new_random_adsl_block(
      seed = 1,
      n_subjects = 100
    ),
    adqs = new_random_adam_block(
      dataset = "qs",
      seed = 1
    ),
    filter = blockr.dplyr::new_filter_block(
      conditions = list(
        list(column = "PARAMCD", values = "FKSI-FWB", mode = "include")
      )
    ),
    propensity = blockr.dplyr::new_mutate_expr_block(
      exprs = list(
        PSIPTW = "dplyr::case_when(
          TRT02P == 'A: Drug X' ~ runif(dplyr::n(), 0.7, 1.3),
          TRT02P == 'B: Placebo' ~ runif(dplyr::n(), 0.8, 1.2),
          TRT02P == 'C: Combination' ~ runif(dplyr::n(), 0.6, 1.4),
          TRUE ~ 1
        )"
      )
    ),
    gt_table = new_llm_gt_block(
      messages = "Run MMRM biomarker interaction analysis with propensity weights",
      code = gt_code
    )
  ),
  links = c(
    new_link("adsl", "adqs", "data"),
    new_link("adqs", "filter", "data"),
    new_link("filter", "propensity", "data"),
    new_link("propensity", "gt_table", "data")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)
