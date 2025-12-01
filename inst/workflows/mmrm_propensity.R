# MMRM Analysis with Propensity Score Weights (adapted from mmrm_propensity)
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

# Adapted from mmrm_propensity function
# Uses propensity weights (PSIPTW) added via mutate block
gt_code <- '
# data is ADQS with ADSL columns and PSIPTW already merged
merged <- data |>
  dplyr::filter(
    !is.na(AVISIT),
    AVISIT != "",
    !is.na(CHG),
    !AVISIT %in% c("BASELINE", "SCREENING"),
    !is.na(PSIPTW)
  ) |>
  dplyr::mutate(
    TRTN = factor(TRT02P),
    AVISITN = factor(AVISITN),
    USUBJID = factor(USUBJID),
    STRATA1 = factor(STRATA1),
    REGION1 = factor(REGION1)
  )

# Fit MMRM model with propensity weights
fit <- mmrm::mmrm(
  formula = CHG ~ TRTN + AVISITN + TRTN:AVISITN + BASE + STRATA1 + REGION1 +
    us(AVISITN | USUBJID),
  data = merged,
  weights = merged$PSIPTW,
  method = "Kenward-Roger"
)

# Extract LS means
em <- emmeans::emmeans(fit, ~ TRTN | AVISITN)
lsm <- as.data.frame(em)

# Get differences between treatments
pairs_em <- pairs(em, reverse = TRUE)
diffs <- as.data.frame(confint(pairs_em))
pairs_df <- as.data.frame(summary(pairs_em))

# Get unique visits
visits <- sort(unique(as.numeric(as.character(lsm$AVISITN))))
trts <- levels(merged$TRTN)

# Format results for table
results <- lapply(visits, function(v) {
  trt_rows <- lapply(trts, function(trt) {
    row <- lsm |> dplyr::filter(AVISITN == v, TRTN == trt)
    if (nrow(row) > 0) {
      tibble::tibble(
        Visit = paste("Week", v),
        Treatment = trt,
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
      Visit = paste("Week", v),
      Treatment = "Difference",
      `LS Mean (SE)` = sprintf("%.2f (%.2f)", diff$estimate[1], diff$SE[1]),
      `95% CI` = sprintf("(%.2f, %.2f)", diff$lower.CL[1], diff$upper.CL[1]),
      `P-value` = sprintf("%.4f", pval$p.value[1])
    )
    dplyr::bind_rows(trt_rows, diff_row)
  } else {
    trt_rows
  }
}) |> dplyr::bind_rows()

# Build GT table
results |>
  gt::gt(groupname_col = "Visit") |>
  gt::tab_header(
    title = "Table 14.2.1.5.1",
    subtitle = "Change from Baseline by Visit - MMRM Analysis (Propensity Score Weighted)"
  ) |>
  gt::tab_source_note(
    source_note = "Model: CHG = TRTN + AVISITN + TRTN*AVISITN + BASE + STRATA1 + REGION1 with unstructured covariance and propensity weights. Kenward-Roger DF."
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
      messages = "Run MMRM analysis with propensity score weights",
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
