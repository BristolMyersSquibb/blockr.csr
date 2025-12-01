# Vital Signs Analysis with LLM GT block

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

# GT code for vital signs descriptive statistics table
gt_code <- '
# Filter to systolic blood pressure, post-baseline visits
analysis_data <- data |>
dplyr::filter(
  !is.na(CHG),
  !AVISIT %in% c("BASELINE", "SCREENING")
)

# Calculate descriptive statistics by treatment and visit
stats <- analysis_data |>
dplyr::group_by(TRT01P, AVISIT, AVISITN) |>
dplyr::summarise(
  n = dplyr::n(),
  mean = mean(CHG, na.rm = TRUE),
  sd = sd(CHG, na.rm = TRUE),
  median = median(CHG, na.rm = TRUE),
  q1 = quantile(CHG, 0.25, na.rm = TRUE),
  q3 = quantile(CHG, 0.75, na.rm = TRUE),
  .groups = "drop"
) |>
dplyr::mutate(
  mean_sd = sprintf("%.1f (%.2f)", mean, sd),
  median_iqr = sprintf("%.1f (%.1f, %.1f)", median, q1, q3)
) |>
dplyr::arrange(AVISITN)

# Reshape for GT table
table_data <- stats |>
dplyr::select(AVISIT, TRT01P, n, mean_sd, median_iqr) |>
tidyr::pivot_wider(
  names_from = TRT01P,
  values_from = c(n, mean_sd, median_iqr),
  names_glue = "{TRT01P}_{.value}"
)

# Create GT table
gt::gt(table_data) |>
gt::tab_header(
  title = "Change from Baseline in Systolic Blood Pressure",
  subtitle = "By Visit and Treatment Arm"
) |>
gt::cols_label(
  AVISIT = "Visit"
) |>
gt::tab_spanner_delim(delim = "_")
'

run_app(
  blocks = c(
    adsl = new_random_adsl_block(
      seed = 1,
      n_subjects = 100
    ),
    advs = new_random_adam_block(
      dataset = "vs",
      seed = 1
    ),
    filter = blockr.dplyr::new_filter_block(
      conditions = list(
        list(column = "PARAMCD", values = "SYSBP", mode = "include")
      )
    ),
    gt_table = new_llm_gt_block(
      messages = "Create a vital signs summary table",
      code = gt_code
    )
  ),
  links = c(
    new_link("adsl", "advs", "data"),
    new_link("advs", "filter", "data"),
    new_link("filter", "gt_table", "data")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)
