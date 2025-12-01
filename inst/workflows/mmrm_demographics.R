# Demographics Table

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

# GT code for demographics summary table
gt_code <- '
# Calculate counts per treatment arm
n_by_trt <- data |>
  dplyr::count(TRT01P) |>
  dplyr::rename(N = n)

# Continuous variables: Age
age_stats <- data |>
  dplyr::group_by(TRT01P) |>
  dplyr::summarise(
    value = sprintf("%.1f (%.2f)", mean(AGE), sd(AGE)),
    .groups = "drop"
  ) |>
  dplyr::mutate(variable = "Age (years)", category = "Mean (SD)") |>
  tidyr::pivot_wider(names_from = TRT01P, values_from = value)

# Categorical: Sex
sex_stats <- data |>
  dplyr::count(TRT01P, SEX) |>
  dplyr::left_join(n_by_trt, by = "TRT01P") |>
  dplyr::mutate(
    value = sprintf("%d (%.1f%%)", n, 100 * n / N)
  ) |>
  dplyr::select(TRT01P, SEX, value) |>
  tidyr::pivot_wider(names_from = TRT01P, values_from = value) |>
  dplyr::rename(category = SEX) |>
  dplyr::mutate(variable = "Sex")

# Categorical: Race
race_stats <- data |>
  dplyr::count(TRT01P, RACE) |>
  dplyr::left_join(n_by_trt, by = "TRT01P") |>
  dplyr::mutate(
    value = sprintf("%d (%.1f%%)", n, 100 * n / N)
  ) |>
  dplyr::select(TRT01P, RACE, value) |>
  tidyr::pivot_wider(names_from = TRT01P, values_from = value) |>
  dplyr::rename(category = RACE) |>
  dplyr::mutate(variable = "Race")

# Categorical: Ethnicity
ethnic_stats <- data |>
  dplyr::count(TRT01P, ETHNIC) |>
  dplyr::left_join(n_by_trt, by = "TRT01P") |>
  dplyr::mutate(
    value = sprintf("%d (%.1f%%)", n, 100 * n / N)
  ) |>
  dplyr::select(TRT01P, ETHNIC, value) |>
  tidyr::pivot_wider(names_from = TRT01P, values_from = value) |>
  dplyr::rename(category = ETHNIC) |>
  dplyr::mutate(variable = "Ethnicity")

# Combine all
table_data <- dplyr::bind_rows(age_stats, sex_stats, race_stats, ethnic_stats) |>
  dplyr::select(variable, category, dplyr::everything())

# Create GT table
gt::gt(table_data, groupname_col = "variable") |>
  gt::tab_header(
    title = "Table 14.1.1",
    subtitle = "Demographics and Baseline Characteristics by Treatment Arm"
  ) |>
  gt::cols_label(category = "") |>
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
    gt_table = new_llm_gt_block(
      messages = "Create a demographics summary table",
      code = gt_code
    )
  ),
  links = c(
    new_link("adsl", "gt_table", "data")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)
