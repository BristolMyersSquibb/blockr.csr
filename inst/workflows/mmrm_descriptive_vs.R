# Descriptive Statistics Table (adapted from mmrm_descriptive)

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

# Adapted from mmrm_descriptive function
gt_code <- '
# Create treatment groups including Total
total_data <- data |>
  dplyr::mutate(TRT01P = "Total")

all_data <- dplyr::bind_rows(data, total_data)

# Get treatment levels
trt_levels <- c(sort(as.character(unique(data$TRT01P))), "Total")
all_data$TRT01P <- factor(all_data$TRT01P, levels = trt_levels)

# Calculate baseline stats
baseline_stats <- all_data |>
  dplyr::filter(AVISIT == "BASELINE" | AVISITN == 0) |>
  dplyr::filter(!is.na(AVAL)) |>
  dplyr::group_by(AVISITN, TRT01P) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(AVAL, na.rm = TRUE),
    sd = sd(AVAL, na.rm = TRUE),
    median = median(AVAL, na.rm = TRUE),
    q1 = quantile(AVAL, 0.25, na.rm = TRUE),
    q3 = quantile(AVAL, 0.75, na.rm = TRUE),
    min = min(AVAL, na.rm = TRUE),
    max = max(AVAL, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(section = "Baseline", visit_label = "Baseline")

# Calculate post-baseline stats
post_data <- all_data |>
  dplyr::filter(!AVISIT %in% c("BASELINE", "SCREENING"), AVISITN > 0)

# Value stats
value_stats <- post_data |>
  dplyr::filter(!is.na(AVAL)) |>
  dplyr::group_by(AVISITN, TRT01P, AVISIT) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(AVAL, na.rm = TRUE),
    sd = sd(AVAL, na.rm = TRUE),
    median = median(AVAL, na.rm = TRUE),
    q1 = quantile(AVAL, 0.25, na.rm = TRUE),
    q3 = quantile(AVAL, 0.75, na.rm = TRUE),
    min = min(AVAL, na.rm = TRUE),
    max = max(AVAL, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(section = "Value", visit_label = paste("Week", AVISITN))

# Change stats
change_stats <- post_data |>
  dplyr::filter(!is.na(CHG)) |>
  dplyr::group_by(AVISITN, TRT01P, AVISIT) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(CHG, na.rm = TRUE),
    sd = sd(CHG, na.rm = TRUE),
    median = median(CHG, na.rm = TRUE),
    q1 = quantile(CHG, 0.25, na.rm = TRUE),
    q3 = quantile(CHG, 0.75, na.rm = TRUE),
    min = min(CHG, na.rm = TRUE),
    max = max(CHG, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(section = "Change from Baseline", visit_label = paste("Week", AVISITN))

# Combine all stats
all_stats <- dplyr::bind_rows(baseline_stats, value_stats, change_stats)

# Format statistics
formatted <- all_stats |>
  dplyr::mutate(
    n_fmt = sprintf("%d", n),
    mean_fmt = sprintf("%.1f", mean),
    sd_fmt = sprintf("%.2f", sd),
    median_fmt = sprintf("%.1f", median),
    q1_q3_fmt = sprintf("%.1f, %.1f", q1, q3),
    min_max_fmt = sprintf("%.0f, %.0f", min, max)
  )

# Reshape for display
stats_long <- formatted |>
  dplyr::select(AVISITN, visit_label, section, TRT01P,
                n_fmt, mean_fmt, sd_fmt, median_fmt,
                q1_q3_fmt, min_max_fmt) |>
  tidyr::pivot_longer(
    cols = c(n_fmt, mean_fmt, sd_fmt, median_fmt, q1_q3_fmt, min_max_fmt),
    names_to = "stat",
    values_to = "value"
  ) |>
  dplyr::mutate(
    stat_label = dplyr::case_when(
      stat == "n_fmt" ~ "n",
      stat == "mean_fmt" ~ "Mean",
      stat == "sd_fmt" ~ "SD",
      stat == "median_fmt" ~ "Median",
      stat == "q1_q3_fmt" ~ "Q1, Q3",
      stat == "min_max_fmt" ~ "Min, Max"
    ),
    stat_order = dplyr::case_when(
      stat == "n_fmt" ~ 1,
      stat == "mean_fmt" ~ 2,
      stat == "sd_fmt" ~ 3,
      stat == "median_fmt" ~ 4,
      stat == "q1_q3_fmt" ~ 5,
      stat == "min_max_fmt" ~ 6
    )
  )

# Pivot wider by treatment
stats_wide <- stats_long |>
  tidyr::pivot_wider(
    id_cols = c(AVISITN, visit_label, section, stat, stat_label, stat_order),
    names_from = TRT01P,
    values_from = value
  ) |>
  dplyr::arrange(AVISITN, dplyr::desc(section == "Baseline"),
                 dplyr::desc(section == "Value"), stat_order)

# Build GT table
stats_wide |>
  dplyr::mutate(
    row_group = paste(visit_label, "-", section)
  ) |>
  dplyr::select(-AVISITN, -stat, -stat_order, -visit_label, -section) |>
  gt::gt(groupname_col = "row_group") |>
  gt::tab_header(
    title = "Table 14.2.1.1.1",
    subtitle = "Actual Value and Change from Baseline in Systolic BP by Visit - Summary"
  ) |>
  gt::cols_label(stat_label = "Statistics") |>
  gt::cols_align(align = "center", columns = -stat_label) |>
  gt::cols_align(align = "left", columns = stat_label) |>
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
      messages = "Create a descriptive statistics table",
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
