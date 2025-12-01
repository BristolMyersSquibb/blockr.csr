# MMRM Subgroup Analysis Table (adapted from mmrm_subgroup)
# Uses two datasets: ADSL and ADQS from random.cdisc.data

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

# Adapted from mmrm_subgroup function
gt_code <- '
# data is ADQS with ADSL columns already merged

# Define subgroups to analyze
subgroups <- list(
  list(name = "Sex", var = "SEX"),
  list(name = "Race", var = "RACE"),
  list(name = "Region", var = "REGION1"),
  list(name = "Strata 1", var = "STRATA1"),
  list(name = "Strata 2", var = "STRATA2"),
  list(name = "Biomarker 2", var = "BMRKR2")
)

# Prepare merged data
merged <- data |>
  dplyr::filter(
    !is.na(AVISIT),
    AVISIT != "",
    !is.na(CHG),
    AVISIT != "BASELINE"
  )

# Get the last visit for primary analysis
last_visit <- max(as.numeric(as.character(merged$AVISITN)), na.rm = TRUE)

# Helper function to run MMRM for a subgroup
run_subgroup_mmrm <- function(data, subgroup_var, subgroup_val) {
  subdata <- data |>
    dplyr::filter(.data[[subgroup_var]] == subgroup_val) |>
    dplyr::mutate(
      TRTN = factor(TRT02P),
      AVISITN = factor(AVISITN),
      USUBJID = factor(USUBJID)
    )

  n_subjects <- dplyr::n_distinct(subdata$USUBJID)
  if (n_subjects < 10) {
    return(list(success = FALSE, reason = paste("Too few subjects:", n_subjects)))
  }

  tryCatch({
    fit <- mmrm::mmrm(
      formula = CHG ~ TRTN + AVISITN + TRTN:AVISITN + BASE +
        us(AVISITN | USUBJID),
      data = subdata,
      method = "Kenward-Roger"
    )

    em <- emmeans::emmeans(fit, ~ TRTN | AVISITN)
    lsm <- as.data.frame(em) |>
      dplyr::filter(AVISITN == last_visit)

    pairs_em <- pairs(em, reverse = TRUE)
    diffs <- as.data.frame(confint(pairs_em)) |>
      dplyr::filter(AVISITN == last_visit)
    pairs_df <- as.data.frame(summary(pairs_em)) |>
      dplyr::filter(AVISITN == last_visit)

    list(
      success = TRUE,
      lsmeans = lsm,
      differences = diffs,
      pvalue = pairs_df$p.value[1],
      n_subjects = n_subjects
    )
  }, error = function(e) {
    list(success = FALSE, reason = as.character(e))
  })
}

# Run analysis for each subgroup
all_results <- list()

for (sg in subgroups) {
  subgroup_vals <- merged |>
    dplyr::filter(!is.na(.data[[sg$var]]), .data[[sg$var]] != "") |>
    dplyr::pull(sg$var) |>
    unique() |>
    sort()

  for (val in subgroup_vals) {
    result <- run_subgroup_mmrm(merged, sg$var, val)

    if (result$success) {
      trts <- levels(factor(merged$TRT02P))

      trt_lsm <- lapply(trts, function(trt) {
        row <- result$lsmeans |> dplyr::filter(TRTN == trt)
        if (nrow(row) > 0) sprintf("%.2f (%.2f)", row$emmean, row$SE) else NA
      })
      names(trt_lsm) <- paste0(trts, " LSM (SE)")

      row_data <- tibble::tibble(
        Subgroup = sg$name,
        Level = as.character(val),
        N = result$n_subjects
      )

      for (nm in names(trt_lsm)) {
        row_data[[nm]] <- trt_lsm[[nm]]
      }

      row_data$`Diff (SE)` <- sprintf("%.2f (%.2f)",
        result$differences$estimate[1], result$differences$SE[1])
      row_data$`95% CI` <- sprintf("(%.2f, %.2f)",
        result$differences$lower.CL[1], result$differences$upper.CL[1])
      row_data$`P-value` <- sprintf("%.4f", result$pvalue)

      all_results[[length(all_results) + 1]] <- row_data
    }
  }
}

results_df <- dplyr::bind_rows(all_results)

# Build GT table
results_df |>
  gt::gt(groupname_col = "Subgroup") |>
  gt::tab_header(
    title = "Table 14.2.4.1.1",
    subtitle = paste("Change from Baseline at Week", last_visit, "by Subgroup - MMRM Analysis")
  ) |>
  gt::cols_align(align = "center", columns = dplyr::everything()) |>
  gt::cols_align(align = "left", columns = "Level") |>
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
    gt_table = new_llm_gt_block(
      messages = "Run MMRM subgroup analysis",
      code = gt_code
    )
  ),
  links = c(
    new_link("adsl", "adqs", "data"),
    new_link("adqs", "filter", "data"),
    new_link("filter", "gt_table", "data")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)
