# Function Block Demo
# Demonstrates new_function_block that auto-generates UI from function arguments.
# Output type is auto-detected: gt_tbl objects render as GT, everything else as DataTable.

library(blockr)
library(blockr.csr)

# Example 1: Simple filter function (returns data.frame -> renders as DataTable)
filter_species <- function(data, species = c("setosa", "versicolor", "virginica")) {
  dplyr::filter(data, Species == species)
}

# Example 2: Function that returns a GT table (auto-detected -> renders as GT)
make_summary_table <- function(
    data,
    title = "Iris Summary",
    decimals = 2L) {
  summary_data <- data |>
    dplyr::group_by(Species) |>
    dplyr::summarize(
      N = dplyr::n(),
      Mean_Sepal_Length = mean(Sepal.Length),
      Mean_Petal_Length = mean(Petal.Length),
      .groups = "drop"
    )

  gt::gt(summary_data) |>
    gt::tab_header(title = title) |>
    gt::fmt_number(columns = c("Mean_Sepal_Length", "Mean_Petal_Length"), decimals = decimals)
}

# Example 3: Multi-param transform (returns data.frame -> renders as DataTable)
transform_data <- function(
    data,
    species = c("setosa", "versicolor", "virginica"),
    min_sepal_length = 5.0,
    max_rows = 20L) {
  data |>
    dplyr::filter(
      Species == species,
      Sepal.Length >= min_sepal_length
    ) |>
    utils::head(max_rows)
}

run_app(
  blocks = c(
    # Data source
    data = blockr.core::new_dataset_block(
      dataset = "iris",
      package = "datasets",
      block_name = "Iris Data"
    ),

    # Returns data.frame -> auto DataTable
    fn_filter = new_function_block(
      fn = filter_species,
      block_name = "Filter Species"
    ),

    # Returns gt_tbl -> auto GT
    fn_summary = new_function_block(
      fn = make_summary_table,
      block_name = "Summary Table"
    ),

    # Returns data.frame -> auto DataTable
    fn_transform = new_function_block(
      fn = transform_data,
      block_name = "Transform Data"
    )
  ),
  links = c(
    new_link("data", "fn_filter", "data"),
    new_link("data", "fn_summary", "data"),
    new_link("data", "fn_transform", "data")
  )
)
