# Function Var Block Demo
# Demonstrates new_function_var_block that accepts variadic data inputs (...)
# and auto-generates UI from function arguments.
# Output type is auto-detected: gt_tbl objects render as GT, everything else as DataTable.

library(blockr)
library(blockr.csr)
pkgload::load_all()

# Example 1: Simple bind_rows with ID column
bind_with_id <- function(..., .id = "source") {
  dplyr::bind_rows(..., .id = .id)
}

# Example 2: Custom reduce function for sequential merging
reduce_merge <- function(..., by = "name", all = TRUE) {
  dfs <- list(...)
  if (length(dfs) == 1) return(dfs[[1]])
  Reduce(function(x, y) merge(x, y, by = by, all = all), dfs)
}

# Example 3: Summary across multiple datasets returning GT
summarize_datasets <- function(..., title = "Dataset Summary") {
  dfs <- list(...)
  summary_data <- data.frame(
    Dataset = seq_along(dfs),
    Rows = vapply(dfs, nrow, integer(1)),
    Cols = vapply(dfs, ncol, integer(1))
  )
  summary_data$Total_Rows <- sum(summary_data$Rows)

  gt::gt(summary_data) |>
    gt::tab_header(title = title) |>
    gt::cols_label(
      Dataset = "Dataset #",
      Rows = "Row Count",
      Cols = "Column Count",
      Total_Rows = "Combined Total"
    )
}

# Example 4: Intersect columns across all datasets
common_columns <- function(...) {
  dfs <- list(...)
  common_cols <- Reduce(intersect, lapply(dfs, names))

  # Keep only common columns from first dataset
  result <- dfs[[1]][, common_cols, drop = FALSE]

  # Bind all datasets with common columns
  for (i in seq_along(dfs)[-1]) {
    result <- rbind(result, dfs[[i]][, common_cols, drop = FALSE])
  }
  result
}

run_app(
  blocks = c(
    # Data sources - create three filtered subsets of iris
    setosa = blockr.dplyr::new_filter_expr_block(
      exprs = list("Species == 'setosa'"),
      block_name = "Setosa"
    ),

    versicolor = blockr.dplyr::new_filter_expr_block(
      exprs = list("Species == 'versicolor'"),
      block_name = "Versicolor"
    ),

    virginica = blockr.dplyr::new_filter_expr_block(
      exprs = list("Species == 'virginica'"),
      block_name = "Virginica"
    ),

    iris_data = blockr.core::new_dataset_block(
      dataset = "iris",
      package = "datasets",
      block_name = "Iris Data"
    ),

    # Bind all three with source ID (returns data.frame -> auto DataTable)
    bound = new_function_var_block(
      fn = bind_with_id,
      block_name = "Bind with ID"
    ),

    # Summary table (returns gt_tbl -> auto GT)
    summary = new_function_var_block(
      fn = summarize_datasets,
      block_name = "Summary Table"
    )
  ),
  links = c(
    # Connect iris to all three filter blocks
    new_link("iris_data", "setosa", "data"),
    new_link("iris_data", "versicolor", "data"),
    new_link("iris_data", "virginica", "data"),

    # Bind block: connect all three filtered datasets
    new_link("setosa", "bound", "1"),
    new_link("versicolor", "bound", "2"),
    new_link("virginica", "bound", "3"),

    # Summary block: connect all three filtered datasets
    new_link("setosa", "summary", "1"),
    new_link("versicolor", "summary", "2"),
    new_link("virginica", "summary", "3")
  )
)
