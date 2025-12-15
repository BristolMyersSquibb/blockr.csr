# Function XY Block Demo
# Demonstrates new_function_xy_block that accepts two data inputs (x and y)
# and auto-generates UI from function arguments.
# Output type is auto-detected: gt_tbl objects render as GT, everything else as DataTable.

library(blockr)
library(blockr.csr)
pkgload::load_all()

# Example 1: Simple left join function
join_data <- function(x, y) {
  dplyr::left_join(x, y, by = "name")
}

# Example 2: Merge with configurable parameters
merge_with_options <- function(
    x, y,
    all_x = TRUE,
    all_y = FALSE) {
  merge(x, y, by = "name", all.x = all_x, all.y = all_y)
}

# Example 3: Custom comparison function returning GT table
compare_datasets <- function(
    x, y,
    title = "Dataset Comparison") {

  comparison <- data.frame(
    Metric = c("Rows", "Columns", "Common Columns"),
    X = c(nrow(x), ncol(x), NA),
    Y = c(nrow(y), ncol(y), NA)
  )
  comparison$Common_Columns[3] <- length(intersect(names(x), names(y)))

  gt::gt(comparison) |>
    gt::tab_header(title = title)
}

# Example 4: Bind rows with source indicator
stack_with_source <- function(
    x, y,
    x_label = "Dataset X",
    y_label = "Dataset Y") {
  x$.source <- x_label
  y$.source <- y_label
  dplyr::bind_rows(x, y)
}

run_app(
  blocks = c(
    # Data sources - using dplyr's band data
    members = blockr.core::new_dataset_block(
      dataset = "band_members",
      package = "dplyr",
      block_name = "Band Members"
    ),

    instruments = blockr.core::new_dataset_block(
      dataset = "band_instruments",
      package = "dplyr",
      block_name = "Band Instruments"
    ),

    # Simple join (returns data.frame -> auto DataTable)
    joined = new_function_xy_block(
      fn = join_data,
      block_name = "Join Data"
    ),

    # Merge with options (returns data.frame -> auto DataTable)
    merged = new_function_xy_block(
      fn = merge_with_options,
      block_name = "Merge with Options"
    ),

    # Comparison (returns gt_tbl -> auto GT)
    comparison = new_function_xy_block(
      fn = compare_datasets,
      block_name = "Compare Datasets"
    ),

    # Stack with source (returns data.frame -> auto DataTable)
    stacked = new_function_xy_block(
      fn = stack_with_source,
      block_name = "Stack Datasets"
    )
  ),
  links = c(
    # Join block: members -> x, instruments -> y
    new_link("members", "joined", "x"),
    new_link("instruments", "joined", "y"),

    # Merge block: members -> x, instruments -> y
    new_link("members", "merged", "x"),
    new_link("instruments", "merged", "y"),

    # Comparison block: members -> x, instruments -> y
    new_link("members", "comparison", "x"),
    new_link("instruments", "comparison", "y"),

    # Stack block: members -> x, instruments -> y
    new_link("members", "stacked", "x"),
    new_link("instruments", "stacked", "y")
  )
)
