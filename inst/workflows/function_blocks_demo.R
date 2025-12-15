# Function Blocks Demo
# Demonstrates all three function block types:
# - function_block: single data input
# - function_xy_block: two data inputs (x, y)
# - function_var_block: variadic inputs (...)

library(blockr)
library(blockr.csr)
pkgload::load_all()

# =============================================================================
# Example 1: Simple function_block - filter by species
# =============================================================================
filter_species <- function(data, species = c("setosa", "versicolor", "virginica")) {
dplyr::filter(data, Species == species)
}

# =============================================================================
# Example 2: Simple function_block - head with configurable n
# =============================================================================
head_n <- function(data, n = 10L) {
utils::head(data, n)
}

# =============================================================================
# Example 3: function_xy_block - left join two datasets
# =============================================================================
join_xy <- function(x, y, by = "name") {
dplyr::left_join(x, y, by = by)
}

# =============================================================================
# Example 4: function_var_block - bind multiple datasets with source ID
# =============================================================================
bind_all <- function(..., .id = "source") {
dplyr::bind_rows(..., .id = .id)
}

run_app(
  blocks = c(
    # --- Data Sources ---
    iris_data = blockr.core::new_dataset_block(
      dataset = "iris",
      package = "datasets",
      block_name = "Iris Data"
    ),

    band_members = blockr.core::new_dataset_block(
      dataset = "band_members",
      package = "dplyr",
      block_name = "Band Members"
    ),

    band_instruments = blockr.core::new_dataset_block(
      dataset = "band_instruments",
      package = "dplyr",
      block_name = "Band Instruments"
    ),

    # --- function_block examples (single data input) ---

    # Filter iris by species
    filtered = new_function_block(
      fn = filter_species,
      block_name = "Filter Species"
    ),

    # Take head of filtered data
    top_rows = new_function_block(
      fn = head_n,
      block_name = "Head N Rows"
    ),

    # --- function_xy_block example (two data inputs) ---

    # Join band members with instruments
    joined = new_function_xy_block(
      fn = join_xy,
      block_name = "Join X & Y"
    ),

    # --- function_var_block example (variadic inputs) ---

    # Bind multiple filtered iris subsets
    setosa_filter = blockr.dplyr::new_filter_expr_block(
      exprs = list("Species == 'setosa'"),
      block_name = "Setosa Only"
    ),

    versicolor_filter = blockr.dplyr::new_filter_expr_block(
      exprs = list("Species == 'versicolor'"),
      block_name = "Versicolor Only"
    ),

    combined = new_function_var_block(
      fn = bind_all,
      block_name = "Bind All (...)"
    )
  ),
  links = c(
    # --- function_block chain: iris -> filter -> head ---
    new_link("iris_data", "filtered", "data"),
    new_link("filtered", "top_rows", "data"),

    # --- function_xy_block: members(x) + instruments(y) -> joined ---
    new_link("band_members", "joined", "x"),
    new_link("band_instruments", "joined", "y"),

    # --- function_var_block: multiple filtered datasets -> combined ---
    new_link("iris_data", "setosa_filter", "data"),
    new_link("iris_data", "versicolor_filter", "data"),
    new_link("setosa_filter", "combined", "1"),
    new_link("versicolor_filter", "combined", "2")
  )
)
