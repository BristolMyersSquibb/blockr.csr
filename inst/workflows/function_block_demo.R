# Function Block Demo
# Demonstrates the new_function_block that auto-generates UI from function arguments

library(blockr)
library(blockr.csr)

# Example 1: Simple filter function with dropdown for species
filter_species <- function(data, species = c("setosa", "versicolor", "virginica")) {
dplyr::filter(data, Species == species)
}

# Example 2: Head with configurable n
head_n <- function(data, n = 10L) {
utils::head(data, n)
}

# Example 3: Multiple parameters
transform_data <- function(
data,
species = c("setosa", "versicolor", "virginica"),
min_sepal_length = 5.0,
include_all = FALSE) {
if (include_all) {
data
} else {
dplyr::filter(
  data,
  Species == species,
  Sepal.Length >= min_sepal_length
)
}
}

run_app(
blocks = c(
# Data source
data = blockr.core::new_dataset_block(
  dataset = "iris",
  package = "datasets",
  block_name = "Iris Data"
),

# Function block with species filter
fn_filter = new_function_block(
  fn = filter_species,
  block_name = "Filter by Species"
),

# Another function block with multiple params
fn_transform = new_function_block(
  fn = transform_data,
  block_name = "Multi-param Transform"
)
),
links = c(
new_link("data", "fn_filter", "data"),
new_link("data", "fn_transform", "data")
)
)
