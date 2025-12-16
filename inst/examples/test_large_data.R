# Test script for reproducing large data issue
# https://github.com/BristolMyersSquibb/blockr.csr/issues/5

library(blockr)
library(blockr.csr)
pkgload::load_all()

# Define a simple function that returns all data (no head)
return_all <- function(data) {
  data
}

# Define a function that uses head first (workaround)
return_head <- function(data, n = 1000L) {
  head(data, n)
}

# Option 1: Large ADSL only
# run_app(
#   blocks = c(
#     source = new_random_adsl_block(
#       n_subjects = 1000,
#       seed = 123,
#       block_name = "Large ADSL (1000 subjects)"
#     ),
#     output = new_function_block(
#       fn = return_all,
#       block_name = "Return All Data"
#     )
#   ),
#   links = c(
#     new_link("source", "output")
#   )
# )

# Option 2: ADAE with many rows per subject (more likely to trigger the warning)
run_app(
  blocks = c(
    adsl = new_random_adsl_block(
      n_subjects = 500,
      seed = 123,
      block_name = "ADSL (500 subjects)"
    ),
    # ADAE typically has many rows per subject (one per adverse event)
    adae = new_random_adam_block(
      dataset = "ae",
      seed = 123,
      block_name = "ADAE (many rows)"
    ),
    # This should trigger the DT warning about client-side being too big
    output = new_function_block(
      fn = return_all,
      block_name = "Return All ADAE Data"
    )
  ),
  links = c(
    new_link("adsl", "adae"),
    new_link("adae", "output")
  )
)
