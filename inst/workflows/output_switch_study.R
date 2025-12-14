# Output Switch Case Study
#
# Investigating whether we can switch output types at runtime in blockr.
# This is a minimal example to understand the architecture.

library(blockr)
library(blockr.csr)

run_app(
  blocks = c(
    switch_block = new_output_switch_block(
      output_type = "data.frame",
      block_name = "Output Switch Study"
    )
  )
)
