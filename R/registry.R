#' Register CSR Blocks
#'
#' Registers the random CDISC blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks
register_csr_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_random_adsl_block",
      "new_random_adam_block",
      "new_function_block"
    ),
    name = c(
      "Random ADSL block",
      "Random ADaM block",
      "Function block"
    ),
    description = c(
      "Generate random ADSL dataset using random.cdisc.data",
      "Generate random ADaM dataset (ADQS, ADAE, ADVS, etc.) using random.cdisc.data",
      "Transform data with a custom R function. UI auto-generated from function arguments."
    ),
    category = c(
      "input",
      "input",
      "transform"
    ),
    icon = c(
      "people",
      "clipboard-data",
      "code-slash"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
