# Test random ADSL block

test_that("new_random_adsl_block creates a valid block", {
  block <- new_random_adsl_block()

  expect_s3_class(block, "random_adsl_block")
  expect_s3_class(block, "data_block")
  expect_s3_class(block, "block")
})

test_that("new_random_adsl_block accepts parameters", {
  block <- new_random_adsl_block(
    seed = 42,
    n_subjects = 200
  )

  expect_s3_class(block, "random_adsl_block")
  expect_s3_class(block, "data_block")
})

test_that("new_random_adsl_block constructor parameters match state", {
  block <- new_random_adsl_block(
    seed = 123,
    n_subjects = 50
  )

  # Get constructor formals

  constructor_params <- names(formals(new_random_adsl_block))
  constructor_params <- constructor_params[constructor_params != "..."]

  # Block should be created successfully with all parameters
  expect_s3_class(block, "random_adsl_block")
  expect_length(constructor_params, 2) # seed, n_subjects
})

test_that("new_random_adsl_block validates n_subjects range", {
  # Valid values
  block1 <- new_random_adsl_block(n_subjects = 10)
  expect_s3_class(block1, "random_adsl_block")

  block2 <- new_random_adsl_block(n_subjects = 1000)
  expect_s3_class(block2, "random_adsl_block")

  # Default value
  block3 <- new_random_adsl_block()
  expect_s3_class(block3, "random_adsl_block")
})

test_that("new_random_adsl_block accepts different seeds", {
  seeds <- c(1, 42, 123, 999)

  for (s in seeds) {
    block <- new_random_adsl_block(seed = s)
    expect_s3_class(block, "random_adsl_block")
  }
})

# Test random ADaM block

test_that("new_random_adam_block creates a valid block", {
  block <- new_random_adam_block()

  expect_s3_class(block, "random_adam_block")
  expect_s3_class(block, "transform_block")
  expect_s3_class(block, "block")
})

test_that("new_random_adam_block accepts parameters", {
  block <- new_random_adam_block(
    dataset = "ae",
    seed = 42
  )

  expect_s3_class(block, "random_adam_block")
  expect_s3_class(block, "transform_block")
})

test_that("new_random_adam_block constructor parameters match state", {
  block <- new_random_adam_block(
    dataset = "vs",
    seed = 123
  )

  # Get constructor formals
  constructor_params <- names(formals(new_random_adam_block))
  constructor_params <- constructor_params[constructor_params != "..."]

  # Block should be created successfully with all parameters
  expect_s3_class(block, "random_adam_block")
  expect_length(constructor_params, 2) # dataset, seed
})

test_that("new_random_adam_block supports all dataset types", {
  datasets <- c("qs", "ae", "vs", "lb", "tte", "eg", "cm", "mh", "ex")

  for (ds in datasets) {
    block <- new_random_adam_block(dataset = ds)
    expect_s3_class(block, "random_adam_block")
  }
})

test_that("new_random_adam_block accepts different seeds", {
  seeds <- c(1, 42, 123, 999)

  for (s in seeds) {
    block <- new_random_adam_block(seed = s)
    expect_s3_class(block, "random_adam_block")
  }
})

# Test registry

test_that("register_csr_blocks works", {
  expect_no_error(register_csr_blocks())
})
