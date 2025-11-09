# tests/testthat/test-adapter-base.R

# Helper to restore default adapters after clearing
restore_default_adapters <- function() {
  if (requireNamespace("did", quietly = TRUE)) {
    register_adapter(adapter_cs())
  }
  if (requireNamespace("didimputation", quietly = TRUE)) {
    register_adapter(adapter_imputation())
  }
}

test_that("adapter base class creates valid structure", {
  test_adapter <- estimator_adapter(
    name = "test",
    fit_fn = function(...) list(result = "test"),
    extract_fn = function(x) x,
    requires = "base"
  )

  expect_s3_class(test_adapter, "estimator_adapter")
  expect_equal(test_adapter$name, "test")
  expect_type(test_adapter$fit, "closure")
  expect_type(test_adapter$extract, "closure")
})

test_that("adapter registry stores and retrieves adapters", {
  # Clear registry
  clear_adapter_registry()

  test_adapter <- estimator_adapter(
    name = "test_model",
    fit_fn = function(...) list(),
    extract_fn = function(x) x
  )

  register_adapter(test_adapter)
  retrieved <- get_adapter("test_model")

  expect_equal(retrieved$name, "test_model")

  # Restore defaults
  restore_default_adapters()
})

test_that("get_adapter throws error for unknown model", {
  clear_adapter_registry()
  expect_error(
    get_adapter("nonexistent_model"),
    "No adapter registered for model: nonexistent_model"
  )

  # Restore defaults
  restore_default_adapters()
})

test_that("list_adapters returns registered adapter names", {
  # Clear registry
  clear_adapter_registry()

  register_adapter(estimator_adapter("model1", function(...) NULL, function(x) x))
  register_adapter(estimator_adapter("model2", function(...) NULL, function(x) x))

  adapters <- list_adapters()
  expect_equal(sort(adapters), c("model1", "model2"))

  # Restore defaults
  restore_default_adapters()
})
