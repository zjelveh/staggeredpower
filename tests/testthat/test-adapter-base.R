# tests/testthat/test-adapter-base.R
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
  .adapter_registry <<- new.env(parent = emptyenv())

  test_adapter <- estimator_adapter(
    name = "test_model",
    fit_fn = function(...) list(),
    extract_fn = function(x) x
  )

  register_adapter(test_adapter)
  retrieved <- get_adapter("test_model")

  expect_equal(retrieved$name, "test_model")
})

test_that("get_adapter throws error for unknown model", {
  .adapter_registry <<- new.env(parent = emptyenv())
  expect_error(
    get_adapter("nonexistent_model"),
    "No adapter registered for model: nonexistent_model"
  )
})

test_that("list_adapters returns registered adapter names", {
  .adapter_registry <<- new.env(parent = emptyenv())

  register_adapter(estimator_adapter("model1", function(...) NULL, function(x) x))
  register_adapter(estimator_adapter("model2", function(...) NULL, function(x) x))

  adapters <- list_adapters()
  expect_equal(sort(adapters), c("model1", "model2"))
})
