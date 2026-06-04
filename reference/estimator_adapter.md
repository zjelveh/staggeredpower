# Base Adapter Class for DiD Estimators

Provides a common interface for translating between staggeredpower's
model-agnostic parameters and package-specific estimation functions.

## Usage

``` r
estimator_adapter(name, fit_fn, extract_fn, requires = NULL)
```

## Arguments

- name:

  Character. Unique identifier for this estimator

- fit_fn:

  Function. Translates common params → package call, returns raw result

- extract_fn:

  Function. Translates raw result → standard_estimate object

- requires:

  Character vector. Package dependencies

## Value

An estimator_adapter object
