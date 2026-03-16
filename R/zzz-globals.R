# Global variable bindings for R CMD check
# These are data.table column names and NSE symbols used within functions.
# Declaring them here suppresses "no visible binding for global variable" NOTEs.

if (getRversion() >= "2.15.1") utils::globalVariables(c(
  # data.table special symbols
  ".", ".I", ".N", ".SD",

  # adapter_cs
  ".cs_rate",

  # adapter_did2s
  ".did2s_rate",

  # adapter_imputation
  ".imp_rate", ".imputation_rate", "rel_time",

  # adapter_etwfe_poisson_glm
  ".count", ".log_pop", "gt_cell",

  # enforce_PTA / enforce_PTA_CS / enforce_PTA_imputation / enforce_PTA_poisson
  "treated", ".time_centered", "unit_effect", "i.unit_effect",
  "time_effect", "i.time_effect", "counterfactual", "control_effect",
  ".mu_hat", ".trend_effect", ".control_effect", "cf_mean_rate",
  ".noise_eps", "i.eps", ".pois_count",
  ".mu_log_hat", ".log_rate_cf", ".lambda", ".counterfactual_count",
  "rel_pass", "..controls",

  # run_power_analysis
  "bound_error", "na_error", "y_cf", ".p_eff", ".n_total",
  "sim",

  # run_power_grid
  "spec_id", "i", "att", "se", "model", "n_dropped_units",

  # pretrend_test
  ".rel_time", ".rel_time_trim", ".group", "difference", "early", "late", "ratio",

  # store_results
  "result_id", "status",

  # noise_engine calibrate functions
  "r_tilde", "resid_ld", "prev_resid", "cohort", "r_tilde_lag",
  "eta", "u_k",
  ".noise_e_hat", ".noise_e_lag", ".noise_eta",
  "time", "dgamma",
  ".pois_unit_fe", "i..pois_unit_fe",
  ".pois_time_fe", "i..pois_time_fe",
  ".pois_log_rate", ".pois_lambda_hat",
  "du", "u_mean",

  # noise_engine / other
  "delta_y", "period", "unit", "group", "treatment_period",
  ".fitted", ".resid", "cell_key"
))
