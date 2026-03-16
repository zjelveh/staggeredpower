# R/config.R
#' Load Power Analysis Configuration
#' 
#' @param config_path Path to YAML config file
#' @export
load_config <- function(config_path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required for YAML config loading. ",
         "Install with: install.packages('yaml')", call. = FALSE)
  }
  yaml::read_yaml(config_path)
}