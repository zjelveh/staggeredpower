# R/config.R
#' Load Power Analysis Configuration
#' 
#' @param config_path Path to YAML config file
#' @export
load_config <- function(config_path) {
  yaml::read_yaml(config_path)
}