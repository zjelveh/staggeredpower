#!/usr/bin/env Rscript
# Analyze Power Analysis Results - V3 Database with Controls and Transformations
#
# This script creates fine-grained, informative plots with:
# - Intelligent faceting by outcome, transformation, model
# - Bias analysis at null effect (percent_effect = 1.0)
# - Power curves with optimal color/fill mappings

library(RSQLite)
library(data.table)
library(ggplot2)
library(magrittr)  # For %>% pipe operator

# =============================================================================
# 1. Configuration
# =============================================================================

DB_FILE <- "power_analysis_results_V3.sqlite"
OUTPUT_DIR <- "results_v3"
TABLES_DIR <- file.path(OUTPUT_DIR, "tables")
PLOTS_DIR <- file.path(OUTPUT_DIR, "plots")
REPORTS_DIR <- file.path(OUTPUT_DIR, "reports")

# Create output directories
for (dir in c(OUTPUT_DIR, TABLES_DIR, PLOTS_DIR, REPORTS_DIR)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(sprintf("Created directory: %s\n", dir))
  }
}

cat("=== Power Analysis Results V3 (Fine-Grained Analysis) ===\n\n")

# =============================================================================
# 2. Connect to Database
# =============================================================================

if (!file.exists(DB_FILE)) {
  stop(sprintf("Database file not found: %s", DB_FILE))
}

db <- dbConnect(SQLite(), DB_FILE)

tables <- dbListTables(db)
cat(sprintf("Database contains %d tables: %s\n\n",
            length(tables), paste(tables, collapse = ", ")))

# =============================================================================
# 3. Load Data
# =============================================================================

cat("Loading data from database...\n")

final_power <- as.data.table(dbReadTable(db, "final_power"))
cat(sprintf("  - final_power: %d rows\n", nrow(final_power)))

if ("power_summary" %in% tables) {
  power_summary <- as.data.table(dbReadTable(db, "power_summary"))
  cat(sprintf("  - power_summary: %d rows\n", nrow(power_summary)))
}

cat("\n")

# =============================================================================
# 4. Data Preparation
# =============================================================================

cat("Preparing data for analysis...\n")

# Create readable labels
final_power[, control_label := fcase(
  controls == "no_controls", "None",
  controls == "unemp_rate", "Econ",
  controls == "unemp_rate*index_violent_crimes_rate", "Econ+Crime",
  grepl("is_disc.*is_mand", controls) & !grepl("population", controls), "Econ+Crime+DV",
  grepl("population", controls), "Full",
  default = controls
)]

# Get actual control specifications present in data
actual_controls <- unique(final_power$control_label)
actual_controls <- actual_controls[order(actual_controls)]
cat(sprintf("Control specifications in data: %s\n", paste(actual_controls, collapse = ", ")))

# Order control specifications (using only those present)
control_order <- c("None", "Econ", "Econ+Crime", "Econ+Crime+DV", "Full")
control_order <- control_order[control_order %in% actual_controls]
final_power[, control_label := factor(control_label, levels = control_order)]

# Create clean outcome labels
final_power[, outcome_clean := gsub("y_nibrs_female_|y_shr_female_", "", outcome)]
final_power[, data_source := ifelse(grepl("nibrs", outcome), "NIBRS", "SHR")]

# Create transformation label
final_power[, transform_label := ifelse(outcome_transformed == "ihs", "IHS", "Raw")]

# Calculate effect as percentage deviation from 1.0
final_power[, effect_pct := (percent_effect - 1) * 100]

# Calculate rejection indicator
final_power[, rejected := abs(att/se) > 1.96]

# Create combined model-transform label for some plots
final_power[, model_transform := paste0(toupper(substr(model, 1, 1)),
                                        substr(model, 2, nchar(model)),
                                        " (", transform_label, ")")]

cat("  ✓ Created labels and derived variables\n\n")

# =============================================================================
# 5. Bias Analysis at Null Effect
# =============================================================================

cat("=== BIAS ANALYSIS AT NULL EFFECT ===\n\n")

# Extract simulations at null effect (percent_effect = 1.0)
null_sims <- final_power[percent_effect == 1.0]

cat(sprintf("Analyzing %d simulations at null effect (true effect = 0)\n\n",
            nrow(null_sims)))

# Compute bias statistics
bias_stats <- null_sims[, .(
  mean_att = mean(att, na.rm = TRUE),
  median_att = median(att, na.rm = TRUE),
  sd_att = sd(att, na.rm = TRUE),
  mean_se = mean(se, na.rm = TRUE),
  bias = mean(att, na.rm = TRUE),  # At null, bias = mean estimate
  rejection_rate = mean(rejected, na.rm = TRUE),  # Should be ~0.05
  n_sims = .N
), by = .(data_source, outcome_clean, transform_label, control_label, model)]

# Save bias table
fwrite(bias_stats, file.path(TABLES_DIR, "bias_at_null_effect.csv"))
cat("  ✓ Saved: bias_at_null_effect.csv\n")

# Print key bias findings
cat("\nBias by data source and transformation:\n")
bias_summary <- bias_stats[, .(
  mean_bias = mean(bias),
  mean_rejection_rate = mean(rejection_rate),
  n_specs = .N
), by = .(data_source, transform_label, control_label)][
  order(data_source, transform_label, control_label)]
print(bias_summary)
cat("\n")

# =============================================================================
# 6. Bias Plots
# =============================================================================

cat("Generating bias plots...\n")

# Plot 1: Bias distribution by outcome, transformation, and controls
# Facet grid: outcome_clean (rows) × transform_label (cols)
# Color: control_label
# Separate plots for each model

for (mod in unique(bias_stats$model)) {
  bias_data <- bias_stats[model == mod]

  # Create model label (capitalize first letter)
  model_label <- paste0(toupper(substr(mod, 1, 1)), substr(mod, 2, nchar(mod)))

  # Filter out Inf and NaN values for plotting
  bias_data_clean <- bias_data[is.finite(bias)]

  p_bias <- ggplot(bias_data_clean,
                   aes(x = control_label, y = bias, fill = control_label)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.3) +
    facet_grid(outcome_clean ~ transform_label, scales = "free_y") +
    scale_fill_brewer(palette = "Set2", name = "Controls") +
    labs(
      title = sprintf("Bias at Null Effect: %s Estimator", model_label),
      subtitle = "True effect = 0; bias should be close to zero",
      x = "Control Specification",
      y = "Bias (Mean ATT)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 9, face = "bold")
    )

  ggsave(file.path(PLOTS_DIR, sprintf("bias_null_by_outcome_%s.png", mod)),
         p_bias, width = 10, height = 12, dpi = 300, bg = "white")
  cat(sprintf("  ✓ Saved: bias_null_by_outcome_%s.png\n", mod))
}

# Plot 2: Rejection rates at null (should be ~5%)
# Facet: outcome (rows) × model (cols)
# Color: transformation, Fill: controls

rejection_plot_data <- bias_stats[, .(
  rejection_rate = mean(rejection_rate),
  n_specs = sum(n_sims)
), by = .(outcome_clean, transform_label, control_label, model)]

p_rejection <- ggplot(rejection_plot_data,
                      aes(x = control_label, y = rejection_rate,
                          fill = transform_label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0.05, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  facet_grid(outcome_clean ~ model) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.15)) +
  scale_fill_manual(values = c("Raw" = "#66c2a5", "IHS" = "#fc8d62"),
                    name = "Transformation") +
  labs(
    title = "Type I Error Rates at Null Effect",
    subtitle = "Red line shows nominal 5% level",
    x = "Control Specification",
    y = "Rejection Rate"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(PLOTS_DIR, "type1_error_rates_null.png"),
       p_rejection, width = 12, height = 12, dpi = 300, bg = "white")
cat("  ✓ Saved: type1_error_rates_null.png\n\n")

# =============================================================================
# 7. Power Analysis
# =============================================================================

cat("=== POWER ANALYSIS ===\n\n")

# Compute power by all dimensions
power_by_spec <- final_power[, .(
  power = mean(rejected, na.rm = TRUE),
  mean_att = mean(att, na.rm = TRUE),
  mean_se = mean(se, na.rm = TRUE),
  n_sims = .N
), by = .(data_source, outcome_clean, transform_label, control_label,
          model, percent_effect, effect_pct)]

cat("Computed power across all specifications\n")

# Save full power table
fwrite(power_by_spec, file.path(TABLES_DIR, "power_by_all_specs.csv"))
cat("  ✓ Saved: power_by_all_specs.csv\n\n")

# =============================================================================
# 8. Power Curve Plots - Fine-Grained
# =============================================================================

cat("Generating fine-grained power curve plots...\n")

# Plot 3: Power curves by outcome with controls as color
# Facet grid: outcome_clean (rows) × model (cols)
# Separate plots for Raw vs IHS

for (trans in c("Raw", "IHS")) {
  power_data <- power_by_spec[transform_label == trans]

  p_power_outcome <- ggplot(power_data,
                            aes(x = effect_pct, y = power,
                                color = control_label, linetype = control_label)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.5) +
    geom_hline(yintercept = 0.80, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    geom_vline(xintercept = 0, linetype = "dotted",
               color = "gray40", linewidth = 0.5) +
    facet_grid(outcome_clean ~ model, scales = "free") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                       labels = scales::percent) +
    scale_x_continuous(breaks = seq(-50, 50, 25),
                       labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
    scale_color_brewer(palette = "Set2", name = "Controls") +
    scale_linetype_manual(values = c("solid", "dashed", "dotted",
                                     "dotdash", "longdash"),
                          name = "Controls") +
    labs(
      title = sprintf("Power Curves by Outcome: %s Transformation", trans),
      subtitle = "Horizontal line: 80% power threshold",
      x = "Treatment Effect (% change from baseline)",
      y = "Statistical Power"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 9, face = "bold"),
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm")
    )

  ggsave(file.path(PLOTS_DIR, sprintf("power_by_outcome_%s.png", tolower(trans))),
         p_power_outcome, width = 10, height = 12, dpi = 300, bg = "white")
  cat(sprintf("  ✓ Saved: power_by_outcome_%s.png\n", tolower(trans)))
}

# Plot 4: Direct comparison of transformations
# Facet: outcome (rows) × control_label (cols)
# Color: transformation
# Show both models on same plot with linetype

# Use highest control spec available (not necessarily "Full")
highest_control <- control_order[length(control_order)]
power_transform_compare <- power_by_spec[control_label %in% c("None", highest_control)]

p_transform_compare <- ggplot(power_transform_compare,
                              aes(x = effect_pct, y = power,
                                  color = transform_label,
                                  linetype = model)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2) +
  geom_hline(yintercept = 0.80, linetype = "dashed",
             color = "gray40", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "gray40", linewidth = 0.4) +
  facet_grid(outcome_clean ~ control_label) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     labels = scales::percent) +
  scale_x_continuous(breaks = seq(-50, 50, 25),
                     labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  scale_color_manual(values = c("Raw" = "#1b9e77", "IHS" = "#d95f02"),
                     name = "Transformation") +
  scale_linetype_manual(values = c("cs" = "solid", "imputation" = "dashed"),
                        labels = c("cs" = "CS", "imputation" = "Imputation"),
                        name = "Estimator") +
  labs(
    title = "Raw vs IHS Transformation: Power Comparison",
    subtitle = sprintf("Comparing no controls vs %s", highest_control),
    x = "Treatment Effect (% change from baseline)",
    y = "Statistical Power"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

ggsave(file.path(PLOTS_DIR, "power_transform_comparison.png"),
       p_transform_compare, width = 10, height = 12, dpi = 300, bg = "white")
cat("  ✓ Saved: power_transform_comparison.png\n")

# Plot 5: Control specification impact
# Facet: transformation (rows) × outcome (cols)
# Color: control specification
# Single model (CS) for clarity

power_controls <- power_by_spec[model == "cs"]

p_controls <- ggplot(power_controls,
                     aes(x = effect_pct, y = power,
                         color = control_label, group = control_label)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_hline(yintercept = 0.80, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "gray40", linewidth = 0.5) +
  facet_grid(transform_label ~ outcome_clean) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     labels = scales::percent) +
  scale_x_continuous(breaks = seq(-50, 50, 25),
                     labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  scale_color_brewer(palette = "Set2", name = "Controls") +
  labs(
    title = "Impact of Control Specifications on Power (CS Estimator)",
    subtitle = "Rows: transformation type; Columns: outcome variable",
    x = "Treatment Effect (% change from baseline)",
    y = "Statistical Power"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(PLOTS_DIR, "power_controls_cs.png"),
       p_controls, width = 14, height = 8, dpi = 300, bg = "white")
cat("  ✓ Saved: power_controls_cs.png\n")

# Plot 6: Model comparison (CS vs Imputation)
# Facet: outcome (rows) × transformation (cols)
# Color: model
# Show highest control spec only

power_model_compare <- power_by_spec[control_label == highest_control]

p_model_compare <- ggplot(power_model_compare,
                          aes(x = effect_pct, y = power,
                              color = model, linetype = model)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 0.80, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "gray40", linewidth = 0.5) +
  facet_grid(outcome_clean ~ transform_label) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     labels = scales::percent) +
  scale_x_continuous(breaks = seq(-50, 50, 25),
                     labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  scale_color_manual(values = c("cs" = "#7570b3", "imputation" = "#e7298a"),
                     labels = c("cs" = "CS", "imputation" = "Imputation"),
                     name = "Estimator") +
  scale_linetype_manual(values = c("cs" = "solid", "imputation" = "dashed"),
                        labels = c("cs" = "CS", "imputation" = "Imputation"),
                        name = "Estimator") +
  labs(
    title = "CS vs Imputation Estimator: Power Comparison",
    subtitle = sprintf("%s controls", highest_control),
    x = "Treatment Effect (% change from baseline)",
    y = "Statistical Power"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(PLOTS_DIR, "power_model_comparison_full_controls.png"),
       p_model_compare, width = 10, height = 12, dpi = 300, bg = "white")
cat("  ✓ Saved: power_model_comparison_full_controls.png\n\n")

# =============================================================================
# 9. Summary Statistics Tables
# =============================================================================

cat("Generating summary statistics...\n")

# Table 1: Power at key effect sizes by outcome and specification
key_effects <- c(0.9, 1.0, 1.1, 1.2)
power_at_key <- final_power[percent_effect %in% key_effects, .(
  power = mean(rejected, na.rm = TRUE),
  n_sims = .N
), by = .(outcome_clean, transform_label, control_label,
          model, percent_effect)]

power_at_key_wide <- dcast(power_at_key,
                            outcome_clean + transform_label + control_label + model ~
                              percent_effect,
                            value.var = "power")

fwrite(power_at_key_wide, file.path(TABLES_DIR, "power_at_key_effects.csv"))
cat("  ✓ Saved: power_at_key_effects.csv\n")

# Table 2: Control specification impact summary
control_impact <- power_by_spec[, .(
  mean_power = mean(power),
  power_at_20pct = mean(power[abs(effect_pct - 20) < 0.1]),
  power_at_minus20pct = mean(power[abs(effect_pct + 20) < 0.1]),
  n_specs = .N
), by = .(control_label, transform_label, model)]

fwrite(control_impact, file.path(TABLES_DIR, "control_impact_summary.csv"))
cat("  ✓ Saved: control_impact_summary.csv\n")

# Table 3: Transformation comparison
transform_impact <- power_by_spec[, .(
  mean_power = mean(power),
  mean_se = mean(mean_se),
  n_outcomes = length(unique(outcome_clean))
), by = .(transform_label, control_label, model, effect_pct)]

fwrite(transform_impact, file.path(TABLES_DIR, "transformation_impact.csv"))
cat("  ✓ Saved: transformation_impact.csv\n\n")

# =============================================================================
# 10. Generate Comprehensive Report
# =============================================================================

cat("Generating comprehensive report...\n")

report_file <- file.path(REPORTS_DIR, "v3_analysis_report.txt")
sink(report_file)

cat(strrep("=", 80), "\n")
cat("POWER ANALYSIS V3: COMPREHENSIVE REPORT\n")
cat("Fine-Grained Analysis with Bias and Power\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

cat("DATASET OVERVIEW\n")
cat(strrep("-", 80), "\n")
cat(sprintf("Total simulation runs: %d\n", nrow(final_power)))
cat(sprintf("Outcomes analyzed: %d\n", length(unique(final_power$outcome))))
cat(sprintf("  - NIBRS: %d\n", sum(grepl("nibrs", unique(final_power$outcome)))))
cat(sprintf("  - SHR: %d\n", sum(grepl("shr", unique(final_power$outcome)))))
cat(sprintf("Transformations: %s\n", paste(unique(final_power$transform_label), collapse = ", ")))
cat(sprintf("Control specifications: %d (%s)\n",
            length(unique(final_power$control_label)),
            paste(levels(final_power$control_label), collapse = ", ")))
cat(sprintf("Estimators: %s\n", paste(unique(final_power$model), collapse = ", ")))
cat(sprintf("Effect sizes tested: %d (%.0f%% to %.0f%%)\n",
            length(unique(final_power$percent_effect)),
            min(final_power$effect_pct), max(final_power$effect_pct)))
cat("\n\n")

cat("BIAS ANALYSIS AT NULL EFFECT\n")
cat(strrep("-", 80), "\n")
cat("Mean bias by transformation and controls:\n\n")
print(bias_summary)
cat("\n")

cat("Notes:\n")
cat("- Bias should be close to zero at null effect\n")
cat("- Rejection rates should be close to 5% (nominal Type I error)\n")
cat("- Large deviations suggest specification issues\n\n")

# Identify problematic specs
high_bias <- bias_stats[abs(bias) > 0.1]
if (nrow(high_bias) > 0) {
  cat("⚠ Specifications with |bias| > 0.1:\n")
  print(high_bias[order(-abs(bias)),
                  .(outcome_clean, transform_label, control_label, model, bias)])
  cat("\n")
}

high_rejection <- bias_stats[rejection_rate > 0.10]
if (nrow(high_rejection) > 0) {
  cat("⚠ Specifications with rejection rate > 10%:\n")
  print(high_rejection[order(-rejection_rate),
                       .(outcome_clean, transform_label, control_label,
                         model, rejection_rate)])
  cat("\n")
}

cat("\n")

cat("POWER ANALYSIS SUMMARY\n")
cat(strrep("-", 80), "\n")
cat("Average power at +20% effect by specification:\n\n")

power_20 <- power_by_spec[abs(effect_pct - 20) < 0.1, .(
  mean_power = mean(power),
  n_outcomes = length(unique(outcome_clean))
), by = .(transform_label, control_label, model)][
  order(transform_label, control_label, model)]
print(power_20)
cat("\n\n")

cat("KEY FINDINGS\n")
cat(strrep("-", 80), "\n\n")

# 1. Transformation impact
cat("1. TRANSFORMATION EFFECTS\n\n")
compare_controls <- c("None", highest_control)
for (ctrl in compare_controls) {
  raw_power <- mean(power_20[control_label == ctrl & transform_label == "Raw"]$mean_power, na.rm = TRUE)
  ihs_power <- mean(power_20[control_label == ctrl & transform_label == "IHS"]$mean_power, na.rm = TRUE)
  if (!is.na(raw_power) && !is.na(ihs_power)) {
    cat(sprintf("   %s controls:\n", ctrl))
    cat(sprintf("     - Raw outcomes: %.1f%% power at +20%% effect\n", raw_power * 100))
    cat(sprintf("     - IHS outcomes: %.1f%% power at +20%% effect\n", ihs_power * 100))
    cat(sprintf("     - Difference: %+.1f pp\n\n", (ihs_power - raw_power) * 100))
  }
}

# 2. Control specification impact
cat("2. CONTROL SPECIFICATION EFFECTS\n\n")
for (trans in c("Raw", "IHS")) {
  none_power <- mean(power_20[control_label == "None" & transform_label == trans]$mean_power, na.rm = TRUE)
  highest_power <- mean(power_20[control_label == highest_control & transform_label == trans]$mean_power, na.rm = TRUE)
  if (!is.na(none_power) && !is.na(highest_power)) {
    cat(sprintf("   %s transformation:\n", trans))
    cat(sprintf("     - No controls: %.1f%% power\n", none_power * 100))
    cat(sprintf("     - %s controls: %.1f%% power\n", highest_control, highest_power * 100))
    cat(sprintf("     - Impact: %+.1f pp\n\n", (highest_power - none_power) * 100))
  }
}

# 3. Model comparison
cat(sprintf("3. ESTIMATOR COMPARISON (%s controls, +20%% effect)\n\n", highest_control))
for (trans in c("Raw", "IHS")) {
  cs_power <- mean(power_20[model == "cs" & control_label == highest_control &
                             transform_label == trans]$mean_power, na.rm = TRUE)
  imp_power <- mean(power_20[model == "imputation" & control_label == highest_control &
                              transform_label == trans]$mean_power, na.rm = TRUE)
  if (!is.na(cs_power) && !is.na(imp_power)) {
    cat(sprintf("   %s transformation:\n", trans))
    cat(sprintf("     - CS: %.1f%% power\n", cs_power * 100))
    cat(sprintf("     - Imputation: %.1f%% power\n", imp_power * 100))
    cat(sprintf("     - Difference: %+.1f pp\n\n", (imp_power - cs_power) * 100))
  }
}

cat("\n")

cat("OUTPUT FILES\n")
cat(strrep("-", 80), "\n")
cat("\nTables:\n")
cat("  - bias_at_null_effect.csv: Bias and Type I error rates\n")
cat("  - power_by_all_specs.csv: Complete power analysis results\n")
cat("  - power_at_key_effects.csv: Power at key effect sizes\n")
cat("  - control_impact_summary.csv: Control specification impacts\n")
cat("  - transformation_impact.csv: Raw vs IHS comparison\n")
cat("\nPlots - Bias:\n")
cat("  - bias_null_by_outcome_cs.png: Bias by outcome (CS)\n")
cat("  - bias_null_by_outcome_imputation.png: Bias by outcome (Imputation)\n")
cat("  - type1_error_rates_null.png: Type I error rates\n")
cat("\nPlots - Power:\n")
cat("  - power_by_outcome_raw.png: Power curves, raw transformation\n")
cat("  - power_by_outcome_ihs.png: Power curves, IHS transformation\n")
cat("  - power_transform_comparison.png: Raw vs IHS comparison\n")
cat("  - power_controls_cs.png: Control specification impact\n")
cat("  - power_model_comparison_full_controls.png: CS vs Imputation\n")
cat("\n")

cat(strrep("=", 80), "\n")

sink()

cat(sprintf("  ✓ Saved: %s\n", report_file))

# =============================================================================
# 11. Clean Up
# =============================================================================

dbDisconnect(db)

cat("\n=== Analysis Complete! ===\n")
cat(sprintf("\nAll results saved to: %s/\n", OUTPUT_DIR))
cat("\nRecommended viewing order:\n")
cat("  1. Read report: cat", report_file, "\n")
cat("  2. Check bias: bias_null_by_outcome_*.png\n")
cat("  3. Compare transformations: power_transform_comparison.png\n")
cat("  4. Examine controls: power_controls_cs.png\n")
cat("  5. Review individual outcomes: power_by_outcome_*.png\n")
cat("\n")
