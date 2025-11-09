#!/usr/bin/env Rscript
# Analyze Power Analysis Results
#
# This script reads results from the SQLite database created by large_scale_analysis.R
# and generates summary reports and power curve visualizations.
#
# Prerequisites:
# - Run large_scale_analysis.R first to populate the database
# - Database file: power_analysis_results.sqlite

library(RSQLite)
library(data.table)
library(ggplot2)

# =============================================================================
# 1. Configuration
# =============================================================================

DB_FILE <- "power_analysis_results.sqlite"
OUTPUT_DIR <- "results"
TABLES_DIR <- file.path(OUTPUT_DIR, "tables")
PLOTS_DIR <- file.path(OUTPUT_DIR, "plots")
REPORTS_DIR <- file.path(OUTPUT_DIR, "reports")

# Create output directories if they don't exist
for (dir in c(OUTPUT_DIR, TABLES_DIR, PLOTS_DIR, REPORTS_DIR)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(sprintf("Created directory: %s\n", dir))
  }
}

cat("=== Power Analysis Results Summary ===\n\n")

# =============================================================================
# 2. Connect to Database
# =============================================================================

if (!file.exists(DB_FILE)) {
  stop(sprintf("Database file not found: %s\nPlease run large_scale_analysis.R first", DB_FILE))
}

db <- dbConnect(SQLite(), DB_FILE)

# Check what tables exist
tables <- dbListTables(db)
cat(sprintf("Database contains %d tables: %s\n\n",
            length(tables), paste(tables, collapse = ", ")))

# =============================================================================
# 3. Load Data
# =============================================================================

cat("Loading data from database...\n")

# Load final_power table (detailed simulation results)
if ("final_power" %in% tables) {
  final_power <- as.data.table(dbReadTable(db, "final_power"))
  cat(sprintf("  - final_power: %d rows\n", nrow(final_power)))
} else {
  stop("final_power table not found in database")
}

# Load power_summary table (aggregated power by specification)
if ("power_summary" %in% tables) {
  power_summary <- as.data.table(dbReadTable(db, "power_summary"))
  cat(sprintf("  - power_summary: %d rows\n", nrow(power_summary)))
} else {
  # Calculate it from final_power if not available
  cat("  - power_summary: calculating from final_power...\n")
  power_summary <- final_power[, .(
    power = mean(abs(att/se) > 1.96),
    mean_att = mean(att),
    mean_se = mean(se),
    n_sims = .N / length(unique(model)),
    mean_units_dropped = mean(n_dropped_units, na.rm = TRUE)
  ), by = .(outcome, data_source, analysis_level, pta_type,
            percent_effect, controls, model)]
}

# Load specifications table
if ("specifications" %in% tables) {
  specifications <- as.data.table(dbReadTable(db, "specifications"))
  cat(sprintf("  - specifications: %d rows\n", nrow(specifications)))
}

cat("\n")

# =============================================================================
# 4. Summary Statistics
# =============================================================================

cat("=== SUMMARY STATISTICS ===\n\n")

# Overall counts
cat("Dataset Overview:\n")
cat(sprintf("  Total simulations: %d\n", nrow(final_power)))
cat(sprintf("  Unique outcomes: %d\n", length(unique(final_power$outcome))))
cat(sprintf("  Unique specifications: %d\n",
            nrow(unique(final_power[, .(outcome, pta_type, percent_effect, controls)]))))
cat(sprintf("  Models tested: %s\n", paste(unique(final_power$model), collapse = ", ")))
cat(sprintf("  Effect sizes: %s\n",
            paste(sort(unique(final_power$percent_effect)), collapse = ", ")))
cat("\n")

# Outcomes analyzed
cat("Outcomes Analyzed:\n")
outcomes_table <- final_power[, .N, by = outcome][order(-N)]
print(outcomes_table)
cat("\n")

# Power by model
cat("Average Power by Model (across all specifications):\n")
power_by_model <- final_power[, .(
  power = mean(abs(att/se) > 1.96),
  mean_effect = mean(att),
  mean_se = mean(se)
), by = model]
print(power_by_model)
cat("\n")

# Power by effect size
cat("Average Power by Effect Size (across all specifications):\n")
power_by_effect <- final_power[, .(
  power = mean(abs(att/se) > 1.96),
  n_specs = length(unique(paste(outcome, model, pta_type, controls)))
), by = percent_effect][order(percent_effect)]
print(power_by_effect)
cat("\n")

# PTA violations summary
cat("PTA Violations Summary:\n")
pta_summary <- final_power[, .(
  mean_units_dropped = mean(n_dropped_units, na.rm = TRUE),
  mean_pct_dropped = mean(share_units_dropped, na.rm = TRUE) * 100,
  max_units_dropped = max(n_dropped_units, na.rm = TRUE)
), by = .(pta_type, model)]
print(pta_summary)
cat("\n")

# =============================================================================
# 5. Save Summary Tables
# =============================================================================

cat("Saving summary tables...\n")

# Table 1: Power by outcome and model
power_by_outcome_model <- power_summary[, .(
  avg_power = mean(power),
  min_power = min(power),
  max_power = max(power),
  n_specs = .N
), by = .(outcome, model)][order(outcome, model)]

fwrite(power_by_outcome_model,
       file.path(TABLES_DIR, "power_by_outcome_model.csv"))
cat(sprintf("  ✓ Saved: %s\n", file.path(TABLES_DIR, "power_by_outcome_model.csv")))

# Table 2: Power by effect size and model
power_by_effect_model <- power_summary[, .(
  avg_power = mean(power),
  sd_power = sd(power),
  n_outcomes = length(unique(outcome))
), by = .(percent_effect, model)][order(percent_effect, model)]

fwrite(power_by_effect_model,
       file.path(TABLES_DIR, "power_by_effect_model.csv"))
cat(sprintf("  ✓ Saved: %s\n", file.path(TABLES_DIR, "power_by_effect_model.csv")))

# Table 3: Full power summary
fwrite(power_summary,
       file.path(TABLES_DIR, "power_summary_full.csv"))
cat(sprintf("  ✓ Saved: %s\n", file.path(TABLES_DIR, "power_summary_full.csv")))

cat("\n")

# =============================================================================
# 6. Generate Power Curve Plots
# =============================================================================

cat("Generating power curve plots...\n")

# Prepare data with pta_type for faceting
power_plot_data <- power_summary[!is.na(outcome) & !is.na(pta_type)]

# Create cleaner outcome labels if needed
power_plot_data[, outcome_label := gsub("y_nibrs_female_|y_shr_female_", "", outcome)]

# Transform effect size to show as deviation from baseline (100%)
# percent_effect = 1.0 means baseline (0% change)
# percent_effect = 0.5 means -50% effect
# percent_effect = 1.5 means +50% effect
power_plot_data[, effect_pct := (percent_effect - 1) * 100]

# Power curves faceted by PTA type and outcome
p1 <- ggplot(power_plot_data,
             aes(x = effect_pct, y = power, color = model)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = scales::percent) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  scale_color_brewer(palette = "Set2") +
  facet_grid(outcome_label ~ pta_type, scales = "free_y") +
  labs(
    x = "Treatment Effect (as Percent)",
    y = "Power",
    color = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    strip.text = element_text(face = "bold", size = 10)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave(file.path(PLOTS_DIR, "power_curves.png"),
       p1, width = 10, height = 10, dpi = 300)
cat(sprintf("  ✓ Saved: %s\n", file.path(PLOTS_DIR, "power_curves.png")))

# =============================================================================
# 7. PTA Violations Analysis
# =============================================================================

cat("\nGenerating PTA violations summary...\n")

# Violations should be constant across effect sizes (based on pre-period only)
# Violations don't depend on the model used for estimation
# Summarize by pta_type and outcome only
violations_summary <- final_power[, .(
  mean_units_dropped = mean(n_dropped_units, na.rm = TRUE),
  sd_units_dropped = sd(n_dropped_units, na.rm = TRUE),
  mean_pct_dropped = mean(share_units_dropped, na.rm = TRUE) * 100,
  sd_pct_dropped = sd(share_units_dropped, na.rm = TRUE) * 100,
  n_sims = .N
), by = .(pta_type, outcome, controls, enforce_type)]

violations_summary[, outcome_label := gsub("y_nibrs_female_|y_shr_female_", "", outcome)]

# Save violations table
fwrite(violations_summary,
       file.path(TABLES_DIR, "pta_violations_summary.csv"))
cat(sprintf("  ✓ Saved: %s\n", file.path(TABLES_DIR, "pta_violations_summary.csv")))

# Plot violations by PTA type and outcome
p_vio <- ggplot(violations_summary,
                aes(x = outcome_label, y = mean_pct_dropped, fill = pta_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = pmax(mean_pct_dropped - sd_pct_dropped, 0),
                    ymax = mean_pct_dropped + sd_pct_dropped),
                position = position_dodge(width = 0.8), width = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Outcome",
    y = "Units Dropped (% of sample)",
    fill = "PTA Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

ggsave(file.path(PLOTS_DIR, "pta_violations.png"),
       p_vio, width = 8, height = 5, dpi = 300)
cat(sprintf("  ✓ Saved: %s\n", file.path(PLOTS_DIR, "pta_violations.png")))

# =============================================================================
# 8. Bias Analysis (at 0% treatment effect)
# =============================================================================

cat("\nGenerating bias analysis...\n")

# At percent_effect = 1.0 (0% change from baseline), estimates should be zero
# This checks for bias in the estimators
bias_data <- final_power[percent_effect == 1.0, .(
  mean_att = mean(att),
  sd_att = sd(att),
  mean_se = mean(se),
  bias = mean(att) - 0,  # True effect is 0
  y0_bar = mean(y0_bar),  # Simple baseline rate
  y0_bar_csa = mean(y0_bar_csa, na.rm = TRUE),  # CSA-weighted baseline
  # y0_bar_imp = mean(y0_bar_imp, na.rm = TRUE),  # Imputation-weighted baseline (not in current DB)
  n_sims = .N
), by = .(pta_type, model, outcome, controls, enforce_type)]

# Use model-specific weighted baseline:
# - CS model uses CSA-weighted baseline (from never-treated units)
# - Imputation model uses imputation-weighted baseline (cell-size weighted)
# TEMP FIX: Using y0_bar_csa for both until database is regenerated with y0_bar_imp
bias_data[, y0_bar_weighted := y0_bar_csa]  # ifelse(model == "cs", y0_bar_csa, y0_bar_imp)]

# Calculate bias as percentage of model-specific weighted baseline
bias_data[, bias_pct := (bias / y0_bar_weighted) * 100]
bias_data[, sd_att_pct := (sd_att / y0_bar_weighted) * 100]

bias_data[, outcome_label := gsub("y_nibrs_female_|y_shr_female_", "", outcome)]

# Save bias table
fwrite(bias_data,
       file.path(TABLES_DIR, "bias_at_zero_effect.csv"))
cat(sprintf("  ✓ Saved: %s\n", file.path(TABLES_DIR, "bias_at_zero_effect.csv")))

# Plot bias by PTA type and outcome (as % of baseline rate)
# Use free y-axis scales so small biases (like aggshare) are visible
p_bias <- ggplot(bias_data,
                 aes(x = outcome_label, y = bias_pct, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = bias_pct - sd_att_pct,
                    ymax = bias_pct + sd_att_pct),
                position = position_dodge(width = 0.8), width = 0.3, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  facet_wrap(~pta_type, ncol = 2, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Outcome",
    y = "Bias at 0% Effect (% of Model-Weighted Baseline)",
    fill = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

ggsave(file.path(PLOTS_DIR, "bias_at_zero_effect.png"),
       p_bias, width = 10, height = 5, dpi = 300)
cat(sprintf("  ✓ Saved: %s\n", file.path(PLOTS_DIR, "bias_at_zero_effect.png")))

# =============================================================================
# 9. Violations vs Bias Scatter Plot
# =============================================================================

cat("\nGenerating violations vs bias scatter plot...\n")

# Merge violations with bias data
# Since violations don't vary by model, but bias does, we need to join carefully
bias_vio <- merge(
  bias_data[, .(pta_type, model, outcome, outcome_label, bias_pct, y0_bar_weighted)],
  violations_summary[, .(pta_type, outcome, mean_pct_dropped)],
  by = c("pta_type", "outcome")
)

# Scatter plot: violations (x-axis) vs bias (y-axis)
p_scatter <- ggplot(bias_vio,
                    aes(x = mean_pct_dropped, y = bias_pct,
                        color = model, shape = outcome_label)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~pta_type, ncol = 2) +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "Units Dropped (% of sample)",
    y = "Bias at 0% Effect (% of Model-Weighted Baseline)",
    color = "Model",
    shape = "Outcome"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  )

ggsave(file.path(PLOTS_DIR, "bias_vs_violations.png"),
       p_scatter, width = 10, height = 5, dpi = 300)
cat(sprintf("  ✓ Saved: %s\n", file.path(PLOTS_DIR, "bias_vs_violations.png")))

cat("\n")

# =============================================================================
# 10. Generate Summary Report
# =============================================================================

cat("Generating summary report...\n")

report_file <- file.path(REPORTS_DIR, "power_analysis_report.txt")
sink(report_file)

cat(strrep("=", 80), "\n")
cat("POWER ANALYSIS SUMMARY REPORT\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

cat("DATABASE CONTENTS\n")
cat(strrep("-", 80), "\n")
cat(sprintf("Total simulation runs: %d\n", nrow(final_power)))
cat(sprintf("Unique outcomes: %d\n", length(unique(final_power$outcome))))
cat(sprintf("Models tested: %s\n", paste(unique(final_power$model), collapse = ", ")))
cat(sprintf("Effect sizes tested: %s\n",
            paste(sort(unique(final_power$percent_effect)), collapse = ", ")))
cat("\n\n")

cat("POWER BY MODEL\n")
cat(strrep("-", 80), "\n")
print(power_by_model)
cat("\n\n")

cat("POWER BY EFFECT SIZE\n")
cat(strrep("-", 80), "\n")
print(power_by_effect)
cat("\n\n")

cat("POWER BY OUTCOME AND MODEL\n")
cat(strrep("-", 80), "\n")
print(power_by_outcome_model)
cat("\n\n")

cat("PTA VIOLATIONS SUMMARY\n")
cat(strrep("-", 80), "\n")
print(pta_summary)
cat("\n\n")

cat("KEY FINDINGS\n")
cat(strrep("-", 80), "\n")
cat(sprintf("- Highest average power: %.2f%% (%s model)\n",
            max(power_by_model$power) * 100,
            power_by_model[which.max(power)]$model))
cat(sprintf("- Effect size needed for 80%% power: ~%.1f%%\n",
            power_by_effect[power >= 0.80][1]$percent_effect * 100))
cat(sprintf("- Average PTA violations: %.1f units (%.1f%%)\n",
            mean(final_power$n_dropped_units, na.rm = TRUE),
            mean(final_power$share_units_dropped, na.rm = TRUE) * 100))
cat("\n\n")

cat("OUTPUT FILES\n")
cat(strrep("-", 80), "\n")
cat("Tables (in tables/):\n")
cat("  - power_by_outcome_model.csv\n")
cat("  - power_by_effect_model.csv\n")
cat("  - power_summary_full.csv\n")
cat("  - pta_violations_summary.csv\n")
cat("  - bias_at_zero_effect.csv\n")
cat("\nPlots (in plots/):\n")
cat("  - power_curves.png (faceted by PTA type and outcome)\n")
cat("  - pta_violations.png (violations by PTA type and outcome)\n")
cat("  - bias_at_zero_effect.png (estimator bias at 0% effect)\n")
cat("  - bias_vs_violations.png (bias vs violations scatter plot)\n")
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
cat("\nGenerated files:\n")
cat(sprintf("  Tables (%s/):\n", basename(TABLES_DIR)))
cat("    - power_by_outcome_model.csv\n")
cat("    - power_by_effect_model.csv\n")
cat("    - power_summary_full.csv\n")
cat("    - pta_violations_summary.csv\n")
cat("    - bias_at_zero_effect.csv\n")
cat(sprintf("  Plots (%s/):\n", basename(PLOTS_DIR)))
cat("    - power_curves.png (power by effect size)\n")
cat("    - pta_violations.png (violations by PTA type and outcome)\n")
cat("    - bias_at_zero_effect.png (estimator bias at 0% effect)\n")
cat("    - bias_vs_violations.png (bias vs violations scatter plot)\n")
cat(sprintf("  Reports (%s/):\n", basename(REPORTS_DIR)))
cat(sprintf("    - %s\n", basename(report_file)))
cat("\nView the report:\n")
cat(sprintf("  cat %s\n", report_file))
cat("\nView the plots:\n")
cat(sprintf("  open %s/*.png\n", PLOTS_DIR))
