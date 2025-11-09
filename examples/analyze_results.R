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

# Create output directory if it doesn't exist
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat(sprintf("Created output directory: %s\n", OUTPUT_DIR))
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
       file.path(OUTPUT_DIR, "power_by_outcome_model.csv"))
cat(sprintf("  ✓ Saved: %s\n", file.path(OUTPUT_DIR, "power_by_outcome_model.csv")))

# Table 2: Power by effect size and model
power_by_effect_model <- power_summary[, .(
  avg_power = mean(power),
  sd_power = sd(power),
  n_outcomes = length(unique(outcome))
), by = .(percent_effect, model)][order(percent_effect, model)]

fwrite(power_by_effect_model,
       file.path(OUTPUT_DIR, "power_by_effect_model.csv"))
cat(sprintf("  ✓ Saved: %s\n", file.path(OUTPUT_DIR, "power_by_effect_model.csv")))

# Table 3: Full power summary
fwrite(power_summary,
       file.path(OUTPUT_DIR, "power_summary_full.csv"))
cat(sprintf("  ✓ Saved: %s\n", file.path(OUTPUT_DIR, "power_summary_full.csv")))

cat("\n")

# =============================================================================
# 6. Generate Power Curve Plots
# =============================================================================

cat("Generating power curve plots...\n")

# Plot 1: Power curves by model (averaged across outcomes)
p1 <- ggplot(power_by_effect_model,
             aes(x = percent_effect, y = avg_power, color = model)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = pmax(avg_power - sd_power, 0),
                  ymax = pmin(avg_power + sd_power, 1),
                  fill = model), alpha = 0.2, color = NA) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Statistical Power by Effect Size",
    subtitle = "Averaged across all outcomes (shaded area = ± 1 SD)",
    x = "Simulated Effect Size (% of baseline)",
    y = "Power (% rejecting null at 5% level)",
    color = "Model",
    fill = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(OUTPUT_DIR, "power_curve_by_model.png"),
       p1, width = 10, height = 6, dpi = 300)
cat(sprintf("  ✓ Saved: %s\n", file.path(OUTPUT_DIR, "power_curve_by_model.png")))

# Plot 2: Power curves by outcome (for each model)
# Only plot if we have multiple outcomes
if (length(unique(power_summary$outcome)) > 1 &&
    length(unique(power_summary$outcome)) <= 10) {

  p2 <- ggplot(power_summary[!is.na(outcome)],
               aes(x = percent_effect, y = power, color = outcome)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~model, ncol = 2) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    scale_color_brewer(palette = "Set2") +
    labs(
      title = "Statistical Power by Outcome and Model",
      x = "Simulated Effect Size",
      y = "Power",
      color = "Outcome"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      legend.text = element_text(size = 10)
    ) +
    guides(color = guide_legend(nrow = 3, byrow = TRUE))

  ggsave(file.path(OUTPUT_DIR, "power_curve_by_outcome.png"),
         p2, width = 12, height = 8, dpi = 300)
  cat(sprintf("  ✓ Saved: %s\n", file.path(OUTPUT_DIR, "power_curve_by_outcome.png")))
}

# Plot 3: PTA violations by model
if ("n_dropped_units" %in% names(final_power)) {
  pta_plot_data <- final_power[!is.na(n_dropped_units), .(
    mean_dropped = mean(n_dropped_units),
    se_dropped = sd(n_dropped_units) / sqrt(.N)
  ), by = .(model, percent_effect)]

  p3 <- ggplot(pta_plot_data,
               aes(x = percent_effect, y = mean_dropped, color = model)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = mean_dropped - se_dropped,
                      ymax = mean_dropped + se_dropped),
                  width = 0.01, alpha = 0.5) +
    scale_x_continuous(labels = scales::percent) +
    scale_color_brewer(palette = "Set2") +
    labs(
      title = "PTA Violations by Effect Size and Model",
      subtitle = "Average number of units dropped (± SE)",
      x = "Simulated Effect Size",
      y = "Mean Units Dropped",
      color = "Model"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90")
    ) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))

  ggsave(file.path(OUTPUT_DIR, "pta_violations_by_effect.png"),
         p3, width = 10, height = 6, dpi = 300)
  cat(sprintf("  ✓ Saved: %s\n", file.path(OUTPUT_DIR, "pta_violations_by_effect.png")))
}

cat("\n")

# =============================================================================
# 7. Generate Summary Report
# =============================================================================

cat("Generating summary report...\n")

report_file <- file.path(OUTPUT_DIR, "power_analysis_report.txt")
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
cat("Tables:\n")
cat("  - power_by_outcome_model.csv\n")
cat("  - power_by_effect_model.csv\n")
cat("  - power_summary_full.csv\n")
cat("\nPlots:\n")
cat("  - power_curve_by_model.png\n")
if (file.exists(file.path(OUTPUT_DIR, "power_curve_by_outcome.png"))) {
  cat("  - power_curve_by_outcome.png\n")
}
if (file.exists(file.path(OUTPUT_DIR, "pta_violations_by_effect.png"))) {
  cat("  - pta_violations_by_effect.png\n")
}
cat("\n")

cat(strrep("=", 80), "\n")

sink()

cat(sprintf("  ✓ Saved: %s\n", report_file))

# =============================================================================
# 8. Clean Up
# =============================================================================

dbDisconnect(db)

cat("\n=== Analysis Complete! ===\n")
cat(sprintf("\nAll results saved to: %s/\n", OUTPUT_DIR))
cat("\nView the report:\n")
cat(sprintf("  cat %s\n", report_file))
cat("\nView the plots:\n")
cat(sprintf("  open %s/*.png\n", OUTPUT_DIR))
