#!/usr/bin/env Rscript
# Analyze Power Analysis Results - V2 Database with Transformations
#
# This script analyzes the V2 database which contains both untransformed
# and IHS-transformed outcomes, allowing comparison across transformations.

library(RSQLite)
library(data.table)
library(ggplot2)

# =============================================================================
# 1. Configuration
# =============================================================================

DB_FILE <- "power_analysis_results_V2.sqlite"
OUTPUT_DIR <- "results_v2"
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

cat("=== Power Analysis Results V2 (with Transformations) ===\n\n")

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
# 4. Transformation Summary
# =============================================================================

cat("=== TRANSFORMATION SUMMARY ===\n\n")

cat("Transformations in database:\n")
transform_counts <- final_power[, .N, by = outcome_transformed][order(-N)]
print(transform_counts)
cat("\n")

cat("Breakdown by transformation and model:\n")
transform_model_counts <- final_power[, .N, by = .(outcome_transformed, model)][
  order(outcome_transformed, model)]
print(transform_model_counts)
cat("\n")

# =============================================================================
# 5. Summary Statistics by Transformation
# =============================================================================

cat("=== SUMMARY STATISTICS BY TRANSFORMATION ===\n\n")

# Overall counts
cat("Dataset Overview:\n")
cat(sprintf("  Total simulations: %d\n", nrow(final_power)))
cat(sprintf("  Unique outcomes: %d\n", length(unique(final_power$outcome))))
cat(sprintf("  Transformations: %s\n", 
            paste(unique(final_power$outcome_transformed), collapse = ", ")))
cat(sprintf("  Models tested: %s\n", paste(unique(final_power$model), collapse = ", ")))
cat("\n")

# Power by transformation and model
cat("Average Power by Transformation and Model:\n")
power_by_transform_model <- final_power[, .(
  power = mean(abs(att/se) > 1.96),
  mean_att = mean(att),
  mean_se = mean(se),
  n_sims = .N
), by = .(outcome_transformed, model)]
print(power_by_transform_model)
cat("\n")

# Power by transformation and effect size
cat("Average Power by Transformation and Effect Size:\n")
power_by_transform_effect <- final_power[, .(
  power = mean(abs(att/se) > 1.96),
  n_specs = .N
), by = .(outcome_transformed, percent_effect)][order(outcome_transformed, percent_effect)]
print(power_by_transform_effect)
cat("\n")

# PTA violations by transformation
cat("PTA Violations by Transformation:\n")
pta_by_transform <- final_power[, .(
  mean_units_dropped = mean(n_dropped_units, na.rm = TRUE),
  mean_pct_dropped = mean(share_units_dropped, na.rm = TRUE) * 100,
  max_units_dropped = max(n_dropped_units, na.rm = TRUE),
  n_sims = .N
), by = outcome_transformed]
print(pta_by_transform)
cat("\n")

# =============================================================================
# 6. Save Summary Tables
# =============================================================================

cat("Saving summary tables...\n")

# Table 1: Power by transformation, outcome, and model
power_by_transform_outcome_model <- final_power[, .(
  avg_power = mean(abs(att/se) > 1.96),
  mean_att = mean(att),
  mean_se = mean(se),
  n_sims = .N
), by = .(outcome_transformed, outcome, model)][order(outcome_transformed, outcome, model)]

fwrite(power_by_transform_outcome_model,
       file.path(TABLES_DIR, "power_by_transform_outcome_model.csv"))
cat(sprintf("  ✓ Saved: %s\n", file.path(TABLES_DIR, "power_by_transform_outcome_model.csv")))

# Table 2: Power by transformation and effect size
fwrite(power_by_transform_effect,
       file.path(TABLES_DIR, "power_by_transform_effect.csv"))
cat(sprintf("  ✓ Saved: %s\n", file.path(TABLES_DIR, "power_by_transform_effect.csv")))

# Table 3: Full power data
fwrite(final_power,
       file.path(TABLES_DIR, "final_power_full.csv"))
cat(sprintf("  ✓ Saved: %s\n", file.path(TABLES_DIR, "final_power_full.csv")))

cat("\n")

# =============================================================================
# 7. Generate Power Curve Plots by Transformation
# =============================================================================

cat("Generating power curve plots comparing transformations...\n")

# Prepare plot data
power_plot_data <- final_power[!is.na(outcome)]
power_plot_data[, outcome_label := gsub("y_nibrs_female_|y_shr_female_", "", outcome)]
power_plot_data[, effect_pct := (percent_effect - 1) * 100]

# Calculate average power by transformation and effect size
power_curves_data <- power_plot_data[, .(
  power = mean(abs(att/se) > 1.96),
  n_sims = .N
), by = .(outcome_transformed, model, effect_pct, percent_effect)]

# Power curves comparing transformations
p1 <- ggplot(power_curves_data,
             aes(x = effect_pct, y = power, 
                 color = model, linetype = outcome_transformed)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0.80, linetype = "dashed", 
             color = "gray50", linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = scales::percent) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  scale_color_brewer(palette = "Set2") +
  scale_linetype_manual(values = c("No" = "solid", "ihs" = "dashed", "log" = "dotted"),
                        labels = c("No" = "Untransformed", 
                                   "ihs" = "IHS", 
                                   "log" = "Log")) +
  labs(
    title = "Power Curves by Transformation Type",
    x = "Treatment Effect (as Percent)",
    y = "Power",
    color = "Model",
    linetype = "Transformation"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE),
         linetype = guide_legend(nrow = 1, byrow = TRUE))

ggsave(file.path(PLOTS_DIR, "power_curves_by_transformation.png"),
       p1, width = 10, height = 7, dpi = 300)
cat(sprintf("  ✓ Saved: %s\n", file.path(PLOTS_DIR, "power_curves_by_transformation.png")))

# Faceted plot by outcome
power_by_outcome_data <- power_plot_data[, .(
  power = mean(abs(att/se) > 1.96)
), by = .(outcome_transformed, outcome_label, model, effect_pct)]

p2 <- ggplot(power_by_outcome_data,
             aes(x = effect_pct, y = power, 
                 color = outcome_transformed)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray50") +
  facet_grid(outcome_label ~ model, scales = "free_y") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = scales::percent) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  scale_color_manual(values = c("No" = "#1b9e77", "ihs" = "#d95f02", "log" = "#7570b3"),
                     labels = c("No" = "Untransformed", "ihs" = "IHS", "log" = "Log")) +
  labs(
    title = "Power Curves by Outcome and Transformation",
    x = "Treatment Effect (as Percent)",
    y = "Power",
    color = "Transformation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 10)
  )

ggsave(file.path(PLOTS_DIR, "power_curves_by_outcome_transformation.png"),
       p2, width = 12, height = 10, dpi = 300)
cat(sprintf("  ✓ Saved: %s\n", file.path(PLOTS_DIR, "power_curves_by_outcome_transformation.png")))

# =============================================================================
# 8. PTA Violations Comparison
# =============================================================================

cat("\nGenerating PTA violations comparison...\n")

violations_by_transform <- final_power[, .(
  mean_units_dropped = mean(n_dropped_units, na.rm = TRUE),
  sd_units_dropped = sd(n_dropped_units, na.rm = TRUE),
  mean_pct_dropped = mean(share_units_dropped, na.rm = TRUE) * 100,
  n_sims = .N
), by = .(outcome_transformed, pta_type, outcome)]

violations_by_transform[, outcome_label := gsub("y_nibrs_female_|y_shr_female_", "", outcome)]

fwrite(violations_by_transform,
       file.path(TABLES_DIR, "pta_violations_by_transformation.csv"))
cat(sprintf("  ✓ Saved: %s\n", file.path(TABLES_DIR, "pta_violations_by_transformation.csv")))

# Plot violations by transformation
p_vio <- ggplot(violations_by_transform,
                aes(x = outcome_label, y = mean_pct_dropped, 
                    fill = outcome_transformed)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~pta_type, ncol = 2) +
  scale_fill_manual(values = c("No" = "#1b9e77", "ihs" = "#d95f02", "log" = "#7570b3"),
                    labels = c("No" = "Untransformed", "ihs" = "IHS", "log" = "Log")) +
  labs(
    title = "PTA Violations by Transformation Type",
    x = "Outcome",
    y = "Units Dropped (% of sample)",
    fill = "Transformation"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

ggsave(file.path(PLOTS_DIR, "pta_violations_by_transformation.png"),
       p_vio, width = 10, height = 6, dpi = 300)
cat(sprintf("  ✓ Saved: %s\n", file.path(PLOTS_DIR, "pta_violations_by_transformation.png")))

# =============================================================================
# 9. Generate Summary Report
# =============================================================================

cat("\nGenerating summary report...\n")

report_file <- file.path(REPORTS_DIR, "power_analysis_v2_report.txt")
sink(report_file)

cat(strrep("=", 80), "\n")
cat("POWER ANALYSIS V2 SUMMARY REPORT (with Transformations)\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

cat("DATABASE CONTENTS\n")
cat(strrep("-", 80), "\n")
cat(sprintf("Total simulation runs: %d\n", nrow(final_power)))
cat(sprintf("Transformations: %s\n", 
            paste(unique(final_power$outcome_transformed), collapse = ", ")))
cat(sprintf("Models tested: %s\n", paste(unique(final_power$model), collapse = ", ")))
cat("\nRows by transformation:\n")
print(transform_counts)
cat("\n\n")

cat("POWER BY TRANSFORMATION AND MODEL\n")
cat(strrep("-", 80), "\n")
print(power_by_transform_model)
cat("\n\n")

cat("PTA VIOLATIONS BY TRANSFORMATION\n")
cat(strrep("-", 80), "\n")
print(pta_by_transform)
cat("\n\n")

cat("KEY FINDINGS\n")
cat(strrep("-", 80), "\n")
cat("Transformation Comparison:\n")
for (trans in unique(final_power$outcome_transformed)) {
  trans_data <- final_power[outcome_transformed == trans]
  trans_label <- ifelse(trans == "No", "Untransformed", 
                        ifelse(trans == "ihs", "IHS", trans))
  cat(sprintf("\n%s transformation:\n", trans_label))
  cat(sprintf("  - Rows: %d\n", nrow(trans_data)))
  cat(sprintf("  - Average power: %.2f%%\n", 
              mean(abs(trans_data$att/trans_data$se) > 1.96) * 100))
  cat(sprintf("  - Average units dropped: %.1f (%.1f%%)\n",
              mean(trans_data$n_dropped_units, na.rm = TRUE),
              mean(trans_data$share_units_dropped, na.rm = TRUE) * 100))
}
cat("\n\n")

cat("OUTPUT FILES\n")
cat(strrep("-", 80), "\n")
cat("Tables (in tables/):\n")
cat("  - power_by_transform_outcome_model.csv\n")
cat("  - power_by_transform_effect.csv\n")
cat("  - final_power_full.csv\n")
cat("  - pta_violations_by_transformation.csv\n")
cat("\nPlots (in plots/):\n")
cat("  - power_curves_by_transformation.png\n")
cat("  - power_curves_by_outcome_transformation.png\n")
cat("  - pta_violations_by_transformation.png\n")
cat("\n")

cat(strrep("=", 80), "\n")

sink()

cat(sprintf("  ✓ Saved: %s\n", report_file))

# =============================================================================
# 10. Clean Up
# =============================================================================

dbDisconnect(db)

cat("\n=== Analysis Complete! ===\n")
cat(sprintf("\nAll results saved to: %s/\n", OUTPUT_DIR))
cat("\nNext steps:\n")
cat(sprintf("  1. View report: cat %s\n", report_file))
cat(sprintf("  2. View plots: open %s/*.png\n", PLOTS_DIR))
cat(sprintf("  3. Analyze tables: %s/*.csv\n", TABLES_DIR))
