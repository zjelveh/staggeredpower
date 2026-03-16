# Create example dataset for staggeredpower package
library(data.table)

# Read source data
dt <- fread("/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/base_rate_fallacy/data/nibrs_clearance_unbalanced_state.csv")

# Select balanced panel subset with key columns only
nfs_panel <- dt[in_balanced_panel == TRUE & year >= 2003 & year <= 2019,
  .(state, year, year_passed, treat_ind, rel_pass,
    agg_case_def1, agg_cleared_arrest_def1)]

# Rename for clarity
setnames(nfs_panel, c("state", "year", "year_passed", "treat_ind", "rel_pass",
                       "agg_case_def1", "agg_cleared_arrest_def1"),
         c("state", "year", "treatment_year", "treated", "rel_time",
           "assault_rate", "clearance_rate"))

# Convert to plain data.frame for CRAN compatibility
nfs_panel <- as.data.frame(nfs_panel)

# Save dataset
save(nfs_panel, file = "data/nfs_panel.rda", compress = "xz")
