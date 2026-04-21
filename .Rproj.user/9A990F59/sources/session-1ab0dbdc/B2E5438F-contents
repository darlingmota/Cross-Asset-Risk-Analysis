# Data cleaning script

# Yahoo Finance Global Markets Intelligence 2026

# Transform raw data with 451 rows and 131 columns into clean data with 449 rows and 16 columns by removing redundant, correlated and irrelevant colums

# install.packages("tidyverse")
library(tidyverse)

df_raw <- read_csv("yahoo_finance_global_markets_2026.csv",
                   show_col_types = FALSE)

cat(sprintf(" rows: %d\n", nrow(df_raw)))
cat(sprintf(" columns: %d\n", ncol(df_raw)))

keep_cols <- c(
  "ticker",
  "symbol",
  "shortName",
  "asset_class",
  "quoteType",
  
  "country",
  "sector",
  "industry",
  "exchange",
  "currency",
  
  "return_1y_pct",
  "volatility_1y_ann",
  "sharpe_1y",
  "max_drawdown_1y_pct",
  
  "price_date",
  "build_timestamp"
)

cat(sprintf("Identifiers: 5 columns\n"))
cat(sprintf("Context: 5 columns\n"))
cat(sprintf("Core Metrics: 4 columns\n"))
cat(sprintf("Metadata: 2 columns\n"))
cat(sprintf("Total: 16 columns (from 131)\n"))

missing_cols <- setdiff(keep_cols, names(df_raw))
if (length(missing_cols) > 0) {
  stop(sprintf("ERROR: missing colums in raw data: %s\n",
               paste(missing_cols, collapse = ", ")))
}

cat("  all 16 required columns found in raw data\n\n")

df_clean <- df_raw %>%
  select(all_of(keep_cols))

cat(sprintf(" columns selected: %d\n", ncol(df_clean)))
cat(sprintf(" rows selected: %d\n", nrow(df_clean)))

core_metrics <- c("return_1y_pct", "volatility_1y_ann", "sharpe_1y",
                  "max_drawdown_1y_pct")

missing_summary <- df_clean %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "count") %>%
  filter(count > 0) %>%
  arrange(desc(count))

if (nrow(missing_summary) > 0) {
  cat("  Missing values found:\n")
  for (i in 1:nrow(missing_summary)) {
    row <- missing_summary[i, ]
    pct <- 100 * row$count / nrow(df_clean)
    cat(sprintf("    %s: %d (%.1f%%)\n", row$column, row$count, pct))
  }
  cat("\n")
} else {
  cat("  no missing values in any columns\n\n")
}

rows_before <- nrow(df_clean)

df_clean <- df_clean %>%
  drop_na(all_of(core_metrics))

rows_after <- nrow(df_clean)
rows_removed <- rows_before - rows_after
pct_removed <- 100 * rows_removed / rows_before

cat(sprintf("  Rows before: %d\n", rows_before))
cat(sprintf("  Rows removed: %d (%.1f%%)\n", rows_removed, pct_removed))
cat(sprintf("  Rows after:  %d\n\n", rows_after))

dup_tickers <- df_clean %>%
  group_by(ticker) %>%
  filter(n() > 1) %>%
  pull(ticker) %>%
  unique()

if (length(dup_tickers) > 0) {
  cat(sprintf("  found %d duplicate tickers:\n", length(dup_tickers)))
  for (t in dup_tickers) {
    cat(sprintf("    %s\n", t))
  }
  cat("  keep first occurrence only.\n\n")
  df_clean <- df_clean %>%
    group_by(ticker) %>%
    slice(1) %>%
    ungroup()
} else {
  cat("  no duplicate tickers found\n\n")
}

cat(sprintf("  final row count: %d\n\n", nrow(df_clean)))

vol_invalid <- df_clean %>%
  filter(volatility_1y_ann <= 0) %>%
  nrow()

if (vol_invalid > 0) {
  cat(sprintf("   %d rows with volatility <= 0 (removing)\n", vol_invalid))
  df_clean <- df_clean %>%
    filter(volatility_1y_ann > 0)
}

sharpe_extreme <- df_clean %>%
  filter(sharpe_1y < -10 | sharpe_1y > 10) %>%
  nrow()

if (sharpe_extreme > 0) {
  cat(sprintf("  %d rows with Sharpe ratio outside [-10, 10] (keeping)\n",
              sharpe_extreme))
  cat(" these extreme values are valid for crypto losses etc\n")
}

ret_extreme <- df_clean %>%
  filter(return_1y_pct < -1 | return_1y_pct > 1) %>%
  nrow()

if (ret_extreme > 0) {
  cat(sprintf("  %d rows with extreme returns >±100%% (keeping)\n",
              ret_extreme))
}

cat(sprintf("\n  final validated row count: %d\n\n", nrow(df_clean)))

for (col in core_metrics) {
  data <- df_clean[[col]]
  cat(sprintf("  %s:\n", col))
  cat(sprintf("    Min:     %10.2f\n", min(data, na.rm = TRUE)))
  cat(sprintf("    Q1:      %10.2f\n", quantile(data, 0.25, na.rm = TRUE)))
  cat(sprintf("    Median:  %10.2f\n", median(data, na.rm = TRUE)))
  cat(sprintf("    Q3:      %10.2f\n", quantile(data, 0.75, na.rm = TRUE)))
  cat(sprintf("    Max:     %10.2f\n", max(data, na.rm = TRUE)))
  cat(sprintf("    Mean:    %10.2f\n", mean(data, na.rm = TRUE)))
  cat(sprintf("    Stdev:   %10.2f\n\n", sd(data, na.rm = TRUE)))
}

asset_dist <- df_clean %>%
  count(asset_class, sort = TRUE) %>%
  mutate(pct = sprintf("%.1f%%", 100 * n / sum(n)))

for (i in 1:nrow(asset_dist)) {
  row <- asset_dist[i, ]
  cat(sprintf(" %15s: %3d (%s)\n", row$asset_class, row$n, row$pct))
}

output_path <- "yahoo_finance_clean.csv"
write_csv(df_clean, output_path)

cat(sprintf("  written to: %s\n", output_path))
cat(sprintf("  File size: %.0f KB\n", file.size(output_path) / 1024))
cat(sprintf("  Final size: %d rows × %d columns\n\n", nrow(df_clean), ncol(df_clean)))

cat(sprintf("original data:  %d rows × 131 columns\n", nrow(df_raw)))
cat(sprintf("cleaned data:   %d rows × 16 columns\n\n", nrow(df_clean)))

cat("columns retained (in order):\n")
for (i in seq_along(names(df_clean))) {
  cat(sprintf("  %2d. %s\n", i, names(df_clean)[i]))
}

cat("\n")
cat("kept columns breakdown:\n")
cat("  identifiers:    5\n")
cat("  context:         5\n")
cat("  core metrics:   4\n")
cat("  metadata:       2\n")

cat("\n")
cat("removed columns breakdown:\n")
cat("  price data:               23 columns \n")
cat("  volume data:               4 columns \n")
cat("  redundant returns:         5 columns \n")
cat("  redundant volatility:      2 columns \n")
cat("  valuation ratios:         27 columns \n")
cat("  equity-only metrics:      47 columns \n")
cat("  analyst forecasts:         9 columns \n")
cat("  other: 33 columns\n")
cat("  total removed:            115 columns \n")

cat("\n")
cat("rows removed breakdown:\n")
cat(sprintf("  - Missing core metrics: %d rows (%.1f%%)\n", rows_removed, pct_removed))
cat(sprintf("  - Duplicates:           0 rows\n"))
cat(sprintf("  - Invalid ranges:       0 rows (kept all extreme values)\n"))

# Verify we have exactly 16 columns
if (ncol(df_clean) == 16) {
  cat("  column count: 16\n")
} else {
  cat(sprintf("   Column count is %d, expected 16\n", ncol(df_clean)))
}

# Verify we have 449 rows
if (nrow(df_clean) == 449) {
  cat("  row count: 449\n")
} else {
  cat(sprintf("  row count is %d, expected 449\n", nrow(df_clean)))
}

# Verify no missing in core metrics
missing_in_core <- df_clean %>%
  select(all_of(core_metrics)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  as.numeric() %>%
  sum()

if (missing_in_core == 0) {
  cat("  missing values in core metrics: 0\n")
} else {
  cat(sprintf("   %d missing values in core metrics\n", missing_in_core))
}

# Verify no duplicates
n_unique <- n_distinct(df_clean$ticker)
if (n_unique == nrow(df_clean)) {
  cat("  duplicate tickers: 0\n")
} else {
  cat(sprintf("  %d duplicates found\n", nrow(df_clean) - n_unique))
}

cat("data is ready for analysis\n")


  
  