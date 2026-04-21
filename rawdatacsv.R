# Data cleaning script

# Yahoo Finance Global Markets Intelligence 2026

# Transform raw data with 451 rows and 131 columns into clean data with 449 rows and 16 columns by removing redundant, correlated and irrelevant colums



install.packages("tidyverse")
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
  filter(sharpe_1y <-10 | sharpe_1y > 10) %>%
  nrow()

if (sharpe_extreme > 0) {
  cat(sprintf("  %d rows with Sharpe ratio outside [-10, 10] (keeping)\n",
              sharpe_extreme))
  cat(" these extreme values are valid for crypto losses etc\n")
}

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


  
  