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
