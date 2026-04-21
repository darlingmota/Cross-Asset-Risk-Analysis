# Data cleaning script

# Yahoo Finance Global Markets Intelligence 2026

# Transform raw data with 451 rows and 131 columns into clean data with 449 rows and 16 columns by removing redundant, correlated and irrelevant colums



install.packages("tidyverse")
library(tidyverse)

df_raw <- read_csv("yahoo_finance_global_markets_2026.csv",
                   show_col_types = FALSE)

cat(sprintf(" rows: %d\n", nrow(df_raw)))
cat(sprintf(" columns: %d\n", ncol(df_raw)))

keep_cols <- c

