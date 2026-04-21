library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)

df <- read_csv("yahoo_finance_clean.csv", show_col_types = FALSE)
cat(sprintf(" Loaded: %d rows x %d columns\n", nrow(df), ncol(df)))
cat(sprintf(" asset classes: %s\n", paste (unique(df$asset_class), collapse = ", ")))



