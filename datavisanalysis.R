library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)


library(tidyverse)      
library(scales)         
library(ggrepel)        
library(patchwork)     
library(treemapify)     
library(ggdensity)      





df <- read_csv("yahoo_finance_clean.csv", show_col_types = FALSE)
cat(sprintf(" Loaded: %d rows x %d columns\n", nrow(df), ncol(df)))
cat(sprintf(" asset classes: %s\n", paste (unique(df$asset_class), collapse = ", ")))

asset_palette <- c(
  "US_MEGA"     = "#1f4e79",
  "US_MID"      = "#2c7fb8",
  "INTL"        = "#74a9cf",
  "ETF"         = "#2ca25f",
  "INDICES"     = "#78c679",
  "COMMODITIES" = "#fdae61",
  "FOREX"       = "#c994c7",
  "CRYPTO"      = "#d7301f"
)

