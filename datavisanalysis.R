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

cat(sprintf("  Loaded: %d rows × %d columns\n", nrow(df), ncol(df)))
cat(sprintf("  Asset classes: %s\n", paste(unique(df$asset_class), collapse = ", ")))




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

my_theme <- theme_minimal(base_size = 12, base_family = "Helvetica") +
  theme(
    plot.title    = element_text(face = "bold", size = 15, colour = "#1f4e79", margin = margin(b = 5)),
    plot.subtitle = element_text(colour = "grey40", size = 11, margin = margin(b = 10)),
    plot.caption  = element_text(colour = "grey50", size = 9, hjust = 0, margin = margin(t = 10)),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position  = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(colour = "grey40"),
    axis.title = element_text(colour = "grey40", size = 11)
  )

theme_set(my_theme)


dir.create("charts", showWarnings = FALSE)



