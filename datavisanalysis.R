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




p_e1 <- df %>%
  count(asset_class) %>%
  mutate(
    asset_class = fct_reorder(asset_class, n),
    pct = n / sum(n) * 100
  ) %>%
  ggplot(aes(area = n, fill = asset_class, label = asset_class)) +
  geom_treemap(colour = "white", size = 3) +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    size = 12,
    fontface = "bold",
    family = "Helvetica"
  ) +
  scale_fill_manual(values = asset_palette, guide = "none") +
  labs(
    title    = "Dataset composition by asset class",
    subtitle = "449 instruments. Size = count, colour = class"
  )

ggsave("charts/exploratory_1_asset_class_counts.png", p_e1,
       width = 10, height = 6, dpi = 300)


p_e2 <- df %>%
  filter(!is.na(return_1y_pct), !is.na(asset_class)) %>%
  filter(return_1y_pct > -100, return_1y_pct < 300) %>%
  mutate(asset_class = fct_reorder(asset_class, return_1y_pct, .fun = median)) %>%
  ggplot(aes(x = return_1y_pct, y = asset_class, fill = asset_class)) +
  ggridges::geom_density_ridges(
    alpha = 0.75,
    colour = "white",
    linewidth = 1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6, alpha = 0.5) +
  scale_fill_manual(values = asset_palette, guide = "none") +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Return distributions by asset class",
    subtitle = "Density curves show different shapes across classes",
    x = "1 year price return",
    y = NULL,
  )

ggsave("charts/exploratory_2_return_distribution.png", p_e2,
       width = 10, height = 6, dpi = 300)



p_e3 <- df %>%
  filter(quoteType == "EQUITY", !is.na(sector)) %>%
  count(sector) %>%
  mutate(
    sector = fct_reorder(sector, n),
    pct = n / sum(n) * 100
  ) %>%
  ggplot(aes(area = n, fill = n, label = paste0(sector, "\n(", n, ")"))) +
  geom_treemap(colour = "white", size = 2.5) +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    size = 10,
    fontface = "bold"
  ) +
  scale_fill_gradient(low = "#B8E0D2", high = "#1f4e79", guide = "none") +
  labs(
    title    = "Equities composition by sector",
    subtitle = "Size = number of companies, colour = intensity",
  )

ggsave("charts/exploratory_3_sector_counts.png", p_e3,
       width = 10, height = 6, dpi = 300)


p_e4 <- df %>%
  filter(!is.na(volatility_1y_ann), volatility_1y_ann < 200) %>%
  mutate(asset_class = fct_reorder(asset_class, volatility_1y_ann, .fun = median)) %>%
  ggplot(aes(x = asset_class, y = volatility_1y_ann, fill = asset_class)) +
  geom_violin(alpha = 0.75, colour = NA) +
  geom_boxplot(width = 0.2, fill = "white", colour = "grey40", alpha = 0.8) +
  scale_fill_manual(values = asset_palette, guide = "none") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_flip() +
  labs(
    title    = "Volatility distributions by asset class",
    subtitle = "Violin shows full distribution and box shows quartiles",
    x = NULL,
    y = "Annualised volatility",
  )

ggsave("charts/exploratory_4_volatility_boxplot.png", p_e4,
       width = 10, height = 6, dpi = 300)



label_set <- c("SOXL", "FSLY", "JNJ", "TD", "LRCX", "BTC-USD", "ETH-USD", "GC=F")

p_x1 <- df %>%
  filter(!is.na(volatility_1y_ann), !is.na(return_1y_pct),
         volatility_1y_ann < 150, between(return_1y_pct, -100, 300)) %>%
  ggplot(aes(x = volatility_1y_ann, y = return_1y_pct, colour = asset_class)) +

  stat_density_2d(
    geom = "contour",
    alpha = 0.1,
    colour = "grey70",
    bins = 5
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, alpha = 0.3) +
  geom_vline(xintercept = median(df$volatility_1y_ann, na.rm = TRUE), 
             linetype = "dotted", linewidth = 0.5, alpha = 0.3) +
  geom_point(alpha = 0.7, size = 3, stroke = 0.5) +
  geom_text_repel(
    data = . %>% filter(ticker %in% label_set),
    aes(label = ticker),
    colour = "black",
    size = 3.5,
    fontface = "bold",
    min.segment.length = 0,
    max.overlaps = Inf,
    box.padding = 0.5,
    segment.colour = "grey70"
  ) +
  scale_colour_manual(values = asset_palette) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Risk vs return across 451 global instruments",
    subtitle = "Density contours show clustering and higher vol = wider spread",
    x = "Annualised volatility",
    y = "1 year price return",
    colour = "Asset class",
  ) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4, stroke = 1)))

ggsave("charts/explanatory_1_risk_return_scatter.png", p_x1,
       width = 11, height = 7, dpi = 300)
