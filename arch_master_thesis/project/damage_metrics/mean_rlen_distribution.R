# Load packages.
library(tidyr)
library(dplyr)
library(forcats)
library(scales)
library(gridExtra)
library(ggplot2)
library(purrr)
library(ggpubr)
library(pals)

# Load dataframe. I keep absolute file paths so I don't have to keep moving
# files between /home6 and /scratch.
df <- read.csv("data/concatenated_metaDMGfinal.tsv", sep = "\t")

# Rename the sample column.
colnames(df)[colnames(df) == "filename"] <- "sample"

# Shorten sample names. gsub replaces first argument with second argument.
# So /scratch/s5986052/metaDMG-stuff/lca_outputs/UU0148_aggregated_results.stat
# becomes UU0148.
df$sample <- gsub(
  "/scratch/s5986052/metaDMG-stuff/lca_outputs/",
  "",
  df$sample
)
df$sample <- gsub(
  "_aggregated_results.stat",
  "",
  df$sample
)

# Add taxonomic grouping columns.
df <- df |> mutate(
  tax_group = case_when(
    grepl("Viridiplant", taxa_path) ~ "Viridiplantae",
    grepl("Metazoa", taxa_path) ~ "Metazoa",
    grepl("Bacteria", taxa_path) ~ "Bacteria",
    grepl("Archaea", taxa_path) ~ "Archaea",
    grepl("Fungi", taxa_path) ~ "Fungi"
  ),
  site = case_when(
    sample %in% c(
      "UU0148", "UU0149", "UU0150", "UU0151", "UU0152",
      "UU0153", "UU0154"
    ) ~ "Harelbeke",
    sample == "UU0291" ~ "Herwen-Hemeling",
    sample %in% c("UU0393", "UU0396", "UU0398") ~ "Heerlen",
    sample == "UU0408" ~ "Control"
  )
)

df <- df |> mutate(
  log10_mean_rlen = log10(mean_rlen)
)

# Look at dataset distribution to compare agaist taxonomic grouping.
p <- ggplot(df, aes(x = log10_mean_rlen)) + # there are na's.
  geom_density(fill = "lightblue") +
  stat_summary(aes(xintercept = ..x.., y = 0),
    fun = mean, geom = "vline",
    orientation = "y", color = "red", linetype = "dashed"
  ) +
  theme_minimal() +
  labs(title = "Distribution of mean_rlen across dataset, log scale")
ggsave("outputs/mean_rlen_distribution.png", plot = p)

# Look at dataset distribution to compare agaist taxonomic grouping.
p <- ggplot(df, aes(x = log10_mean_rlen)) + # there are na's.
  geom_density(fill = "lightblue") +
  facet_wrap(~tax_group) +
  stat_summary(aes(xintercept = ..x.., y = 0),
    fun = mean, geom = "vline",
    orientation = "y", color = "red", linetype = "dashed"
  ) +
  theme_minimal() +
  labs(title = "Distribution of mean_rlen across taxonomic groups, log scale")
ggsave("outputs/mean_rlen_taxonomic_distribution.png", plot = p)

# Look at dataset distribution to compare agaist taxonomic grouping.
p <- ggplot(df, aes(x = log10_mean_rlen)) + # there are na's.
  geom_density(fill = "lightblue") +
  facet_wrap(~site) +
  stat_summary(aes(xintercept = ..x.., y = 0),
    fun = mean, geom = "vline",
    orientation = "y", color = "red", linetype = "dashed"
  ) +
  theme_minimal() +
  labs(title = "Distribution of mean_rlen across sites, log scale")
ggsave("outputs/mean_rlen_site_distribution.png", plot = p)

summ <- df |>
  group_by(site, tax_group) |> 
  summarise(
    mean = mean(mean_rlen),
    sd = sd(mean_rlen),
    median = median(mean_rlen),
    geom_mean = 10^mean(log10_mean_rlen),
    p90 = quantile(mean_rlen, 0.90),
    p99 = quantile(mean_rlen, 0.99),
    frac_below_mean = mean(mean_rlen < mean(mean_rlen)),
    frac_below_35 = mean(mean_rlen < 35),
    frac_below_50 = mean(mean_rlen < 50),
    frac_below_70 = mean(mean_rlen < 70),
    frac_below_100 = mean(mean_rlen < 100),
    frac_below_geom_mean = mean(log10_mean_rlen < mean(log10_mean_rlen))
  )
write.csv(summ, "outputs/mean_rlen_summary.csv")
