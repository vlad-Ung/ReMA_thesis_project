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

# Add log10_nreads column.
df <- df |> mutate(
  log10_nreads = log10(nreads)
)

# Look at dataset distribution to compare agaist taxonomic grouping.
p <- ggplot(df, aes(x = log10_nreads)) + 
  geom_density(fill = "lightblue") +
  stat_summary(aes(xintercept = ..x.., y = 0),
    fun = mean, geom = "vline",
    orientation = "y", color = "red", linetype = "dashed"
  ) +
  theme_minimal() +
  labs(title = "Distribution of nreads across dataset, log scale")
ggsave("outputs/nreads_distribution.png", plot = p)

# Look at dataset distribution to compare agaist taxonomic grouping.
p <- ggplot(df, aes(x = log10_nreads)) +
  geom_density(fill = "lightblue") +
  facet_wrap(~tax_group) +
  stat_summary(aes(xintercept = ..x.., y = 0),
    fun = mean, geom = "vline",
    orientation = "y", color = "red", linetype = "dashed"
  ) +
  theme_minimal() +
  labs(title = "Distribution of nreads across taxonomic groups, log scale")
ggsave("outputs/nreads_taxonomic_distribution.png", plot = p)

# Look at dataset distribution to compare agaist taxonomic grouping.
p <- ggplot(df, aes(x = log10_nreads)) +
  geom_density(fill = "lightblue") +
  facet_wrap(~site) +
  stat_summary(aes(xintercept = ..x.., y = 0),
    fun = mean, geom = "vline",
    orientation = "y", color = "red", linetype = "dashed"
  ) +
  theme_minimal() +
  labs(title = "Distribution of nreads across sites, log scale")
ggsave("outputs/nreads_site_distribution.png", plot = p)

summ <- df |>
  group_by(site, tax_group) |> 
  summarise(
    mean_nreads   = mean(nreads),
    sd = sd(nreads),
    median_nreads = median(nreads),
    geom_mean = 10^mean(log10_nreads),
    p90_nreads    = quantile(nreads, 0.90),
    p99_nreads    = quantile(nreads, 0.99),
    frac_below_mean = mean(nreads < mean(nreads)),
    frac_below_geom_mean = mean(log10_nreads < mean(log10_nreads)),
    frac_below_100 = mean(nreads < 100),
    frac_below_50 = mean(nreads < 50)
  )
write.csv(summ, "outputs/nreads_summary.csv")
