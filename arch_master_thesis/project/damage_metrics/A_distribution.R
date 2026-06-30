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
df <- read.csv("arch_master_thesis/data/concatenated_metaDMGfinal.tsv", sep = "\t")

# Rename the sample column.
colnames(df)[colnames(df) == "filename"] <- "sample"

# Shorten sample names. gsub replaces first argument with second argument.
# So /scratch/s5986052/metaDMG-stuff/lca_arch_master_thesis/outputs/UU0148_aggregated_results.stat
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
  log10_A = log10(A)
)

# Look at dataset distribution to compare agaist taxonomic grouping.
# Log scale shows geometric mean for finite values onle!
# Because I have zeros values resulting in -Inf.
p <- ggplot(df, aes(x = log10_A)) + # there are na's.
  geom_density(fill = "lightblue") +
  stat_summary(aes(xintercept = ..x.., y = 0),
    fun = mean, geom = "vline",
    orientation = "y", color = "red", linetype = "dashed"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 10))
ggsave("arch_master_thesis/outputs/A_distribution.png", plot = p)

# Taxonomic group facet wrap.
p <- ggplot(df, aes(x = log10_A)) + # there are na's.
  geom_density(fill = "lightblue") +
  facet_wrap(~tax_group) +
  stat_summary(aes(xintercept = ..x.., y = 0),
    fun = mean, geom = "vline",
    orientation = "y", color = "red", linetype = "dashed"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 10))
ggsave("arch_master_thesis/outputs/A_taxonomic_distribution.png", plot = p)

# Site facet wrap.
p <- ggplot(df, aes(x = log10_A)) + # there are na's.
  geom_density(fill = "lightblue") +
  facet_wrap(~site) +
  stat_summary(aes(xintercept = ..x.., y = 0),
    fun = mean, geom = "vline",
    orientation = "y", color = "red", linetype = "dashed"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 10))
ggsave("arch_master_thesis/outputs/A_site_distribution.png", plot = p)


summ <- na.omit(df) |>
  group_by(site, tax_group) |>
  summarise(
    mean_A = mean(A),
    sd_A = sd(A),
    median_A = median(A),
    geom_mean = 10^mean(log10_A),
    p90_A = quantile(A, 0.90),
    p99_A = quantile(A, 0.99),
    frac_below_mean = mean(A < mean(A)),
    frac_below_10 = mean(A < 0.1),
    frac_below_5 = mean(A < 0.05),
    frac_below_2 = mean(A < 0.02),
    frac_below_geom_mean = mean(log10(A) < mean(log10(A)))
  )
write.csv(summ, "arch_master_thesis/outputs/A_summary.csv")
