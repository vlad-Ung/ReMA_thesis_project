# Load packages.
library(tidyr)
library(dplyr)
library(forcats)
library(scales)
library(gridExtra)
library(ggplot2)
library(purrr)
library(ggpubr)
library(stringr)
library(tibble)

# Load dataframe. I keep absolute file paths so I don't have to keep moving
# files between /home6 and /scratch.
df <- read.csv("arch_master_thesis/data/concatenated_metaDMGfinal.tsv", sep = "\t")

# Rename the sample column.
colnames(df)[colnames(df) == "filename"] <- "sample"

# Shorten sample names. gsub replaces first argument with second argument.
# So /scratch/s5986052/metaDMG-stuff/lca_outputs/UU0148_aggregated_results.stat
# becomes UU0148.
df$sample <- gsub("/scratch/s5986052/metaDMG-stuff/lca_outputs/", "", df$sample)
df$sample <- gsub("_aggregated_results.stat", "", df$sample)

# Rename sample UU0408 control.
df$sample[df$sample == "UU0408"] <- "Control"

# Subset based on authentication metrics:
set_1 <- df |> filter(
  A > 0.1, mean_rlen > 35, nreads > 50,
  grepl("\\bspecies\\b", rank)
)

# subset control to add it manually to set_1.
control_set <- df |> filter(
  sample == "Control",
  nreads > 50,
  mean_rlen > 35
)

# Append control_set to set_1.
set_1 <- rbind(set_1, control_set)

# Load bacterial pathogens dataframe.
df_bacteria <- read.csv(
  "arch_master_thesis/data/bacteria_human_pathogens.csv", sep = ","
)
# Remove reference column (too long, dangerous characters, maybe encoding errors).
df_bacteria <- df_bacteria[, -c(10)]

# Extract bacterial pathogen vector to use as filter later.
bacteria <- paste0(df_bacteria$genus, " ",
  df_bacteria$species
)
# Remove duplicates
bacteria <- unique(bacteria)

# Extract helminthes/nematodes pathogen vector to use as filter later.
helm_nema <- c(
  "Ascaris lumbricoides", "Trichiuris trichiuria",
  "Ancylostoma duodenale", "Necator americanicus",
  "Enterobius vermicularis", "Strongyloides stercoralis",
  "Taenia solium", "Trichinella spiralis"
)

# Extract bacterial pathogen vector to use as filter later.
protozoa <- c(
  "Entamoeba histolytica", "Giardia intestinalis",
  "Cyclospora cayetanenensis", "Cryptosporidium parvum",
  "Cryptosporidium hominis", "Cryptosporidium canis",
  "Cryptosporidium felis", "Cryptosporidium meleagridis",
  "Cryptosporidium muris"
)

# Make combined filter vector with bacteria, helmithes, nematodes and protozoan
# pathogens.
pathogens <- append(bacteria, helm_nema) |> 
  append(protozoa)

# Filter for pathoenic species.
set_1 <- set_1 |> filter(
  name %in% pathogens
)

# Summarise.
set_1 <- set_1 |>
  group_by(sample, name) |>
  summarise(nreads = sum(nreads), .groups = "drop_last") |>
  mutate(props = nreads / sum(nreads)) |>
  ungroup()

# Pivot to wide matrix for clustering.
mat <- set_1 |>
  select(sample, name, props) |>
  pivot_wider(names_from = name, values_from = props, values_fill = 0) |>
  column_to_rownames("sample") |>
  as.matrix()

# Cluster samples and taxa. Similar samples in terms of taxonomic composition
# stay close to each other.
sample_order <- hclust(dist(mat))$order
taxon_order <- hclust(dist(t(mat)))$order # t(mat) transposes the columns

# Pull ordered levels
taxon_levels <- colnames(mat)[taxon_order]

# Put Control at the end in sample levels.
sample_levels <- c(
  "UU0148", "UU0149", "UU0150", "UU0151",
  "UU0152", "UU0153", "UU0154", "UU0291",
  "UU0393", "UU0396", "UU0398", "Control"
)

# Apply ordering to long-format data
set_1_ordered <- set_1 |>
  mutate(
    sample = factor(sample, levels = sample_levels),
    name   = factor(name, levels = sort(taxon_levels, decreasing = TRUE))
  )

# Plot
p <- ggplot(set_1_ordered, aes(x = sample, y = name, fill = props)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = viridis::viridis(10), # 10% increments
    labels  = scales::percent,
    name    = "Relative\nabundance"
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y  = element_text(size = 8),
    legend.text  = element_text(size = 8),
    panel.grid   = element_blank()
  ) +
  labs(title = "Ancient GI pathogen abundances across samples")
ggsave("arch_master_thesis/outputs/GI_bacteria_path_props.png",
  plot = p, width = 190,
  units = "mm", dpi = 300
)

# Pivot to wide matrix for clustering.
mat <- set_1 |>
  select(sample, name, nreads) |>
  pivot_wider(names_from = name, values_from = nreads, values_fill = 0) |>
  column_to_rownames("sample") |>
  as.matrix()

# Cluster samples and taxa. Similar samples in terms of taxonomic composition
# stay close to each other.
sample_order <- hclust(dist(mat))$order
taxon_order <- hclust(dist(t(mat)))$order # t(mat) transposes the columns

# Pull ordered levels
taxon_levels <- colnames(mat)[taxon_order]

# Put Control at the end in sample levels.
sample_levels <- c(
  "UU0148", "UU0149", "UU0150", "UU0151",
  "UU0152", "UU0153", "UU0154", "UU0291",
  "UU0393", "UU0396", "UU0398", "Control"
)

# Apply ordering to long-format data
set_1_ordered <- set_1 |>
  mutate(
    sample = factor(sample, levels = sample_levels),
    name   = factor(name, levels = sort(taxon_levels, decreasing = TRUE))
  )

# Plot
p <- ggplot(set_1_ordered, aes(x = sample, y = name, fill = nreads)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = viridis::viridis(10), # 10% increments
    name = "Number of\nreads"
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y  = element_text(size = 8),
    legend.text  = element_text(size = 8),
    panel.grid   = element_blank()
  ) +
  labs(title = "Ancient GI pathogen reads across samples")
ggsave("arch_master_thesis/outputs/GI_bacteria_path_nreads.png",
  plot = p, width = 190,
  units = "mm", dpi = 300
)
length(unique(set_1_ordered$name))
