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

# Rename sample UU0408 control.
df$sample[df$sample == "UU0408"] <- "Control"

# Subset data.
set_1 <- df |> filter(
  A > 0.1, mean_rlen > 35, nreads > 50,
  grepl("\\genus\\b", rank),
  grepl("Viridiplant", taxa_path)
)

# Add control sample back to the set
# (it got filtered out by authentication thresholding).
control_set <- df |> filter(
  nreads > 50,
  sample == "Control",
  grepl("Viridiplant", taxa_path)
)
set_1 <- rbind(set_1, control_set)

# Summarise.
set_1 <- set_1 |>
  group_by(sample, name) |>
  summarise(nreads = sum(nreads), .groups = "drop_last") |>
  mutate(props = nreads / sum(nreads)) |>
  ungroup()

set_1$name <- as.factor(set_1$name) # needed for graph reordering

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
sample_levels <- rownames(mat)[sample_order]
taxon_levels <- colnames(mat)[taxon_order]

# Put Control at the end in sample levels.
# Check the sample_levels vector first to get the order.
sample_levels <- c(
  "UU0148", "UU0149", "UU0150", "UU0151",
  "UU0152", "UU0153", "UU0154", "UU0291",
  "UU0393", "UU0396", "UU0398", "Control"
)

# Apply ordering to long-format data
set_1_ordered <- set_1 |>
  mutate(
    sample = factor(sample, levels = sample_levels),
    name   = factor(name, levels = taxon_levels)
  )

# Plot
p <- ggplot(set_1_ordered, aes(x = sample, y = name, fill = nreads)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = viridis::viridis(10), # 10% increments
    #labels  = scales::percent,
    name    = "Number of\nreads"
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y  = element_text(size = 8),
    legend.text  = element_text(size = 8),
    panel.grid   = element_blank()
  ) +
  labs(title = "Plant reads across samples + control")
ggsave("outputs/plant_nreads.png",
  plot = p, dpi = 300
)
