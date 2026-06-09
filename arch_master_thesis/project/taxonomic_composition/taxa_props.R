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
  grepl("\\genus\\b", rank)
)

# Add control sample back to the set
# (it got filtered out by authentication thresholding).
control_set <- df |> filter(
  nreads > 50,
  sample == "Control"
)
set_1 <- rbind(set_1, control_set)

# Add taxonomic grouping columns.
set_1 <- set_1 |> mutate(
  tax_group = case_when(
    grepl("Viridiplant", taxa_path) ~ "Viridiplantae",
    grepl("Metazoa", taxa_path) ~ "Metazoa",
    grepl("Bacteria", taxa_path) ~ "Bacteria",
    grepl("Archaea", taxa_path) ~ "Archaea",
    grepl("Fungi", taxa_path) ~ "Fungi"
  )
)

# Adds per sample taxonomic counts to new column.
# Then per sample taxonomic %.
set_1 <- set_1 |> 
  group_by(sample, tax_group) |> 
  summarise(nreads = sum(nreads), .groups = "drop_last") |> 
  mutate(props = nreads / sum(nreads)) |> 
  ungroup()

set_1$tax_group <- as.factor(set_1$tax_group) # needed for graph reordering

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

# Plots.
p <- set_1 |>
  mutate(tax_group = fct_reorder(tax_group, -props)) |>
  ggplot(aes(x = sample, y = props)) +
  geom_col(stat = "identity", position = "dodge", aes(fill = tax_group)) +
  xlab(NULL) +
  ylab("%") +
  labs(title = "Taxonomic group proportions per sample",
    fill = "Taxonomic group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) +
  scale_fill_manual(values = c(
    "Viridiplantae" = "green4",
    "Metazoa" = "#E31A1C",
    "Bacteria" = "#FF7F00",
    "Archaea" = "#6A3D9A",
    "Fungi" = "gold1"
  )
  )
ggsave("arch_master_thesis/outputs/taxa_props.png", plot = p, width = 8, height = 6,
  units = "in", dpi = 300
)

q <- set_1 |>
  mutate(tax_group = fct_reorder(tax_group, -props)) |>
  ggplot(aes(x = sample, y = nreads)) +
  geom_col(stat = "identity", position = "dodge", aes(fill = tax_group)) +
  xlab(NULL) +
  labs(title = "Taxonomic group reads per sample",
    fill = "Taxonomic group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) +
  scale_fill_manual(values = c(
    "Viridiplantae" = "green4",
    "Metazoa" = "#E31A1C",
    "Bacteria" = "#FF7F00",
    "Archaea" = "#6A3D9A",
    "Fungi" = "gold1"
  ))
ggsave("arch_master_thesis/outputs/taxa_nreads.png",
  plot = q, width = 8, height = 6,
  units = "in", dpi = 300
)
