# Load packages.
library(tidyr)
library(dplyr)
library(forcats)
library(scales)
library(gridExtra)
library(ggplot2)
library(purrr)
library(ggpubr)

# Load dataframe. I keep absolute file paths so I don't have to keep
# moving files between /home6 and /scratch.
df <- read.csv("arch_master_thesis/data/concatenated_metaDMGfinal.tsv", sep = "\t")

# Rename the sample column.
colnames(df)[colnames(df) == "filename"] <- "sample"

# Shorten sample names. gsub replaces first argument with second argument.
# So /scratch/s5986052/metaDMG-stuff/lca_outputs/UU0148_aggregated_results.stat
# becomes UU0148.
df$sample <- gsub("/scratch/s5986052/metaDMG-stuff/lca_outputs/", "", df$sample)
df$sample <- gsub("_aggregated_results.stat", "", df$sample)

# This is an example block for working with subsets.
# Subset at genus level.
set_1 <- df |> filter(
  nreads > 50,
  grepl("\\bgenus\\b", rank), # where genus is in rank.
)

set_1 <- set_1 |> mutate(
  tax_group = case_when(
    grepl("Viridiplant", taxa_path) ~ "Viridiplantae",
    grepl("Metazoa", taxa_path) ~ "Metazoa",
    grepl("Bacteria", taxa_path) ~ "Bacteria",
    grepl("Archaea", taxa_path) ~ "Archaea",
    grepl("Fungi", taxa_path) ~ "Fungi"
  )
)

# Source - https://stackoverflow.com/a/9568659
# Posted by Kevin Wright, modified by community. See post 'Timeline' for change history
# Retrieved 2026-04-28, License - CC BY-SA 4.0

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

# # Plot.
p <- ggplot(set_1, aes(y = A, x = Zfit)) +
  geom_point(aes(size = nreads, col = tax_group)) +
  geom_vline(xintercept = 2, linetype = "dashed") +
  geom_hline(yintercept = 0.1, linetype = "dashed") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_color_manual(values = c(
    "Viridiplantae" = "green4",
    "Metazoa" = "#E31A1C",
    "Bacteria" = "#FF7F00",
    "Archaea" = "#6A3D9A",
    "Fungi" = "gold1"
  )
  ) +
  scale_size_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("significance") + 
  ylab("damage") +
  labs(title = "Damage amplitude vs. Significance",
    size = "Number of reads",
    colour = "Taxonomic group"
  )
print(p)
# Save plot.
ggsave("arch_master_thesis/outputs/A_Zfit.png", plot = p, width = 8, height = 6)
