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
human <- df |> filter(
  A > 0.1, mean_rlen > 35, nreads > 50,
  grepl("\\Homo\\b", name),
  grepl("Metazoa", taxa_path)
)

# Add control sample back to the set
# (it got filtered out by authentication thresholding).
control_set <- df |> filter(
  nreads > 50,
  sample == "Control",
  grepl("Metazoa", taxa_path)
)
# set_1 <- rbind(set_1, control_set)