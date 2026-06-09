# Load packages.
library(tidyr)
library(dplyr)
library(forcats)
library(scales)
library(gridExtra)
library(ggplot2)
library(purrr)
library(ggpubr)
source("project/get_dmg_decay_fit.R")

# Hyperparameters
minDMG <- 0.1 # taxa above set value
zfit <- 2 # minimum significance
MinLength <- 35 # minimum mean readlength
reads <- 50 # number of reads required

# Load dataframe. I keep absolute file paths so I don't have to keep moving
# files between /home6 and /scratch.
df <- read.csv("data/concatenated_metaDMGfinal.tsv", sep = "\t")

# Rename the sample column.
colnames(df)[colnames(df) == "filename"] <- "sample"

# Shorten sample names. gsub replaces first argument with second argument.
# So /scratch/s5986052/metaDMG-stuff/lca_outputs/UU0148_aggregated_results.stat
# becomes UU0148.
df$sample <- gsub("/scratch/s5986052/metaDMG-stuff/lca_outputs/", "", df$sample)
df$sample <- gsub("_aggregated_results.stat", "", df$sample)

set_1 <- df |> filter(
  A > minDMG, nreads >= reads, mean_rlen >= MinLength,
  #Zfit > zfit,
  grepl("\\bgenus\\b", rank)
)

# deamination plot with facet wrap per each taxon in a sample
tax_g_list <- unique(set_1$name)
nrank <- "rank" # Replace with the actual rank column name

# TODO: study this block in detail and learn what it does.
X <- tax_g_list
purrr::map(tax_g_list, function(X, nrank) {
  sel_tax <- set_1 |>
    rename(label = sample) |>
    filter(name == X) |>
    filter(rank == rank) |>
    select(name, label) |>
    distinct() |>
    arrange(name)
  if (nrow(sel_tax) > 0) {
    n_readsa <- set_1 |>
      inner_join(sel_tax) |>
      filter(rank == rank) |>
      pull(nreads) |>
      sum()
    ggpubr::ggarrange(plotlist = list(
      get_dmg_decay_fit(df = set_1 |> rename(label = sample) |>
        inner_join(sel_tax) |>
        filter(rank == rank), orient = "fwd", y_max = 0.70) +
        ggtitle(paste0(X, " nreads=", n_readsa, " Forward")),
      get_dmg_decay_fit(df = set_1 |> rename(label = sample) |>
        inner_join(sel_tax) |>
        filter(rank == rank), orient = "rev", y_max = 0.70) +
        ggtitle(paste0(X, " nreads=", n_readsa, " Reverse"))
    ), align = "hv")
    ggsave(paste0("outputs/figures/", X, "-dmg.pdf"), plot = last_plot())
  }
})
