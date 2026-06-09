# Import necessary libraries.
library(dplyr)
library(mia) # need this for grouping by genus later.
library(tibble)
library(tidyr)


# Load data.
data <- read.csv("arch_master_thesis/data/concatenated_metaDMGfinal.tsv", sep = "\t")

# Rename the sample column.
colnames(data)[colnames(data) == "filename"] <- "sample"

# Shorten sample names. gsub replaces first argument with second argument.
# So /scratch/s5986052/metaDMG-stuff/lca_outputs/UU0148_aggregated_results.stat
# becomes UU0148.
data$sample <- gsub(
  "/scratch/s5986052/metaDMG-stuff/lca_outputs/",
  "",
  data$sample
)
data$sample <- gsub(
  "_aggregated_results.stat",
  "",
  data$sample
)

# Rename sample UU0408 control.
data$sample[data$sample == "UU0408"] <- "Control"

# Keep only observations determined to genus.
all_data <- data |> filter(
  rank == "genus"
)

# Keep only observations where rank is genus and within damage thresholds.
anc_data <- data |> filter(
  rank == "genus",
  A > 0.1,
  nreads > 50,
  mean_rlen > 35
)

control_set <- data |> filter(
  sample == "Control",
  nreads > 50,
  mean_rlen > 35
)

anc_data <- rbind(anc_data, control_set)

# Keep only sample, taxid and aggregate nreads columns.
all_data <- all_data |>
  group_by(sample, taxid) |>
  summarise(
    nreads = sum(nreads)
  ) |>
  ungroup()

anc_data <- anc_data |>
  group_by(sample, taxid) |>
  summarise(
    nreads = sum(nreads)
  ) |>
  ungroup()

# Transpose samples to col_names and taxid to rownames.
all_data <- all_data |>
  pivot_wider(
    names_from = sample,
    values_from = nreads
  ) |>
  mutate(across(-taxid, ~ replace_na(.x, 0)))

anc_data <- anc_data |>
  pivot_wider(
    names_from = sample,
    values_from = nreads
  ) |>
  mutate(across(-taxid, ~ replace_na(.x, 0)))

# Rename taxid to "OTU".
colnames(all_data)[colnames(all_data) == "taxid"] <- "#OTU ID"
colnames(anc_data)[colnames(anc_data) == "taxid"] <- "#OTU ID"

readr::write_tsv(all_data, "arch_master_thesis/data/OTUs/all_data.csv")
readr::write_tsv(anc_data, "arch_master_thesis/data/OTUs/anc_data.csv")
