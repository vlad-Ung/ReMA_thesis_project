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

# Subset based on authentication metrics:
set_1 <- data |> filter(
  A > 0.1, mean_rlen > 35, nreads > 50,
  grepl("\\bspecies\\b", rank)
)

# subset control to add it manually to set_1.
control_set <- data |> filter(
  sample == "Control",
  nreads > 50,
  mean_rlen > 35
)

# Append control_set to set_1.
set_1 <- rbind(set_1, control_set)

# Load bacterial pathogens dataframe.
df_bacteria <- read.csv(
  "arch_master_thesis/data/bacteria_human_pathogens.csv",
  sep = ","
)
# Remove reference column (too long, dangerous characters, maybe encoding errors).
df_bacteria <- df_bacteria[, -c(10)]

# Extract bacterial pathogen vector to use as filter later.
bacteria <- paste0(
  df_bacteria$genus, " ",
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

anc_data <- set_1 |>
  group_by(sample, taxid) |>
  summarise(
    nreads = sum(nreads)
  ) |>
  ungroup()

anc_data <- anc_data |>
  pivot_wider(
    names_from = sample,
    values_from = nreads
  ) |>
  mutate(across(-taxid, ~ replace_na(.x, 0)))

# Rename taxid to "OTU".
colnames(anc_data)[colnames(anc_data) == "taxid"] <- "#OTU ID"

readr::write_tsv(anc_data, "arch_master_thesis/data/OTUs/OTU_pathogens.csv")
