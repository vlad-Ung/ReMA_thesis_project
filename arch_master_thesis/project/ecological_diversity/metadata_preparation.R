# Import necessary libraries.
library(dplyr)

# Load feature tables.
all_data <- read.csv("arch_master_thesis/data/OTUs/all_data.csv", sep = "\t")
anc_data <- read.csv("arch_master_thesis/data/OTUs/anc_data.csv", sep = "\t")

# Extract sample names:
all_meta <- as.data.frame(colnames(all_data)) |>
  filter(`colnames(all_data)` != "X.OTU.ID") |>
  dplyr::rename(sample_id = `colnames(all_data)`) |>
  mutate(
    Site = case_when(
      sample_id %in% c(
        "UU0148", "UU0149", "UU0150",
        "UU0151", "UU0152", "UU0153", "UU0154"
      ) ~ "Harelbeke",
      sample_id == "UU0291" ~ "HeHem",
      sample_id %in% c("UU0393", "UU0396", "UU0398") ~ "HEEN24",
      sample_id == "Control" ~ "Control"
    )
  )

# And the same for anc_data:
anc_meta <- as.data.frame(colnames(anc_data)) |>
  filter(`colnames(anc_data)` != "X.OTU.ID") |>
  dplyr::rename(sample_id = `colnames(anc_data)`) |>
  mutate(
    Site = case_when(
      sample_id %in% c(
        "UU0148", "UU0149", "UU0150",
        "UU0151", "UU0152", "UU0153", "UU0154"
      ) ~ "Harelbeke",
      sample_id == "UU0291" ~ "HeHem",
      sample_id %in% c("UU0393", "UU0396", "UU0398") ~ "HEEN24",
      sample_id == "Control" ~ "Control"
    )
  )

colnames(all_meta)[colnames(all_meta) == "sample_id"] <- "#SampleID"
colnames(anc_meta)[colnames(anc_meta) == "sample_id"] <- "#SampleID"


# Export.
readr::write_tsv(all_meta, "arch_master_thesis/data/OTUs/all_meta.csv")
readr::write_tsv(anc_meta, "arch_master_thesis/data/OTUs/anc_meta.csv")
