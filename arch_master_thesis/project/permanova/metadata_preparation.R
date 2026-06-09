# Import necessary libraries.
library(dplyr)

files <- c(
  "arch_master_thesis/data/OTUs/OTU_microbiota.csv",
  "arch_master_thesis/data/OTUs/OTU_pathogens.csv"
)

i <- 1

for (file in files) {
  data <- read.csv(file, sep = "\t")

  # And the same for anc_data:
  meta <- as.data.frame(colnames(data)) |>
    filter(`colnames(data)` != "X.OTU.ID") |>
    dplyr::rename(sample_id = `colnames(data)`) |>
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

  colnames(meta)[colnames(meta) == "sample_id"] <- "#SampleID"

  # Export.
  if (i == 1) {
    readr::write_tsv(meta, "arch_master_thesis/data/OTUs/microbiota_meta.csv")
    i <- 2
  } else {
    readr::write_tsv(meta, "arch_master_thesis/data/OTUs/pathogen_meta.csv")
  }
}
