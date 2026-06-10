library(dplyr)
library(data.table)
library(Hmisc)
library(corrplot)

# Function to flatten the correlation matrix in a long format for reporting.
flatten_rect <- function(cormat, pmat) {
  expand.grid(row = rownames(cormat), column = colnames(cormat)) |>
    mutate(
      cor = as.vector(cormat),
      p = as.vector(pmat),
      p_adj = p.adjust(pmat, method = "BH")
    )
}

# Load pre-prepared data subsets.
pathogens <- read.csv("arch_master_thesis/data/OTUs/pathogen_OTU.csv", sep = "\t")
gi_microbiota <- read.csv("arch_master_thesis/data/OTUs/gi_microbiota_OTU.csv", sep = "\t")
metazoans <- read.csv("arch_master_thesis/data/OTUs/metazoans_OTU.csv", sep = "\t")

# Add common category column to merge dataframes.
pathogens$category <- "pathogen"
gi_microbiota$category <- "gi_microbe"
metazoans$category <- "metazoan"

data <- bind_rows(pathogens, gi_microbiota, metazoans)
data[is.na(data)] <- 0

# Drop category column.
data <- data |> dplyr::select(-category)

# Transpose.
data_transposed <- transpose(data[, -1])
rownames(data_transposed) <- colnames(data)[-1]
colnames(data_transposed) <- data$name

# Spearman correlation.
res <- rcorr(as.matrix(data_transposed), type = "spearman")

# Pathogens vs GI microbiota.
cor_path_gi <- res$r[pathogens$name, gi_microbiota$name]
p_path_gi <- res$P[pathogens$name, gi_microbiota$name]

# Pathogens vs Metazoans.
cor_path_meta <- res$r[pathogens$name, metazoans$name]
p_path_meta <- res$P[pathogens$name, metazoans$name]

# Flatten correlation matrices to export.
flat_path_gi <- flatten_rect(cor_path_gi, p_path_gi) |>
  filter(p_adj < 0.05)
flat_path_meta <- flatten_rect(cor_path_meta, p_path_meta) |>
  filter(p_adj < 0.05)

readr::write_tsv(flat_path_gi, "arch_master_thesis/outputs/cor_path_gi.csv")
readr::write_tsv(flat_path_meta, "arch_master_thesis/outputs/cor_path_meta.csv")


# Make matrix for adjusted p values to mask insignificant correlations.
p_adj_path_gi <- matrix(
  p.adjust(as.vector(p_path_gi), method = "BH"),
  nrow = nrow(p_path_gi),
  ncol = ncol(p_path_gi),
  dimnames = dimnames(p_path_gi)
)

p_adj_path_meta <- matrix(
  p.adjust(as.vector(p_path_meta), method = "BH"),
  nrow = nrow(p_path_meta),
  ncol = ncol(p_path_meta),
  dimnames = dimnames(p_path_meta)
)

png("arch_master_thesis/outputs/corrplot_path_gi.png",
  width = 1500, height = 1300, res = 150
)
corrplot(cor_path_gi,
  p.mat     = p_adj_path_gi,
  sig.level = 0.05,
  insig     = "blank",
  method    = "circle",
  title     = "Pathogen candidates vs. GI associated microbiota",
  mar       = c(1, 1, 2, 1)
)
dev.off()

png("arch_master_thesis/outputs/corrplot_path_meta.png",
  width = 1300, height = 1300, res = 150
)
corrplot(cor_path_meta,
  p.mat = p_adj_path_meta,
  sig.level = 0.05,
  insig = "blank",
  method = "circle",
  title = "Pathogen candidates vs. Metazoans",
  mar = c(1, 1, 2, 1),
  cl.ratio = 0.5, # legend width relative to plot; default is 0.2, try larger
  cl.cex = 0.8
)
dev.off()
