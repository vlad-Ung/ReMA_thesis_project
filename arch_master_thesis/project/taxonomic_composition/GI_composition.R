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
df <- read.csv("arch_master_thesis/data/concatenated_metaDMGfinal.tsv", sep = "\t")

# Rename the sample column.
colnames(df)[colnames(df) == "filename"] <- "sample"

# Shorten sample names. gsub replaces first argument with second argument.
# So /scratch/s5986052/metaDMG-stuff/lca_outputs/UU0148_aggregated_results.stat
# becomes UU0148.
df$sample <- gsub("/scratch/s5986052/metaDMG-stuff/lca_outputs/", "", df$sample)
df$sample <- gsub("_aggregated_results.stat", "", df$sample)

# Rename sample UU0408 control.
df$sample[df$sample == "UU0408"] <- "Control"

# Set taxa vector to filter by:
taxa_vector <- c(
  "Absiella", "Acetobacter", "Achromobacter", "Acidaminococcus",
  "Actinomyces", "Adlercreutzia", "Aeromonas", "Agathobaculum",
  "Aggregatibacter", "Akkermansia", "Alistipes", "Allisonella",
  "Alloprevotella", "Anaerobutyricum", "Anaerococcus", "Anaerofilum",
  "Anaerofustis", "Anaeroglobus", "Anaeromassilibacillus", "Anaerostipes",
  "Anaerotignum", "Anaerotruncus", "Atopobium", "Azospirillum", "Bacteroides",
  "Barnesiella", "Bifidobacterium", "Bilophila", "Blastocystis", "Blautia",
  "Brachyspira", "Butyricimonas", "Butyrivibrio", "Caecibacter", "Campylobacter",
  "Candidatus Cibiobacter", "Candidatus Gastranaerophilus",
  "Candidatus Methanomethylophilus", "Catabacter", "Catenibacterium",
  "Christensenella", "Citrobacter", "Cloacibacillus", "Clostridioides",
  "Clostridium", "Colibacter", "Collinsella", "Comamonas",
  "Coprobacillus", "Coprobacter", "Coprococcus", "Coraliomargarita",
  "Cryptobacterium", "Culturomica", "Dakarella", "Desulfovibrio",
  "Dialister", "Dielma", "Dorea", "Drancourtella", "Duodenibacillus",
  "Eggerthella", "Eisenbergiella", "Emergencia", "Enorma", "Enterobacter",
  "Enterococcus", "Enteroscipio", "Erysipelatoclostridium", "Escherichia",
  "Evtepia", "Faecalibacterium", "Faecalicatena", "Faecalicoccus",
  "Faecalimonas", "Faecalitalea", "Fenollaria", "Flavonifractor",
  "Fournierella", "Fusicatenibacter", "Fusobacterium", "Gemella",
  "Gemmiger", "Gordonibacter", "Granulicatella", "Haemophilus", "Hafnia",
  "Helicobacter", "Holdemanella", "Holdemania", "Hungatella",
  "Intestinibacter", "Intestinimonas", "Klebsiella", "Kluyvera",
  "Lachnoanaerobaculum", "Lachnoclostridium", "Lachnospira", "Lactobacillus",
  "Lactococcus", "Lawsonibacter", "Leptotrichia", "Leuconostoc", "Libanicoccus",
  "Mailhella", "Marvinbryantia", "Massiliomicrobiota", "Mediterranea",
  "Megamonas", "Megasphaera", "Merdimonas", "Methanobrevibacter",
  "Methanomassiliicoccus", "Methanosphaera", "Mitsuokella", "Mobilibacterium",
  "Monoglobus", "Mordavella", "Morganella", "Negativibacillus", "Neglecta",
  "Niameybacter", "Odoribacter", "Olsenella", "Oribacterium", "Oscillibacter",
  "Oxalobacter", "Parabacteroides", "Paraeggerthella", "Paraprevotella",
  "Parasutterella", "Parvimonas", "Peptoniphilus", "Peptostreptococcus",
  "Phascolarctobacterium", "Phocea", "Plesiomonas", "Porphyromonas",
  "Prevotella", "Prevotellamassilia", "Proteus", "Providencia",
  "Pseudoflavonifractor", "Pyramidobacter", "Raoultella", "Rikenella",
  "Romboutsia", "Roseburia", "Rubneribacter", "Ruminiclostridium",
  "Ruminococcus", "Ruthenibacterium", "Saccharopolyspora",
  "Sanguibacteroides", "Selenomonas", "Sellimonas", "Senegalimassilia",
  "Slackia", "Solobacterium", "Sphaerochaeta", "Streptococcus",
  "Succinatimonas", "Sutterella", "Treponema", "Turicibacter",
  "Turicimonas", "Tyzzerella", "unclassified",
  "unclassified Acidaminococcaceae", "unclassified Akkermansiaceae",
  "unclassified Alphaproteobacteria", "unclassified Atopobiaceae",
  "unclassified Bacteroidales", "unclassified Barnesiellaceae",
  "unclassified Betaproteobacteria",
  "unclassified Candidatus Gastranaerophilales",
  "unclassified Christensenellaceae-Catabacteriaceae",
  "unclassified Clostridiales", "unclassified Clostridiales 1",
  "unclassified Clostridiales 2", "unclassified Clostridiales 3",
  "unclassified Clostridiales 4", "unclassified Clostridiales 5",
  "unclassified Clostridiales 6",
  "unclassified Clostridiales Family XIII. Incertae Sedis",
  "unclassified Coriobacteriia", "unclassified Desulfovibrionaceae",
  "unclassified Eggerthellaceae", "unclassified Elusimicrobia",
  "unclassified Erysipelotrichaceae", "unclassified Flavobacteriales",
  "unclassified Lachnospiraceae", "unclassified Lentisphaerae",
  "unclassified Lentisphaeria", "unclassified Methanomassiliicoccaceae",
  "unclassified Muribaculaceae", "unclassified Negativicutes 2",
  "unclassified Odoribacteraceae", "unclassified Opitutae",
  "unclassified Oscillospiraceae", "unclassified Peptoniphilaceae",
  "unclassified Peptostreptococcaceae", "unclassified Prevotellaceae",
  "unclassified Proteobacteria", "unclassified Rhodospirillaceae",
  "unclassified Rhodospirillales", "unclassified Ruminococcaceae",
  "unclassified Selenomonadaceae", "unclassified Succinivibrionaceae",
  "unclassified Sutterellaceae", "unclassified Tissierellia",
  "unclassified Tissierellia 2", "unclassified Veillonellaceae",
  "unclassified Victivallales", "unclassified_Acholeplasmatales",
  "unclassified_Anaeroplasmataceae", "unclassified_Erysipelotrichaceae",
  "unclassified_ML615J-28_Tenericutes", "unclassified_RF39_Mollicutes",
  "Veillonella", "Victivallis", "Weissella"
)

# Subset based on authentication thresholding.
set_1 <- df |> filter(
  A > 0.1, mean_rlen > 35, nreads > 50,
  grepl("\\bgenus\\b", rank)
)

# Add control sample back to the set
# (it got filtered out by authentication thresholding).
control_set <- df |> filter(
  nreads > 50,
  sample == "Control"
)
set_1 <- rbind(set_1, control_set)

# Filter out species in taxa_vector.
set_1 <- set_1 |>
  filter(
    name %in% taxa_vector
  )

# Summarise.
set_1 <- set_1 |> 
  group_by(sample, name) |> 
  summarise(nreads = sum(nreads), .groups = "drop_last") |> 
  mutate(props = nreads / sum(nreads)) |> 
  ungroup()

# Pivot to wide matrix for clustering.
mat <- set_1 |>
  select(sample, name, props) |>
  pivot_wider(names_from = name, values_from = props, values_fill = 0) |>
  column_to_rownames("sample") |>
  as.matrix()

# Cluster samples and taxa. Similar samples in terms of taxonomic composition
# stay close to each other.
sample_order <- hclust(dist(mat))$order
taxon_order <- hclust(dist(t(mat)))$order # t(mat) transposes the columns

# Pull ordered levels
taxon_levels <- colnames(mat)[taxon_order]

# Put Control at the end in sample levels.
sample_levels <- c(
  "UU0148", "UU0149", "UU0150", "UU0151",
  "UU0152", "UU0153", "UU0154", "UU0291",
  "UU0393", "UU0396", "UU0398", "Control"
)

# Apply ordering to long-format data
set_1_ordered <- set_1 |>
  mutate(
    sample = factor(sample, levels = sample_levels),
    name   = factor(name, levels = sort(taxon_levels, decreasing = TRUE))
  )

# Plot
p <- ggplot(set_1_ordered, aes(x = sample, y = name, fill = props)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = viridis::viridis(10), #10% increments
    labels  = scales::percent,
    name    = "Relative\nabundance"
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y  = element_text(size = 8),
    legend.text  = element_text(size = 8),
    panel.grid   = element_blank()
  ) +
  labs(title = "Ancient GI microbiota abundances across samples + control")
ggsave("arch_master_thesis/outputs/GI_bacteria_props.png",
  plot = p, width = 190,
  units = "mm", dpi = 300
)

# Also plot the reads.
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
taxon_levels <- colnames(mat)[taxon_order]

# Put Control at the end in sample levels.
sample_levels <- c(
  "UU0148", "UU0149", "UU0150", "UU0151",
  "UU0152", "UU0153", "UU0154", "UU0291",
  "UU0393", "UU0396", "UU0398", "Control"
)

# Apply ordering to long-format data
set_1_ordered <- set_1 |>
  mutate(
    sample = factor(sample, levels = sample_levels),
    name   = factor(name, levels = sort(taxon_levels, decreasing = TRUE))
  )

# Plot
p <- ggplot(set_1_ordered, aes(x = sample, y = name, fill = nreads)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = viridis::viridis(10), # 10% increments
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
  labs(title = "Ancient GI microbiota reads per sample + control")
ggsave("arch_master_thesis/outputs/GI_bacteria_nreads.png",
  plot = p, width = 190,
  units = "mm", dpi = 300
)

length(unique(set_1_ordered$name))
