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
set_1 <- data |> filter(
  A > 0.1, mean_rlen > 35, nreads > 50,
  grepl("\\bgenus\\b", rank)
)

# Add control sample back to the set
# (it got filtered out by authentication thresholding).
control_set <- data |> filter(
  mean_rlen > 35, nreads > 50,
  sample == "Control"
)
set_1 <- rbind(set_1, control_set)

# Filter out species in taxa_vector.
set_1 <- set_1 |>
  filter(
    name %in% taxa_vector
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

readr::write_tsv(anc_data, "arch_master_thesis/data/OTUs/OTU_microbiota.csv")
