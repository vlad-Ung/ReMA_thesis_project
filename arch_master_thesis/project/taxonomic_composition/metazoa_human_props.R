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
set_1 <- df |> filter(
  A > 0.1, mean_rlen > 35, nreads > 50,
  grepl("\\genus\\b", rank),
  grepl("Metazoa", taxa_path)
)

# Add control sample back to the set
# (it got filtered out by authentication thresholding).
control_set <- df |> filter(
  nreads > 50,
  sample == "Control",
  grepl("Metazoa", taxa_path)
)
#set_1 <- rbind(set_1, control_set)

# Summarise.
set_1 <- set_1 |> 
  group_by(sample, name) |> 
  summarise(nreads = sum(nreads), .groups = "drop_last") |> 
  mutate(props = nreads / sum(nreads)) |> 
  ungroup()

set_1$name <- as.factor(set_1$name) # needed for graph reordering

# Named vector to control fill color.
fill_colors <- setNames(
  ifelse(levels(set_1$name) == "Homo", "red", "gray"),
  levels(set_1$name)
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

# Plots.
p <- set_1 |>
  mutate(name = fct_reorder(name, -props)) |>
  ggplot(aes(x = sample, y = props)) +
  geom_col(position = "dodge", aes(fill = name)) +
  scale_fill_manual(values = c25[seq_len(length(unique(set_1$name)))]) +
  xlab(NULL) +
  ylab("%") +
  labs(title = "Ancient metazoan proportions per sample",
    fill = "Genus"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )
ggsave("arch_master_thesis/outputs/metazoan_props.png", plot = p,
  width = 8, height = 6, units = "in", dpi = 300
)

# Plots.
p2 <- set_1 |>
  mutate(name = fct_reorder(name, -props)) |>
  ggplot(aes(x = sample, y = nreads)) +
  geom_col(position = "dodge", aes(fill = name)) +
  scale_fill_manual(values = c25[seq_len(length(unique(set_1$name)))]) +
  xlab(NULL) +
  labs(title = "Ancient metazoan nreads per sample",
    fill = "Genus"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )
ggsave("arch_master_thesis/outputs/metazoan_nreads.png",
  plot = p2,
  width = 8, height = 6, units = "in", dpi = 300
)

q <- set_1 |>
  mutate(name = fct_reorder(name, -props)) |>
  ggplot(aes(x = sample, y = props)) +
  geom_col(position = "dodge", aes(fill = name)) +
  scale_fill_manual(values = fill_colors) +
  xlab(NULL) +
  ylab("%") +
  labs(title = "Human aDNA proportions per sample (% of Metazoans)",
    fill = "Genus"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )
ggsave("arch_master_thesis/outputs/human_highlight_props.png", plot = q,
  width = 8, height = 6, units = "in", dpi = 300
)

q2 <- set_1 |>
  mutate(name = fct_reorder(name, -props)) |>
  ggplot(aes(x = sample, y = nreads)) +
  geom_col(position = "dodge", aes(fill = name)) +
  scale_fill_manual(values = fill_colors) +
  xlab(NULL) +
  labs(title = "Human aDNA nreads per sample",
    fill = "Genus"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )
ggsave("arch_master_thesis/outputs/human_highlight_nreads.png",
  plot = q2,
  width = 8, height = 6, units = "in", dpi = 300
)
