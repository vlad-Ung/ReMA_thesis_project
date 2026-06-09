# Load packages.
library(tidyr)
library(dplyr)
library(forcats)
library(scales)
library(gridExtra)
library(ggplot2)
library(purrr)
library(ggpubr)

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

# Add chronology column.
df$Date_BP[df$sample %in% c("UU0148", "UU0149", "UU0150", "UU0151", "UU0152",
                            "UU0153", "UU0154")] <- 1792
df$Date_BP[df$sample == "UU0291"] <- 1900
df$Date_BP[df$sample %in% c("UU0393", "UU0396", "UU0398")] <- 1800
df$Date_BP[df$sample == "UU0408"] <- "Control"

# And make Date_BP a factor column (for levels).
df$Date_BP <- as.factor(df$Date_BP)

# Subset data.
set_1 <- df |> filter(
  nreads > 50,
  grepl("\\genus\\b", rank)
)


# Add taxonomic grouping columns.
set_1 <- set_1 |> mutate(
  tax_group = case_when(
    grepl("Viridiplant", taxa_path) ~ "Viridiplantae",
    grepl("Metazoa", taxa_path) ~ "Metazoa",
    grepl("Bacteria", taxa_path) ~ "Bacteria",
    grepl("Archaea", taxa_path) ~ "Archaea",
    grepl("Fungi", taxa_path) ~ "Fungi"
  )
)

# Plotting damage (A) by period (dates BP)
p <- set_1 |>
  mutate(Date_BP = fct_relevel(
    Date_BP,
    "Control", "1900", "1800", "1792"
  )) |>
  ggplot(aes(x = A, y = Date_BP)) +
  geom_boxplot(aes(x = A, y = Date_BP, fill = sample)) +
  scale_x_continuous(limits = c(0, 0.20), breaks = seq(0, 0.20, by = 0.05)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Damage amplitude over time")

# Plotting mean length (mean_rlen) by period (dates BP)
q <- set_1 |>
  mutate(Date_BP = fct_relevel(
    Date_BP,
    "Control", "1900", "1800", "1792"
  )) |>
  ggplot(aes(x = mean_rlen, y = Date_BP)) +
  geom_boxplot(aes(x = mean_rlen, y = Date_BP, fill = sample)) +
  scale_x_continuous(limits = c(30, 80), breaks = seq(30, 80, by = 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Mean fragment length over time")

# Combining the plots and saving as pdf file
png(file = "arch_master_thesis/outputs/damage_length_time.png", unit = "in", res = 300,
    width = 8, height = 6)
g <- grid.arrange(p, q,
  ncol = 2, nrow = 1
)
dev.off() # need this otherwise the pdf remains open and can't display/save.

summ <- set_1 |> 
  group_by(sample, Date_BP) |> 
  summarise(
    mean_damage = mean(A),
    median_damage = median(A),
    mean_rlen = mean(mean_rlen),
    median_rlen = median(mean_rlen),
    .groups = "drop"
  ) |> 
  group_by(Date_BP) |> 
  filter(n() > 1) |> 
  ungroup()

qqnorm(summ$mean_damage)
qqline(summ$mean_damage)
hist(summ$mean_damage)
shapiro.test(summ$mean_damage)
ks.test(summ$mean_damage, 'pnorm')

qqnorm(summ$mean_rlen)
qqline(summ$mean_rlen)
hist(summ$mean_rlen)
shapiro.test(summ$mean_rlen)
ks.test(summ$mean_rlen, "pnorm")

print(kruskal.test(mean_damage ~ Date_BP, data = summ))
print(kruskal.test(mean_rlen ~ Date_BP, data = summ))

