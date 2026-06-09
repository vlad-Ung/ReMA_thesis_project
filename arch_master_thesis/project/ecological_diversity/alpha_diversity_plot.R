library(ggplot2)

# Function to call plotting reccursively.
# index for diveristy index.
# data for CSV file.
# alt_name for title ('all data' or 'ancient dat').
plot_alpha <- function(index, data, alt_name) {
  df <- subset(data, diversity_index == index)

  ggplot(df, aes(x = Site, y = alpha.diversity, fill = Site)) +
    geom_boxplot() +
    geom_jitter() +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1),
      legend.position = "none"
    ) +
    # To keep Env colors consistent across all "diversity" graphs.
    scale_fill_manual(values = c(
      "sampled_control" = "#000000",
      "Harelbeke" = "#E31A1C",
      "HeHem" = "#57DB5E",
      "HEEN24" = "#5F57DB"
    )) +
    # passess diversity index and type of data through variables.
    labs(title = paste0(index, ", ", alt_name, " data"), x = "", y = "Alpha Diversity")
}

# Determine file paths.
files <- c("arch_master_thesis/outputs/all_alpha_diversity.csv", "arch_master_thesis/outputs/anc_alpha_diversity.csv")

# These names are used to distinguish between file paths.
names <- c("all", "anc")

# These names are used for the plot titles.
alt_names <- c("all", "ancient")

# Determine which diversity indices to loop through.
indices <- c("shannon", "simpson", "richness")

# Go through all the files.
for (j in seq_along(files)) {
  data <- read.csv(files[j], sep = "\t") # read the csv.

  # Go through all diversity indices.
  for (i in seq_along(indices)) {
    # Make the plot with the current diversity index of the read file.
    p <- plot_alpha(indices[i], data, alt_names[j])

    # Alt_names[j] gets passed to the title.
    ggsave(paste0("arch_master_thesis/outputs/", names[j], "_alpha_diversity_", indices[i], ".png"),
      plot = p, width = 8, height = 6, units = "in", dpi = 300
    )
  }
}
