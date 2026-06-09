library(dplyr)

files <- list.files("arch_master_thesis/outputs/", pattern = "_alpha_diversity.csv$",
  full.names = TRUE
)

metrics <- c("shannon", "simpson", "richness")

for (file in files) {
  file <- read.csv(file, sep = "\t")

  for (metric in metrics) {
    data <- file |>
      filter(diversity_index == metric)

    print(metric)
    print(kruskal.test(alpha.diversity ~ Site, data = data))
  }
}
print(file)
