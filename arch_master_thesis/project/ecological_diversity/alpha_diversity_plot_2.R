library(ggplot2)
library(dplyr)

data <- read.csv("arch_master_thesis/outputs/anc_alpha_diversity.csv",
  sep = "\t"
)

p <- ggplot(data, aes(x = Site, y = alpha.diversity, fill = Site)) +
  facet_wrap(~diversity_index, scales = "free") +
  geom_boxplot() +
  geom_jitter() +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1),
    legend.position = "none"
  ) +
  # To keep Env colors consistent across all "diversity" graphs.
  scale_fill_manual(values = c(
    "Control" = "#4d4d4d",
    "Harelbeke" = "#E31A1C",
    "HeHem" = "#57DB5E",
    "HEEN24" = "#5F57DB"
  )) +
  labs(
    title = "Alpha diversity values per site",
    x = "", y = "alpha diversity"
  )

ggsave("arch_master_thesis/outputs/alpha_diversity.png", plot = p,
  dpi = 300,
  width = 7,
  height = 4,
  units = "in")

summ <- data |> 
  group_by(Site, diversity_index) |> 
  summarise(median = median (alpha.diversity),
    min = min(alpha.diversity),
    max = max(alpha.diversity)
  )
