library(vegan)
library(tidyverse)
library(data.table)

# Data preparation.
data <- read.csv("arch_master_thesis/data/OTUs/OTU_microbiota.csv", sep = "\t")
meta <- read.csv("arch_master_thesis/data/OTUs/microbiota_meta.csv", sep = "\t")

data_transposed <- data.table::transpose(data[, -c(1)])

data_transposed <- data_transposed |>
  mutate(
    sample_id = colnames(data)[-c(1)],
    site = as.factor(meta$Site)
  )

colnames(data_transposed)[-c(42, 43)] <- data$`X.OTU.ID`

# Only species abundances.
perm_data <- data_transposed[, -c(42, 43)]

# Data transformation to normalise variance.
set.seed(1234)

perm_data_transformed <- sqrt(perm_data)

# NMDS model to visualise species composition.
nmds_result <- metaMDS(perm_data_transformed, distance = "bray")

# Extract NMDS scores.
nmds_scores <- as.data.frame(scores(nmds_result)$site)

# Find out the group centroids.
group_centroids <- data.frame(
  site = c("Control", "Harelbeke", "HeHem", "HEEN24"),
  centroid_X = c(
    mean(nmds_scores$NMDS1[data_transposed$site == "Control"]),
    mean(nmds_scores$NMDS1[data_transposed$site == "Harelbeke"]),
    mean(nmds_scores$NMDS1[data_transposed$site == "HeHem"]),
    mean(nmds_scores$NMDS1[data_transposed$site == "HEEN24"])
  ),
  centroid_Y = c(
    mean(nmds_scores$NMDS2[data_transposed$site == "Control"]),
    mean(nmds_scores$NMDS2[data_transposed$site == "Harelbeke"]),
    mean(nmds_scores$NMDS2[data_transposed$site == "HeHem"]),
    mean(nmds_scores$NMDS2[data_transposed$site == "HEEN24"])
  )
)

# Dataframe for plotting
plot_data <- data.frame(
  site = data_transposed$site,
  NMDS1 = nmds_scores$NMDS1,
  NMDS2 = nmds_scores$NMDS2,
  xend = c(
    rep(group_centroids[1, 2], 1), rep(group_centroids[2, 2], 6),
    rep(group_centroids[3, 2], 1), rep(group_centroids[4, 2], 3)
  ),
  yend = c(
    rep(group_centroids[1, 3], 1), rep(group_centroids[2, 3], 6),
    rep(group_centroids[3, 3], 1), rep(group_centroids[4, 3], 3)
  )
)

# Plot data.
ggplot(plot_data, aes(NMDS1, NMDS2)) +
  geom_point(aes(colour = site), size = 2) +
  stat_ellipse(
    geom = "polygon", alpha = 0.04, aes(group = site),
    colour = "black", fill = "blue"
  ) +
  geom_point(data = group_centroids, aes(x = centroid_X, y = centroid_Y),
    colour = "black", size = 2, shape = 7
  ) +
  geom_segment(data = plot_data, aes(
    x = NMDS1, y = NMDS2,
    xend = xend, yend = yend, colour = site
  ), alpha = 0.5) +
  scale_color_manual(
    name = "Site", labels = unique(plot_data$site),
    values = c(
      "Control" = "#4d4d4d",
      "Harelbeke" = "#E31A1C",
      "HeHem" = "#57DB5E",
      "HEEN24" = "#5F57DB"
    )
  )

perm_dist <- vegdist(perm_data_transformed, method = "bray")

dispersion <- betadisper(perm_dist, group = data_transposed$site, type = "centroid")
dispersion
plot(dispersion)
anova(dispersion)

