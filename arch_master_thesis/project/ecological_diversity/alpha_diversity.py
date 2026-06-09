import skbio
import pandas as pd

files = [
    "arch_master_thesis/data/OTUs/all_data.csv",
    "arch_master_thesis/data/OTUs/anc_data.csv",
]

names = ["all", "anc"]

for i, file in enumerate(files):
    # Read the OTU made for SourceTracker.
    data = pd.read_csv(
        file, delimiter="\t", index_col=0
    )  # Make the 'OTU' column the index.

    # Calculate Shannon's index, Simpson' index and species richness.
    shannon = skbio.diversity.alpha_diversity(
        metric="shannon", counts=data.transpose(), ids=data.columns
    )

    simpson = skbio.diversity.alpha_diversity(
        metric="simpson", counts=data.transpose(), ids=data.columns
    )

    richness = (data != 0).astype(int).sum(axis=0)

    # Aggregate all metrics in one dataframe.
    alpha_diversity = (
        shannon.to_frame(name="shannon")
        .merge(simpson.to_frame(name="simpson"), left_index=True, right_index=True)
        .merge(richness.to_frame(name="richness"), left_index=True, right_index=True)
    )

    # Check alpha diversity dataframe.
    # print(alpha_diversity)

    # Append metadata (also computed for sourcetracker).
    meta = pd.read_csv(
        f"arch_master_thesis/data/OTUs/{names[i]}_meta.csv", delimiter="\t"
    )

    alpha_diversity = alpha_diversity.merge(
        meta[["#SampleID", "Site"]], left_index=True, right_on="#SampleID", how="outer"
    ).set_index("#SampleID")

    # print(alpha_diversity)

    # Melt in 'long format' to work with plotting later.
    alpha_diversity = alpha_diversity.melt(
        id_vars="Site",
        value_name="alpha diversity",
        var_name="diversity_index",
        ignore_index=False,
    )

    # print(alpha_diversity)

    # Save file to upload and plot in R.
    alpha_diversity.to_csv(
        f"arch_master_thesis/outputs/{names[i]}_alpha_diversity.csv", sep="\t"
    )
