# save as test_pcoa.py
import pandas as pd
import skbio
from plotnine import *
import seaborn as sns
import scipy.spatial as sp, scipy.cluster.hierarchy as hc
import plotly.express as px

# Determin file paths.
files = [
    "arch_master_thesis/data/OTUs/all_data.csv",
    "arch_master_thesis/data/OTUs/anc_data.csv",
]

names = ["all", "anc"]

for i, file in enumerate(files):
    data = pd.read_csv(file, sep="\t", index_col=0)

    meta = pd.read_csv(
        f"arch_master_thesis/data/OTUs/{names[i]}_meta.csv", delimiter="\t"
    )

    # All 0 samples introudce NaNs in the distance matrix and lead to errors.
    data = data.loc[:, data.sum(axis=0) > 0]

    beta_diversity = skbio.diversity.beta_diversity(
        metric="braycurtis", counts=data.transpose(), ids=data.columns, validate=True
    )

    # print(beta_diversity)

    pcoa = skbio.stats.ordination.pcoa(beta_diversity)

    # print(pcoa.samples.head())

    # Extract variance explained per component to make screeplot.
    var_explained = (
        pcoa
        # not 10 because it will be plotted next to PC1 (alphabetical sorting)
        .proportion_explained[:9]  # first 9 components
        .to_frame(name="variance explained")  # rename variance column
        .reset_index()  # make name of PCs an actual column (from row indices)
        .rename(columns={"index": "PC"})
    )

    # print(var_explained.head())

    # Plot the screeplot.
    g = ggplot(var_explained, aes(x="PC", y="variance explained", group=1))
    g += geom_point()
    g += geom_line()
    g += theme_classic()
    g.save(f"arch_master_thesis/outputs/pcoa_screeplot_{names[i]}.png", dpi=300)

    # Extract samples and their contributions in the firs three axes.
    # Rename their index to 'sample'.
    # And promite the index to an actual column.
    pcoa_embed = pcoa.samples[["PC1", "PC2", "PC3"]].rename_axis("sample").reset_index()

    # Assign Env labels to the samples extracted above.
    pcoa_embed = (
        pcoa_embed.merge(
            meta[["#SampleID", "Site"]],  # take only #SampleID and Env columns
            left_on="sample",  # match sample from pcoa_embed
            right_on="#SampleID",  # with #SampleID from meta
            how="left",  # keep only samples from pcoa and don't duplicate Env
        ).drop(
            "#SampleID", axis=1
        )  # removes redundant #SampleID column
        # promote sample index to actual column
        # important for ordered samples in the heatmap.
        .reset_index(drop=True)
    )

    # Plot bibplots.
    g = ggplot(pcoa_embed, aes(x="PC1", y="PC2", color="Site"))
    g += geom_point(size=3)
    g += geom_label(
        aes(label="sample"),  # use aes() instead of passing the Series directly
        nudge_x=0.01,  # use small fractions of your actual axis range
        nudge_y=0.03,
        size=6,
        show_legend=False,  # ← fixes the "a" in the legend
    )
    g += theme_minimal()
    g += scale_color_manual(
        values={
            "Control": "#4d4d4d",
            "Harelbeke": "#E31A1C",
            "HeHem": "#57DB5E",
            "HEEN24": "#5F57DB",
        }
    )
    g += labs(title="PCoA biplot of the Bray-Curtis beta diversity")
    g.save(f"arch_master_thesis/outputs/pcoa_biplot_{names[i]}_1_2.png", dpi=300)

    g = ggplot(pcoa_embed, aes(x="PC1", y="PC3", color="Site"))
    g += geom_point()
    g += theme_minimal()
    g += scale_color_manual(
        {
            "Control": "#4d4d4d",
            "Harelbeke": "#E31A1C",
            "HeHem": "#57DB5E",
            "HEEN24": "#5F57DB",
        }
    )
    g.save(f"arch_master_thesis/outputs/pcoa_biplot_{names[i]}_1_3.png", dpi=300)

    # 3D plot with plotly.
    fig = px.scatter_3d(
        pcoa_embed,
        x="PC1",
        y="PC2",
        z="PC3",
        color="Site",
        color_discrete_map={
            "Control": "#4d4d4d",
            "Harelbeke": "#E31A1C",
            "HeHem": "#57DB5E",
            "HEEN24": "#5F57DB",
        },
        # hover_name="sample",
    )
    fig.update_layout(
        width=1200,
        height=900,
        margin=dict(l=0, r=0, b=0, t=0),
    )

    fig.write_image(
        f"arch_master_thesis/outputs/pcoa_biplot_{names[i]}_3D.png",
    )

    # Manually determine the color mapping to use as row color in
    # heatmap
    pcoa_embed["colour"] = pcoa_embed["Site"].map(
        {
            "Control": "#4d4d4d",
            "Harelbeke": "#E31A1C",
            "HeHem": "#57DB5E",
            "HEEN24": "#5F57DB",
        }
    )

    # Determine hierarchical clustering between samples and store it in variable.
    linkage = hc.linkage(
        sp.distance.squareform(beta_diversity.to_data_frame()), method="average"
    )

    # # This deals with the Env colors sidebar (makes sure it's consistent).
    # row_colors = (
    #     pcoa_embed.set_index("sample")[
    #         "colour"
    #     ]  # grab color column determined manually above
    #     .reindex(
    #         beta_diversity.to_data_frame().index
    #     )  # reorder color column to match row order of the distance matrix
    #     .to_numpy()  # need to convert it to numpy array to get rid of mini legend for the color sidebar
    # )

    cm = sns.clustermap(
        beta_diversity.to_data_frame(),
        row_linkage=linkage,  # use the linkage determined aboved
        col_linkage=linkage,  # use the linkage determined above
        row_colors=pcoa_embed[
            "colour"
        ].to_list(),  # add sidebar with colors corresponding to Env from metadata
    )

    # # load the cm graphic object to ax and force it to heatmap.
    # ax = cm.ax_heatmap
    # # Force x axis labels to appear.
    # ax.set_xticks(range(len(cm.data2d.columns)))
    # # Rotate x-axis labels 45 degrees to the right
    # ax.set_xticklabels(cm.data2d.columns, rotation=45, ha="right", fontsize=6)
    # # Force y-axis labels to appear.
    # ax.set_yticks(range(len(cm.data2d.index)))
    # # To be explicit, set their rotation to 0 and their fontsize to 6.
    # ax.set_yticklabels(cm.data2d.index, rotation=0, fontsize=6)

    cm.savefig(f"arch_master_thesis/outputs/clustermap_{names[i]}.png", dpi=300)
