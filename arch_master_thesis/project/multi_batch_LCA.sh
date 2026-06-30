#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=15:00:00
#SBATCH --mem=150GB

echo -e '1. Cleaning potentially unwanted modules...\n'
module purge

echo -e '2. Loading Anaconda3 for conda...\n'
module load Anaconda3/2024.02-1

echo -e 'conda loaded.\n'

echo -e '3. activating metaDMG...\n'
conda activate metaDMG

echo -e 'metaDMG env is active.\n'

# Make file directories on $TMPDIR to save on I/O efficiency.
echo -e '4. Copying files to $TMPDIR...\n'
mkdir -p "$TMPDIR/bams"
mkdir -p "$TMPDIR/out"

# This reccursively coppies all .bam files it can find in my /scratch to $TMPDIR/bams.
# Excludes hidden folders and metaDMG-stuff/. 
find /scratch/s5986052 -type f -name "*.bam" \
  -not -path "*/.*" \
  -not -path "*/metaDMG-stuff/*" \
  -exec cp {} "$TMPDIR/bams/" \;

# I'm making sure to be in the /scripts directory because I want to store there the logs.
cd ~/authentication/metaDMG-stuff/scripts
echo -e 'Inside scripts directory\n'

# LCA part.
echo -e '5. Computing LCA...\n'

# Loop through every bam im my bams folder.
for bam in $TMPDIR/bams/*.bam
do
  base=$(basename "$bam") # strips the file name of the path. It only leaves the file name.
  base="${base%%_*}" # strips everything in the file name from the _ point onwards.

  metaDMG-cpp lca \
    --names /scratch/s5986052/metaDMG-stuff/configs/names.dmp \
    --nodes /scratch/s5986052/metaDMG-stuff/configs/nodes.dmp \
    --acc2tax /scratch/s5986052/metaDMG-stuff/configs/ncbi_ang_all_acession2taxid.gz \
    --sim_score_low 0.95 \
    --sim_score_high 1.0 \
    --how_many 30 \
    --weight_type 1 \
    --threads 12 \
    --bam "$bam" \
    --out_prefix "$TMPDIR/out/${base}.merged.bam"
done

echo -e 'LCA computed!\n'

echo -e '6. Calculating deamination pattern...\n'

# Deamination patterns part.
for bdamage in $TMPDIR/out/*.bdamage.gz
do
    base=$(basename "$bdamage")
    base="${base%%.*}" # make sure it's a dot here not _ because LCA saves the file with . before!!!

    metaDMG-cpp dfit \
    "$bdamage" \
    --names /scratch/s5986052/metaDMG-stuff/configs/names.dmp \
    --nodes /scratch/s5986052/metaDMG-stuff/configs/nodes.dmp \
    --showfits 2 \
    --lib ds \
    --out "$TMPDIR/out/${base}.merged.bam"
done

echo -e 'dfit computed!\n'

echo -e '7. Aggregating statistics...\n'

# Aggregating the results part. metaDMG-commands are self-explanatory I hope.
for bdamage in $TMPDIR/out/*.bdamage.gz
do
    base=$(basename "$bdamage" .merged.bam.bdamage.gz) # strips this specified sufix from the file name.

    metaDMG-cpp aggregate \
    "$bdamage" \
    --lcastat "$TMPDIR/out/${base}.merged.bam.stat.gz" \
    --names /scratch/s5986052/metaDMG-stuff/configs/names.dmp \
    --nodes /scratch/s5986052/metaDMG-stuff/configs/nodes.dmp \
    --dfit "$TMPDIR/out/${base}.merged.bam.dfit.gz" \
    --out "$TMPDIR/out/${base}_aggregated_results"
done

echo -e "Resulting stats aggregated!\n"

echo -e "8. Saving all outputs to local directory...\n"

# Unarchive the aggregated results in the lca directory.
gunzip $TMPDIR/out/*_aggregated_results.stat.gz

# Concatenate results into a single TSV.
# Grab one file to take its header.
# It could also just be header_file="UU0148_agregated_results.stat",
# but the ls method is more generic.
header_file=$(ls $TMPDIR/out/*_aggregated_results.stat | head -n 1)

# Store the header in a variable.
header=$(head -n 1 "$header_file")

# Determine output file path.
output_file="$TMPDIR/out/concatenated_metaDMGfinal.tsv"

# Write header to the output file and overwrite output_file.
# Basically saying "filename    $header" overwrite the first line of $output_file.
echo -e "filename\t$header" > "$output_file"

echo -e "TSV created!\n"

# Lool through all .stat files and reads all lines from line 2 onwards
# into $line. So it skips headers of all files and only appends the data.
# Appends the output to output_file (>> as opposed to >, so not overwrite).
for file in $TMPDIR/out/*_aggregated_results.stat
do
    tail -n +2 "$file" | while read -r line; do
        echo -e "$file\t$line" >> "$output_file"
    done
done

echo -e "TSV updated!\n"

# Copy results back to scratch
echo -e "9. Copying the results to scratch...\n"

mkdir -p /scratch/s5986052/metaDMG-stuff/lca_outputs
cp -r $TMPDIR/out/* /scratch/s5986052/metaDMG-stuff/lca_outputs/

echo -e "Files saved to scratch!\n"

conda deactivate
echo -e 'END.'