#!/bin/bash
 
#SBATCH --job-name=data_analysis_Rote_Liste_Fische
#SBATCH --chdir=/work/friedrim
#SBATCH --output=/work/%u/%x-%A-%a.log
#SBATCH --time=1-0:0:0
#SBATCH --mem-per-cpu=2G
#SBATCH --cpus-per-task=4
#SBATCH --mail-user=martin.friedrichs-manthey@idiv.de
#SBATCH --mail-type=FAIL
 
module load foss/2020b R/4.0.4-2

output_dir=/work/$USER/$SLURM_ARRAY_JOB_ID
mkdir -p "$output_dir"

factor_output_file="$output_dir/factor-output-$SLURM_ARRAY_TASK_ID.rds"
index_output_file="$output_dir/index-output-$SLURM_ARRAY_TASK_ID.rds"

Rscript $HOME/data_analysis.R "$factor_output_file" "$index_output_file"