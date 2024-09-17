#!/bin/bash
#
#SBATCH --job-name=unbalanced
#SBATCH --output unbalanced.out
#SBATCH --error unbalanced.err
#SBATCH --mem=300G
#SBATCH -c 8
#SBATCH -p scavenger
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=aeo21@duke.edu

source /datacommons/ydiaolab/arinze/apps/miniconda_20220118/etc/profile.d/conda.sh
conda activate granges_ML

Rscript /datacommons/ydiaolab/arinze/Tongyu_ML_DC1_DC2/scripts/train_test_unbalanced.R