#!/bin/bash

#SBATCH -n 20 -t 2:00:00
#SBATCH -p day
#SBATCH --mail-type=all
#SBATCH --mail-user=ben.s.carlson@gmail.com

#To test, use -n 10 -t 10:00 -p scavenge

module load Tools/miniconda
source activate parallelR3

scriptsP=~/projects/rsf/src/scripts

mpirun Rscript $scriptsP/hv/hvs_niche_set.r hvs/5axes2000pts1 -p mpi
