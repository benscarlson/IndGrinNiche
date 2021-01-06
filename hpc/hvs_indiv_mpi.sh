#!/bin/bash

#SBATCH -n 20 -t 60:00
#SBATCH -p day
#SBATCH --mail-type=all
#SBATCH --mail-user=ben.s.carlson@gmail.com

#To test, use -n 10 -t 10:00 -p scavenge

module load Tools/miniconda
source activate parallelR

axes=pct_tree,dist2urban,pct_bare,dist2forest,ndvi

mpirun Rscript ~/projects/rsf/src/scripts/hv/hvs_indiv.r data/obsbg_anno.csv hvs/5axes2000pts1 2000 $axes -p mpi
