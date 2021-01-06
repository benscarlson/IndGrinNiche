#!/bin/bash

#SBATCH -n 120 -t 4:00:00
#SBATCH -p day
#SBATCH --mail-type=all
#SBATCH --mail-user=ben.s.carlson@gmail.com

#To test, use -n 10 -t 10:00 -p scavenge
#To run, use -n 120 -t 4:00:00

module load miniconda
source activate parallelR3

scriptsP=~/projects/rsf/src/scripts

mpirun Rscript $scriptsP/variograms.r data/obs_trim.csv -p mpi
