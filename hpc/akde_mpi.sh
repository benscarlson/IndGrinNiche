#!/bin/bash

#SBATCH -n 120 -t 2:00:00
#SBATCH -p day
#SBATCH --mail-type=all
#SBATCH --mail-user=ben.s.carlson@gmail.com
#SBATCH -J akde

#To test, use -n 10 -t 10:00 -p scavenge
#To run, use -n 120 -t 4:00:00 -p day
# --mem-per-cpu=16G

module load miniconda
source activate parallelR3

scriptsP=~/projects/rsf/src/scripts

mpirun Rscript $scriptsP/akde.r -p mpi
