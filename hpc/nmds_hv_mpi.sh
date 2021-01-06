#!/bin/bash

#SBATCH -n 13 -t 23:59:00
#SBATCH -p day
#SBATCH --mail-type=all
#SBATCH --mail-user=ben.s.carlson@gmail.com
#SBATCH --mem-per-cpu=16G
#SBATCH -J 1000pts

#To test, use -n 10 -t 10:00 -p scavenge
#To run, use -n 120 -t 4:00:00 -p day

module load miniconda
source activate parallelR3

scriptsP=~/projects/rsf/src/scripts

mpirun Rscript $scriptsP/nmds_hv.r hvs/5axes2000pts1 1000 -p mpi
