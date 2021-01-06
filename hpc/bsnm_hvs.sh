#!/bin/bash

#SBATCH --mail-user=ben.s.carlson@gmail.com
#SBATCH --mem-per-cpu=5G
#SBATCH --error=%x/bsnm_hvs.log
#SBATCH --output=%x/bsnm_hvs.log

module load miniconda
source activate parallelR3

mpirun -n $n Rscript --vanilla $src/workflow/bsnm_hvs.r $dat $out $reps -b $bmode -r $sesid -p mpi -m $logs -t

