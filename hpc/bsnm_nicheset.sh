#!/bin/bash

#SBATCH --mail-user=ben.s.carlson@gmail.com
#SBATCH --mem-per-cpu=5G
#SBATCH --error=%x/bsnm_nicheset.log
#SBATCH --output=%x/bsnm_nicheset.log

module load miniconda
source activate parallelR3

mpirun -n $n Rscript --vanilla $src/workflow/bsnm_nicheset.r $out $reps -r $sesid -p mpi  -m $logs -t
