#!/bin/bash

#SBATCH --mail-user=ben.s.carlson@gmail.com
#SBATCH --mem-per-cpu=5G
#SBATCH --error=%x/bsnm_hvs.log
#SBATCH --output=%x/bsnm_hvs.log

module load miniconda
source activate parallelR3

#TODO: I should handle other optional parameters in the same way as bmode and axes
bmode=ci

params=()
[[ ! -z "$bmode" ]] && params+=("-b $bmode")
[[ ! -z "$axes" ]] && params+=("-a $axes")

opts="${params[@]}"

echo mpirun -n $n Rscript --vanilla $src/workflow/bsnm_hvs.r $dat $out $reps -r $sesid $opts -p mpi -m $logs -t

mpirun -n $n Rscript --vanilla $src/workflow/bsnm_hvs.r $dat $out $reps -r $sesid $opts -p mpi -m $logs -t
#-b $bmode -a $axes 
