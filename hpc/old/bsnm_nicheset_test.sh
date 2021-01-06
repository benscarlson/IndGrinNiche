#!/bin/bash

#SBATCH -n 10 -t 10:00 -p scavenge
#SBATCH --mail-type=all
#SBATCH --mail-user=ben.s.carlson@gmail.com
#SBATCH --mem-per-cpu=5G
#SBATCH -J test4
#SBATCH --error=%x/bsnm_nicheset.log
#SBATCH --output=%x/bsnm_nicheset.log

#--job-name try this instead of J
# %x refers to jobname. only available in version 17.02.1 sbatch --version

echo 'Running test configuration for bsnm_nicheset_test.r'

module load miniconda
source activate parallelR3

src=~/projects/ms1/src

module load miniconda
conda activate parallelR3

reps=2
sesid=test4
out=$sesid

mpirun Rscript --vanilla $src/workflow/bsnm_nicheset.r $out $reps -r $sesid -p mpi -t
