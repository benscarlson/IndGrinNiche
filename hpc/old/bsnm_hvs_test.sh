#!/bin/bash

#SBATCH -n 10 -t 10:00 -p scavenge
#SBATCH --mail-type=all
#SBATCH --mail-user=ben.s.carlson@gmail.com
#SBATCH --mem-per-cpu=5G
#SBATCH -J test4

echo 'Running test configuration for bsnm_hvs.r'

module load miniconda
source activate parallelR3

src=~/projects/ms1/src

module load miniconda
conda activate parallelR3

npts=100
reps=2
runid=test4
out=$runid
axes=pct_tree,pct_bare
dat=data/obs_anno.csv

mpirun Rscript --vanilla $src/workflow/bsnm_hvs.r $dat $out $npts $reps -a $axes -r $runid -s 594 -p mpi -t
