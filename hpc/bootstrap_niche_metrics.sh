#!/bin/bash

#SBATCH -n 102 -t 10:00:00
#SBATCH -p day
#SBATCH --mail-type=all
#SBATCH --mail-user=ben.s.carlson@gmail.com
#SBATCH --mem-per-cpu=3G
#SBATCH -J beu15

#To test, use -n 10 -t 10:00 -p scavenge
#To run, use -n 120 -t 4:00:00 -p day

module load miniconda
source activate parallelR3

wd=~/projects/ms1/analysis/bootstrap
src=~/projects/ms1/src

cd $wd

module load miniconda
conda activate parallelR3

npts=2000
reps=101
runid=beu15
out=metrics/$runid/out.csv
axes=pct_tree,dist2urban,pct_bare,dist2forest,ndvi
dat=data/obs_anno.csv

mpirun Rscript --vanilla $src/workflow/bootstrap_niche_metrics.r $dat $out $npts $reps -a $axes -r $runid -s 594 -p mpi -t
