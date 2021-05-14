
#This wf script runs niche metrics using only distance variables

#---------------------#
#--- Set up grace ---#
#---------------------#
wd=~/projects/ms1/analysis/rev2/dist_vars

#Set up analysis folder on grace
ssh grace "mkdir -p ~/projects/ms1/analysis/rev2/dist_vars"
# Move control files to grace
scp -r $wd/ctfs grace:~/projects/ms1/analysis/rev2/dist_vars

ssh grace 

#--------------------#
#--- Run on grace ---#
#--------------------#

wd=~/projects/ms1/analysis/rev2/dist_vars

#need these for all scripts
export src=~/projects/ms1/src
export reps=1
export sesid=niches_dist
export out=$wd/$sesid
export axes=dist2urban,dist2forest

#need these parameters only for bsnm_hvs.r
export dat=~/projects/ms1/data/derived/obs_anno_2k.csv
export bmode=none

#slurm variables
export n=120
export t=2:00:00
export p=day
export J=$sesid
export mail=NONE

#Make sure to make the output directory or mpi will fail because mpi log files go here
cd $wd
mkdir -p $out

# These have to start with the --option b/c echo won't print - as first character
pars=`echo --ntasks $n -p $p -t $t -J $J --mail-type $mail`
exp=`echo --export=ALL,n=$n,p=$p,t=$t,J=$J,mail=$mail`

export logs=$out/logs/mpi_hvs
sbatch $pars $exp $src/hpc/bsnm_hvs.sh

export n=15
export logs=$out/logs/mpi_nicheset
sbatch $pars $exp $src/hpc/bsnm_nicheset.sh

export n=750
export t=4:00:00
export logs=$out/logs/mpi_pw
sbatch $pars $exp $src/hpc/bsnm_pairwise.sh

#-----------------------------#
#--- No distance variables ---#
#-----------------------------#

#This is mostly the same settings as above

wd=~/projects/ms1/analysis/rev2/dist_vars

#need these for all scripts
export src=~/projects/ms1/src
export reps=1
export sesid=niches_nodist
export out=$wd/$sesid
export axes=pct_tree,pct_bare,ndvi

#need these parameters only for bsnm_hvs.r
export dat=~/projects/ms1/data/derived/obs_anno_2k.csv
export bmode=none

#slurm variables
export n=120
export t=2:00:00
export p=day
export J=$sesid
export mail=NONE

#Make sure to make the output directory or mpi will fail because mpi log files go here
cd $wd
mkdir -p $out

# These have to start with the --option b/c echo won't print - as first character
pars=`echo --ntasks $n -p $p -t $t -J $J --mail-type $mail`
exp=`echo --export=ALL,n=$n,p=$p,t=$t,J=$J,mail=$mail`

export logs=$out/logs/mpi_hvs
sbatch $pars $exp $src/hpc/bsnm_hvs.sh

export n=15
export logs=$out/logs/mpi_nicheset
sbatch $pars $exp $src/hpc/bsnm_nicheset.sh

export n=750
export t=2:00:00
export logs=$out/logs/mpi_pw
sbatch $pars $exp $src/hpc/bsnm_pairwise.sh

#---- Pull down results ----#

wd=~/projects/ms1/analysis/rev2/dist_vars
src=~/projects/ms1/src

cd $wd

sesid=niches_dist
mkdir $sesid
scp grace:~/projects/ms1/analysis/rev2/dist_vars/$sesid/*.csv $sesid

sesid=niches_nodist
mkdir $sesid
scp grace:~/projects/ms1/analysis/rev2/dist_vars/$sesid/*.csv $sesid

#--- Load results into the database ---#

cd $wd

sesid=niches_dist

$src/db/load_hpc_csv.r $sesid/niche_stats.csv $sesid niche_stats -t #-b
$src/db/load_hpc_csv.r $sesid/niche_set_stats.csv $sesid niche_set_stats -t
$src/db/load_hpc_csv.r $sesid/pairwise.csv $sesid pairwise -t

sesid=niches_nodist

$src/db/load_hpc_csv.r $sesid/niche_stats.csv $sesid niche_stats -t #-b
$src/db/load_hpc_csv.r $sesid/niche_set_stats.csv $sesid niche_set_stats -t
$src/db/load_hpc_csv.r $sesid/pairwise.csv $sesid pairwise -t

#--- Calculate niche metrics ---#

sesid=niches_dist,niches_nodist

$src/workflow/rini.r $sesid #-t -b
$src/workflow/nestedness.r $sesid #-t -b
$src/workflow/cluster_weighted.r $sesid #-t -b

