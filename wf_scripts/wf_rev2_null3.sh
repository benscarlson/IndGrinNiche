#Instead of using the background estimated from empirical steps, sample from a 
# background randomly using buffers (like null model 2)

wd=~/projects/ms1/analysis/rev2/null3_full
src=~/projects/ms1/src

cd $wd

#---- Create the nest/center locations ----#
cdat=~/projects/ms1/data/derived/nest_center.csv

$src/workflow/nest_center.r $cdat -t

#---- Create available backgrounds according to buffers ----#
dat=~/projects/ms1/data/derived/obs_anno.csv
out=data/bg_buf.csv

$src/workflow/bg_buffer.r $dat $out $cdat -t

#---- Add a dist2nest column ----#
dat=data/bg_buf.csv
out=data/bg_dist.csv

$src/workflow/dist2point.r $dat $out $cdat -c dist2nest

#------------------#
#---- Annotate ----#
#------------------#

datName=bg_dist
dat=data/$datName.csv #used in call to annotation script
gcsimport=gs://mol-playground/benc/ingest_ee/$datName
gcsanno=gs://mol-playground/benc/annotated/$datName
asset=users/benscarlson/annotate/tracks/$datName

pyenv activate geosp_poc #Activate the environment

annoprep $dat #TODO: don't overwrite *_anno.csv if it is already there!
annoimport -d $dat -g $gcsimport -a $asset
#annotate in playground: annotate/annotate_by_static_or_ts.js
#ndvi: projects/map-of-life/benc/ndvi/huj_eobs
#pct_tree: users/benscarlson/glc_landcover/tree
#pct_bare: users/benscarlson/glc_landcover/bare
#dist2forest: projects/map-of-life/benc/dist2forest_huj_eobs
#dist2urban: projects/map-of-life/benc/guf/dist2urban_hujeobs
annoprocess -d $dat -g $gcsanno #TODO: in many cases NaN comes back instead of NA. Why?
annocleanup -d $dat -i $gcsimport -g $gcsanno

pyenv deactivate

#-------------------------#
#--- Sample background ---#
#-------------------------#

wd=~/projects/ms1/analysis/rev2/null3_full
src=~/projects/ms1/src

cd $wd

dat=data/bg_dist_anno.csv
out=data/ud2k.csv

$src/workflow/null3_bg_buf.r $dat $out mod4 2000 -t

#---------------------#
#--- Setup on grace ---#
#---------------------#

# quote prevents path expansion when assigning the variable.
# Required since scp needs to send ~ to remote server
wd=~/projects/ms1/analysis/rev2/null3_full
rd="~/projects/ms1/analysis/rev2/null3_full" 

#Copy data
ssh grace "mkdir -p $rd/data"
scp -r $wd/data/ud2k.csv grace:$rd/data

#Copy control files
scp -r $wd/ctfs grace:$rd

#---------------------#
#--- Run on grace ---#
#---------------------#

ssh grace

wd=~/projects/ms1/analysis/rev2/null3_full

#need these for all scripts
export src=~/projects/ms1/src
export reps=101
export sesid=null3_full
export out=$wd/$sesid

#need these parameters only for bsnm_hvs.r
export dat=$wd/data/ud2k.csv
export bmode=ci

#hvs: n=750, requested 4 hour, took 3.5 hrs.
#nset: n=750, requested 1 hr, took 20 min.
#pw: n=750, requested 8 hrs, took 6.5 hrs

#slurm variables
export n=750
export t=8:00:00
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

export logs=$out/logs/mpi_nicheset
sbatch $pars $exp $src/hpc/bsnm_nicheset.sh

export logs=$out/logs/mpi_pw
sbatch $pars $exp $src/hpc/bsnm_pairwise.sh

#---- check status
ssh grace
squeue -l -u bc447
wd=~/projects/ms1/analysis/rev2/null3_full
cd $wd
tail -50 null3_full/bsnm_hvs.log
tail -50 null3_full/bsnm_nicheset.log
tail -50 null3_full/bsnm_pairwise.log

#-----------------#
#---- Results ----#
#-----------------#

wd=~/projects/ms1/analysis/rev2/null3_full
rd="~/projects/ms1/analysis/rev2/null3_full"

src=~/projects/ms1/src

cd $wd

sesid=null3_full
mkdir -p $sesid
scp grace:$rd/$sesid/*.csv $sesid

#--- Load results into the database ---#

wd=~/projects/ms1/analysis/rev2/null3_full
cd $wd

sesid=null3_full

$src/db/load_hpc_csv.r $sesid/niche_stats.csv $sesid niche_stats -t #-b
$src/db/load_hpc_csv.r $sesid/niche_set_stats.csv $sesid niche_set_stats -t
$src/db/load_hpc_csv.r $sesid/pairwise.csv $sesid pairwise -t

#--- Calculate niche metrics ---#

sesid=null3_full

$src/workflow/rini.r $sesid -t #-b
$src/workflow/nestedness.r $sesid -t #-b
$src/workflow/cluster_weighted.r $sesid -t #-b

#--- Make figure ---#

wd=~/projects/ms1/analysis/rev2/null3_full
src=~/projects/ms1/src
panelout=$wd/figs
figout=~/projects/ms1/docs/ms/submission_natcom/submission_3/v26/figs
#figout=$wd/figs

cd $wd

sesids="full_hvs,full_bg_buf,null3_full" #null3
labels="Individual identity,Environmental availability,Population SSF" #Pop SSF w. steps
stat=5axes2000pts1

$src/figs/bsnm_hist.r $sesids spec --stat "$stat" --labels "$labels" $panelout/a_spec_null3.pdf
$src/figs/bsnm_hist.r $sesids nested --stat "$stat" --labels "$labels" $panelout/b_nested_null3.pdf
$src/figs/bsnm_hist.r $sesids clust --stat "$stat" --labels "$labels" $panelout/c_clust_null3_bg.pdf

#Save the full figure
mkdir -p $figout
cpdf $panelout/a_spec_null3.pdf $panelout/b_nested_null3.pdf $panelout/c_clust_null3_bg.pdf -o $figout/S3.pdf

#----------------------------#
#---- Calculate p-values ----#
#----------------------------#

#Quantiles of value calculated from the data, given null and background distributions
$src/workflow/quantiles.r null3_full 5axes2000pts1
