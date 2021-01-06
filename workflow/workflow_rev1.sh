

wd=~/projects/whitestork/results/stpp_models/huj_eobs
src=~/projects/ms1/src

cd $wd

#Create full dataset of 2000 points with 5 axes
dat=data/obsbg_anno.csv
npts=2000
axes=pct_tree,dist2urban,pct_bare,dist2forest,ndvi
out=~/projects/ms1/data/derived/obs_anno_2k.csv

Rscript $src/workflow/hvs_subsample.r $dat $npts $axes $out

#Create test dataset of 100 points with 2 axes
dat=data/obsbg_anno.csv
npts=100
axes=pct_tree,pct_bare
out=~/projects/ms1/data/derived/obs_anno_100.csv

Rscript $src/workflow/hvs_subsample.r $dat $npts $axes $out
  
#---- Running new version of the script local environment ----

#----
#---- Test Locally ----#
#----

#bsnm_test/test4 - two niches from two niche sets
#bsnm_test2/test1 - three niches from two niche sets
#bsnm_2env - pairs of environmental variables, pops from 2015
#bsnm_test/test_ci - 

wd=~/projects/ms1/analysis/bsnm_test
derived=~/projects/ms1/data/derived
src=~/projects/ms1/src

cd $wd

reps=2 
sesid=test_ci
out=$sesid 
bmode=ci 
dat=$derived/obs_anno_100.csv

$src/workflow/bsnm_hvs.r $dat $out $reps -b $bmode -r $sesid -t -p mc -c 4

$src/workflow/bsnm_nicheset.r $out $reps -r $sesid -t -p mc -c 4

$src/workflow/bsnm_pairwise.r $out $reps -r $sesid -t -p mc -c 4

# Move working folder to grace
scp -r $wd bc447@grace.hpc.yale.edu:~/projects/ms1/analysis
#Move shared data folder to grace
scp -r ~/projects/ms1/data bc447@grace.hpc.yale.edu:~/projects/ms1

#---
#--- on grace ----#
#---

#--- Testing on grace ---#

wd=~/projects/ms1/analysis/bsnm_test

#Even though using export command in sbatch, I still need to export these

#need these for all scripts
export src=~/projects/ms1/src
export reps=2
export sesid=test1
export out=$sesid

#need these only for bsnm_hvs.r
export dat=$derived/obs_anno_100.csv
export bmode=ci

#slurm variables
export n=10
export t=10:00
export p=scavenge
export J=$sesid
export mail=NONE

#Make sure to make the output directory or mpi will fail because mpi log files go here
cd $wd
mkdir -p $J

# These have to start with a --option b/c echo won't print - as first character
pars=`echo --ntasks $n -p $p -t $t -J $J --mail-type $mail`
exp=`echo --export=ALL,n=$n,p=$p,t=$t,J=$J,mail=$mail`

export logs=$out/logs/mpi_hvs
sbatch $pars $exp $src/hpc/bsnm_hvs.sh

export logs=$out/logs/mpi_nicheset
sbatch $pars $exp $src/hpc/bsnm_nicheset.sh

export logs=$out/logs/mpi_pw
sbatch $pars $exp $src/hpc/bsnm_pairwise.sh

squeue -l -u bc447
sacct -j 6555211 --format=JobID,JobName,Partition,AllocCPUS,MaxRSS,Elapsed

#----------------#
#--- Full run ---#
#----------------#
#bsnm/full_hvs - null model hvs for all individuals
#    /full_ci - ci hvs for all individuals
#bsnm_2015 - all individuals from 2015. runtimes-hvs:45 min
#bsnm_2env - pairwise 
#   tree_urban - pct_tree,dist2urban

wd=~/projects/ms1/analysis/bsnm
derived=~/projects/ms1/data/derived

#need these for all scripts
export src=~/projects/ms1/src
export reps=101
export sesid=full_ci
export out=$sesid

#need these only for bsnm_hvs.r
export dat=$derived/obs_anno_2k.csv
export bmode=ci

#slurm variables
export n=750
export t=8:00:00
export p=day
export J=$sesid
export mail=ALL

#Make sure to make the output directory or mpi will fail because mpi log files go here
cd $wd
mkdir -p $J

pars=`echo --ntasks $n -p $p -t $t -J $J --mail-type $mail`
exp=`echo --export=ALL,n=$n,p=$p,t=$t,J=$J,mail=$mail`

export logs=$out/logs/mpi_hvs
sbatch $pars $exp $src/hpc/bsnm_hvs.sh

export logs=$out/logs/mpi_nicheset
sbatch $pars $exp $src/hpc/bsnm_nicheset.sh

export logs=$out/logs/mpi_pw
sbatch $pars $exp $src/hpc/bsnm_pairwise.sh

squeue -l -u bc447

scancel 6420876

#---- Pull down results

 
#full2ns2 - ran with two niche sets. But, rep was incorrectly calculated in output file.
#bsnm/ all niche sets, 2k points, 5 axes, 101 reps
  #full_hvs null model results
  #full_ci  ci results
#bsnm_2015/full_hvs - niche sets from 2015. 2k points. 101 reps
dir=bsnm
sesid=full_ci
wd=~/projects/ms1/analysis/$dir
src=~/projects/ms1/src

cd $wd

mkdir $wd/$sesid
scp bc447@grace.hpc.yale.edu:~/projects/ms1/analysis/$dir/$sesid/niche_stats.csv $wd/$sesid
scp bc447@grace.hpc.yale.edu:~/projects/ms1/analysis/$dir/$sesid/niche_set_stats.csv $wd/$sesid
scp bc447@grace.hpc.yale.edu:~/projects/ms1/analysis/$dir/$sesid/pairwise.csv $wd/$sesid

#chmod 744 $src/poc/bootstrap/fig_metrics_null.r #Use to make executable
$src/poc/bootstrap/fig_metrics_null.r $sesid spec $sesid/figs/spec_null.pdf
$src/poc/bootstrap/fig_metrics_null.r $sesid nested $sesid/figs/pw_null.pdf
$src/poc/bootstrap/fig_metrics_null.r $sesid clust $sesid/figs/clust_null.pdf

#Load the data into the database

#chmod 744 $src/db/load_niche_stats.r
#chmod 744 $src/db/load_hpc_csv.r

#TODO: Can use the approach towards the bottom
#TODO: can use load_hpc_csv.r instead.
#Load niche_stats. Adjust non-standard file  names
sed -i ".bak" "1s/runid/ses\_id/" full_hvs/niche_stats.csv
$src/db/load_niche_stats.r full_hvs
sed -i ".bak" "1s/sesid/ses\_id/" full_ci/niche_stats.csv
$src/db/load_niche_stats.r full_ci -t

#Load niche_set_stats. Adjust non-standard file  names
sed -i ".bak" "1s/sesid/ses\_id/" full_hvs/niche_set_stats.csv
sed -i ".bak" "1s/nset_vol/niche\_set\_vol/" full_hvs/niche_set_stats.csv
$src/db/load_hpc_csv.r full_hvs/niche_set_stats.csv full_hvs niche_set_stats -t

sed -i ".bak" "1s/sesid/ses\_id/" full_ci/niche_set_stats.csv
sed -i ".bak" "1s/nset_vol/niche\_set\_vol/" full_ci/niche_set_stats.csv
$src/db/load_hpc_csv.r full_ci/niche_set_stats.csv full_ci niche_set_stats -t

#Can use this approach 
sesid=full_hvs table=pairwise
sesid=full_ci table=pairwise

sed -i ".bak" "1s/sesid/ses\_id/" $sesid/$table.csv
$src/db/load_hpc_csv.r $sesid/$table.csv $sesid $table -t

#-------------------------------------#
#---- Calculate and store metrics ----#
#-------------------------------------#

#NOTE: use -t when testing, it will roll back the transaction
$src/workflow/rini.r full_hvs,full_ci
$src/workflow/nestedness.r full_hvs,full_ci
$src/workflow/cluster_weighted.r full_hvs,full_ci

chmod 744 $src/poc/bootstrap/fig_metrics_hist.r

sesid='full_hvs,full_ci'

$src/poc/bootstrap/fig_metrics_hist.r $sesid spec figs/spec_null_ci.pdf
$src/poc/bootstrap/fig_metrics_hist.r $sesid nested figs/nested_null_ci.pdf
$src/poc/bootstrap/fig_metrics_hist.r $sesid clust figs/clust_null_ci.pdf

#-------------------------------------------#
#---- Generate and annotate backgrounds ----#
#-------------------------------------------#

#---- central locations ----#
#need to get a central location for individuals that are not breeding
#only need to run this once for all data
#TODO: make this a field in breeding data?
wd=~/projects/whitestork/results/stpp_models/huj_eobs
cd $wd

$src/workflow/median_location.r data/dat.csv data/median_location.csv

#---- generate background points ----#

#-- Backgrounds using 2-week buffer, for loburg 2015
#datName=bg_2wk_lbg2015 #backgrounds for loburg 2015 for two week buffers based on 95% dist2nest
#wd=~/projects/ms1/analysis/bg_lbg2015

#-- Backgrounds using akde, for loburg 2015
# datName=bg_hr_lbg2015 #backgrounds for loburg 2015 using home ranges
# wd=~/projects/ms1/analysis/bg_lbg2015

#-- Backgrounds using 2-week buffer, all niches
# datName=full_bg_buf #backgrounds for all niche sets for two week buffers based on 95% dist2nest
# wd=~/projects/ms1/analysis/bsnm


#-- Backgrounds using akde, for all niches
# datName=full_bg_hr
# wd=~/projects/ms1/analysis/bsnm
# 
# src=~/projects/ms1/src
# 
# cd $wd

#--- generate backgrounds using 2-week buffers
$src/workflow/bg_buffer.r ~/projects/ms1/data/derived/obs_anno.csv data/$datName.csv -t

# chmod 744 $src/workflow/bg_homerange.r #Use to make executable
#$src/workflow/bg_homerange.r ~/projects/ms1/data/derived/obs_anno.csv data/$datName.csv -t

#---- Annotate background points ----#

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

#---------------------------#
#--- Make background hvs ---#
#---------------------------#

# dir=bg_lbg2015
# dat=data/bg_hr_lbg2015_anno.csv
# datsamp=data/bg_hr_lbg2015_2k.csv
# sesid=bg_hr_lbg2015

# wd=~/projects/ms1/analysis/bsnm
# dat=data/full_bg_buf_anno.csv
# datsamp=data/full_bg_buf_2k.csv

# dir=bsnm
# dat=data/full_bg_hr_anno.csv
# datsamp=data/full_bg_hr_2k.csv


wd=~/projects/ms1/analysis/$dir
src=~/projects/ms1/src

cd $wd

#Create full dataset of 2000 points with 5 axes

npts=2000
axes=pct_tree,dist2urban,pct_bare,dist2forest,ndvi
out=$datsamp

Rscript $src/workflow/hvs_subsample.r $dat $npts $axes $out

#Copied backgrounds to bg_lbg2015, cleaned up manually, use this as the working folder for first hpc run
#bg_lbg2015/hvs - full background hvs for loburg 2015
#bsnm/full_bg_buf - full background hvs for all niches

# Move working folder to grace
scp -r $wd bc447@grace.hpc.yale.edu:~/projects/ms1/analysis
# Or, just move data file to grace
scp -r $wd/$datsamp bc447@grace.hpc.yale.edu:~/projects/ms1/analysis/$dir/data

#--- Execute on Grace ---#

#need these for all scripts

dir=bsnm
export sesid=full_bg_hr
export dat=data/full_bg_hr_2k.csv

# dir=bg_lbg2015
# export sesid=bg_hr_lbg2015
# export dat=data/bg_hr_lbg2015_2k.csv

wd=~/projects/ms1/analysis/$dir

export src=~/projects/ms1/src
export reps=101
export out=$sesid
export bmode=ci
export J=$sesid

#slurm variables
#full, with n=750, hvs took 2 hours, nicheset took 27 minutes, pairwise took 7 hours
#lbg2015. with n=750, hvs took 20 minutes, nicheset took 20 minutes, pairwise took 40 minutes
export n=750
export t=10:00:00
export p=day
export mail=ALL

#Make sure to make the output directory or mpi will fail because mpi log files go here
cd $wd
mkdir -p $J

pars=`echo --ntasks $n -p $p -t $t -J $J --mail-type $mail`
exp=`echo --export=ALL,n=$n,p=$p,t=$t,J=$J,mail=$mail`

export logs=$out/logs/mpi_hvs
sbatch $pars $exp $src/hpc/bsnm_hvs.sh

export logs=$out/logs/mpi_nicheset
sbatch $pars $exp $src/hpc/bsnm_nicheset.sh

export logs=$out/logs/mpi_pw
sbatch $pars $exp $src/hpc/bsnm_pairwise.sh

squeue -l -u bc447

scancel 7604913

#---- pull down results ----#

cd $wd

mkdir $wd/$sesid
scp bc447@grace.hpc.yale.edu:~/projects/ms1/analysis/$dir/$sesid/niche_stats.csv $wd/$sesid
scp bc447@grace.hpc.yale.edu:~/projects/ms1/analysis/$dir/$sesid/niche_set_stats.csv $wd/$sesid
scp bc447@grace.hpc.yale.edu:~/projects/ms1/analysis/$dir/$sesid/pairwise.csv $wd/$sesid

#---- Load into database ----#

#Adjust non-standard file  names
#TODO: update scripts to have the correct col names
#sed -i ".bak" "1s/sesid/ses\_id/" $sesid/niche_stats.csv 
#sed -i ".bak" "1s/sesid/ses\_id/" $sesid/niche_set_stats.csv
#sed -i ".bak" "1s/nset_vol/niche\_set\_vol/" $sesid/niche_set_stats.csv
#sed -i ".bak" "1s/sesid/ses\_id/" $sesid/pairwise.csv

$src/db/load_hpc_csv.r $sesid/niche_stats.csv $sesid niche_stats -t
$src/db/load_hpc_csv.r $sesid/niche_set_stats.csv $sesid niche_set_stats -t
$src/db/load_hpc_csv.r $sesid/pairwise.csv $sesid pairwise -t

#-------------------------------------#
#---- Calculate and store metrics ----#
#-------------------------------------#


wd=~/projects/ms1/analysis/bsnm
src=~/projects/ms1/src

cd $wd

#NOTE: use -t when testing, it will roll back the transaction
$src/workflow/rini.r $sesid
$src/workflow/nestedness.r $sesid
$src/workflow/cluster_weighted.r $sesid

#Quantiles of value calculated from the data, given null and background distributions 
$src/workflow/quantiles.r full_hvs,full_bg_buf 5axes2000pts1
#Confidence intervals of distributions
$src/workflow/metric_ci.r full_hvs,full_bg_buf,full_ci

#--- Figures ----#

#!! NOTE !! Moved bsnm_hist.r to src/figs
sesids="full_hvs,full_bg_buf,full_bg_hr,full_ci"
labels="Null dist.,Background (buffer),Background (home range),Sampling dist."
stat=5axes2000pts1

$src/poc/bootstrap/fig_metrics_hist.r $sesids spec --stat "$stat" --labels "$labels" figs/spec_null_ci_buf_hr.pdf
$src/poc/bootstrap/fig_metrics_hist.r $sesids nested --stat "$stat" --labels "$labels" figs/nested_null_ci_buf_hr.pdf
$src/poc/bootstrap/fig_metrics_hist.r $sesids clust --stat "$stat" --labels "$labels" figs/clust_null_ci_buf_hr.pdf

cpdf figs/spec_null_ci_buf_hr.pdf figs/nested_null_ci_buf_hr.pdf figs/clust_null_ci_buf_hr.pdf -o metrics_null_ci_buf_hr.pdf

#-------------------#
#---- Figure S3 ----#
#-------------------#

wd=~/projects/whitestork/results/stpp_models/huj_eobs
src=~/projects/ms1/src
figout=~/projects/ms1/docs/ms/submission_natcom/submission_2/v22/figs

cd $wd

sesids="full_hvs,full_bg_buf"
labels="Individual identity,Environmental availability"
stat=5axes2000pts1

$src/figs/bsnm_hist.r $sesids spec --stat "$stat" --labels "$labels" $figout/sub_bsnm/a_spec_null_bg.pdf
$src/figs/bsnm_hist.r $sesids nested --stat "$stat" --labels "$labels" $figout/sub_bsnm/b_nested_null_bg.pdf
$src/figs/bsnm_hist.r $sesids clust --stat "$stat" --labels "$labels" $figout/sub_bsnm/c_clust_null_bg.pdf

files=`ls $figout/sub_bsnm/*.pdf`

cpdf $files -o $figout/S3.pdf

#-------------------#
#---- Figure S4 ----#
#-------------------#

sesid=5axes2000pts1
hr=ctmm/akde_contour/contours.shp
out=~/projects/ms1/docs/ms/submission_natcom/submission_2/v22/figs/S4.pdf

$src/figs/hr_niche_size.r $sesid $hr $out

#-------------------#
#---- Figure S5 ----#
#-------------------#
sesid=5axes2000pts1
out=~/projects/ms1/docs/ms/submission_natcom/submission_2/v22/figs/S5.pdf

$src/figs/sex_niche_size.r $sesid $out

#-----------------#
#---- File S1 ----#
#-----------------#

$src/supp_files/niche_vol_ci.r ~/projects/ms1/docs/ms/submission_natcom/submission_2/v22/supp_files/S1.csv
