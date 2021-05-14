

wd=~/projects/whitestork/results/stpp_models/huj_eobs
src=~/projects/ms1/src

cd $wd

#Create full dataset of 2000 points with 5 axes
dat=data/obsbg_anno.csv
npts=2000
axes=pct_tree,dist2urban,pct_bare,dist2forest,ndvi
out=~/projects/ms1/data/derived/obs_anno_2k.csv

Rscript $src/workflow/hvs_subsample.r $dat $npts $axes $out

#---
#--- on grace ----#
#---

#----------------#
#--- Full run ---#
#----------------#


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

#---- Pull down results

dir=bsnm
sesid=full_ci
wd=~/projects/ms1/analysis/$dir
src=~/projects/ms1/src

cd $wd

mkdir $wd/$sesid
scp grace:~/projects/ms1/analysis/$dir/$sesid/niche_stats.csv $wd/$sesid
scp grace:~/projects/ms1/analysis/$dir/$sesid/niche_set_stats.csv $wd/$sesid
scp grace:~/projects/ms1/analysis/$dir/$sesid/pairwise.csv $wd/$sesid

$src/poc/bootstrap/fig_metrics_null.r $sesid spec $sesid/figs/spec_null.pdf
$src/poc/bootstrap/fig_metrics_null.r $sesid nested $sesid/figs/pw_null.pdf
$src/poc/bootstrap/fig_metrics_null.r $sesid clust $sesid/figs/clust_null.pdf

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

wd=~/projects/whitestork/results/stpp_models/huj_eobs
cd $wd

$src/workflow/median_location.r data/dat.csv data/median_location.csv

#---- generate background points ----#

datName=full_bg_buf
wd=~/projects/ms1/analysis/bsnm

src=~/projects/ms1/src

cd $wd

#--- generate backgrounds using 2-week buffers
$src/workflow/bg_buffer.r ~/projects/ms1/data/derived/obs_anno.csv data/$datName.csv -t


#---- Annotate background points ----#

dat=data/$datName.csv #used in call to annotation script
gcsimport=gs://mol-playground/benc/ingest_ee/$datName
gcsanno=gs://mol-playground/benc/annotated/$datName
asset=users/benscarlson/annotate/tracks/$datName

pyenv activate geosp_poc #Activate the environment

annoprep $dat
annoimport -d $dat -g $gcsimport -a $asset
annoprocess -d $dat -g $gcsanno
annocleanup -d $dat -i $gcsimport -g $gcsanno

pyenv deactivate

#---------------------------#
#--- Make background hvs ---#
#---------------------------#

wd=~/projects/ms1/analysis/$dir
src=~/projects/ms1/src

cd $wd

#Create full dataset of 2000 points with 5 axes

npts=2000
axes=pct_tree,dist2urban,pct_bare,dist2forest,ndvi
out=$datsamp

Rscript $src/workflow/hvs_subsample.r $dat $npts $axes $out

# Move working folder to grace
scp -r $wd grace:~/projects/ms1/analysis
# Or, just move data file to grace
scp -r $wd/$datsamp grace:~/projects/ms1/analysis/$dir/data

#--- Execute on Grace ---#

dir=bsnm
export sesid=full_bg_buf
export dat=data/full_bg_buf_2k.csv

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

#---- pull down results ----#

cd $wd

mkdir $wd/$sesid
scp grace:~/projects/ms1/analysis/$dir/$sesid/niche_stats.csv $wd/$sesid
scp grace:~/projects/ms1/analysis/$dir/$sesid/niche_set_stats.csv $wd/$sesid
scp grace:~/projects/ms1/analysis/$dir/$sesid/pairwise.csv $wd/$sesid

#---- Load into database ----#

$src/db/load_hpc_csv.r $sesid/niche_stats.csv $sesid niche_stats -t
$src/db/load_hpc_csv.r $sesid/niche_set_stats.csv $sesid niche_set_stats -t
$src/db/load_hpc_csv.r $sesid/pairwise.csv $sesid pairwise -t

#-------------------------------------#
#---- Calculate and store metrics ----#
#-------------------------------------#


wd=~/projects/ms1/analysis/bsnm
src=~/projects/ms1/src

cd $wd

$src/workflow/rini.r $sesid
$src/workflow/nestedness.r $sesid
$src/workflow/cluster_weighted.r $sesid

#Quantiles of value calculated from the data, given null and background distributions 
$src/workflow/quantiles.r full_hvs,full_bg_buf 5axes2000pts1
#Confidence intervals of distributions
$src/workflow/metric_ci.r full_hvs,full_bg_buf,full_ci


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
