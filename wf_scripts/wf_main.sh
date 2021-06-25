
#---- Start script init ----#

#-- parameters
datName=huj_eobs
#-- paths
srcP=~/projects/ms1/src
reportP=$srcP/reports
scriptsP=$srcP/scripts
reportOutP=~/projects/ms1/analysis/$datName/reports


msfigP=~/projects/ms1/docs/ms/v14/figs
akde=ctmm/akde_contour/contours.shp

#-- parameters used in workflow
hvjob=5axes2000pts1 #name of the hypervolume result set
mod=mod4 #The name of the rsf model to use. See models.csv

#working directory should be the specific results folder
#mkdir -p ~/projects/whitestork/results/stpp_models/$datName && cd "$_"
cd ~/projects/whitestork/results/stpp_models/$datName

#---- End Script init ----#

#-- annotation
#TODO: need to test to make sure these paths still work
dat=data/obsbg.csv #used in call to annotation script
gcsimport=gs://mol-playground/benc/ingest_ee/$datName
gcsanno=gs://mol-playground/benc/annotated/$datName
asset=users/benscarlson/annotate/tracks/$datName

#----
#---- Prep workflow ----#
#----

#set up niches.csv file using /src/scripts/rsf/niches.r. TODO: move this to rsf workflow
#set up settings.yml file. use ~/projects/rsf/settings_template.yml

#----
#---- Segment migration ----#
#----

#fine-tune phen2 table based on reports
#TODO: uses early version of runcopy/.p system
Rscript $whitestorkP/reports/report_finetune_mig/report_finetune_mig_mult.r \
  ~/projects/whitestork/results/seg_mig/$datName

Rscript $scriptsP/report_runcopy2.r \
  $whitestorkP/reports/report_finetune_mig/time_queries.rnw \
  ~/projects/whitestork/results/seg_mig/$datName/time_queries.pdf

#----
#---- Set up data ----#
#----

#TODO: check to make sure this can't overwrite an existing database
$scriptsP/database/create_db.sh

#extract telemetry data from shay's dataset and save to working folder
$scriptsP/extract_inds.r

$scriptsP/outliers.r

$scriptsP/obs.r 

$scriptsP/trim_dates.r data/obs.csv data/obs_trim.csv

#--- set up hpc for analyses
ssh grace "mkdir -p ~/results/$datName" #create directory on hpc
ssh grace "mkdir -p ~/results/$datName/data" #create directory on hpc
scp niches.csv grace:~/results/$datName
scp data/obs_trim.csv grace:~/results/$datName/data

#---- variogram analysis ----#

#-- run on grace
ssh grace "cd ~/results/$datName && sbatch ~/projects/rsf/config/variograms_mpi.sh" #run this on hpc, from appropriate results directory

#-- get results
scp -r grace:~/results/$datName/ctmm .

#---- akde ----#

#-- run on grace
scp niches.csv grace:~/results/$datName
ssh grace "cd ~/results/$datName && sbatch ~/projects/rsf/config/akde_mpi.sh" #run this on hpc, from appropriate results directory

scp -r grace:~/results/$datName/ctmm/akde ctmm/.

#-- save akde contours as shapefiles
$scriptsP/akde2shape.r ctmm/akde ctmm/akde_contour/contours.shp

$scriptsP/background_steps.r data/obs_trim.csv data/obsbg.csv

#-----------------------------#
#---- Download GEE Layers ----#
#-----------------------------#

geeDatName=${datName}

#Run GEE script: layers/export_image.js
mkdir layers
gsutil -m cp -r gs://benc/layer_export/$geeDatName/* layers

#Masked pixels are set to a particular integer value (e.g. 255) in ee using unmask(255).
#But, 255 is not yet set as the nodata value on the tif

gdalinfo -mm layers/pct_tree_30m.tif #note max is 255
gdalinfo -mm layers/pct_tree_30m.tif #does not seem to have masked values

#So, need to define the nodata value
gdal_edit.py -a_nodata 255 layers/pct_tree_30m.tif
gdal_edit.py -a_nodata 255 layers/pct_bare_30m.tif

#------------------------------#
#---- GEE based annotation ----#
#------------------------------#

annoprep $dat
annoimport -d $dat -g $gcsimport -a $asset
#annotate in playground: annotate/annotate_by_static_or_ts.js
annoprocess -d $dat -g $gcsanno
annocleanup -d $dat -i $gcsimport -g $gcsanno

#---------------------------------------------------------------#
#---- do another annotation on an already-annotated dataset ----#
#---------------------------------------------------------------#

#annotate in playground: annotate/annotate_by_static_or_ts.js
annoprocess -d $dat -g $gcsanno
annocleanup -d $dat -i $gcsimport -g $gcsanno

#------------------------------#
#---- Local annotation ----#
#------------------------------#

#param1: relative path to datafile to be annotated.
Rscript $scriptsP/anno_local.r data/obsbg_amt_anno.csv

cat data/obsbg_anno.csv | wc -l 
cat data/sav/obsbg_anno.csv | wc -l
rm data/sav/obsbg_anno.csv #clean up - remove backup file

#------------------#
#---- Modeling ----#
#------------------#

#see model_amt.r

#------------------------#
#---- niche analysis ----#
#------------------------#

#prep hpc
ssh grace "mkdir -p ~/results/$datName" #create directory on hpc
ssh grace "mkdir -p ~/results/$datName/data" #create directory on hpc
scp niches.csv grace:~/results/$datName
scp data/obsbg_anno.csv grace:~/results/$datName/data

#----
#---- create individual hypervolumes on the hpc
#----

#Make sure to open scripts for parameter settings
scp grace:~/projects/rsf/config/* ~/projects/ms1/src/hpc
sbatch ~/projects/rsf/config/hvs_indiv_mpi.sh #run this on hpc, from appropriate results directory
scp -r grace:~/results/$datName/hvs/$hvjob hvs/$hvjob #get results from hpc

#----
#---- create population hypervolumes
#----

Rscript $scriptsP/hv/hvs_niche_set.r hvs/$hvjob -s hvs/$hvjob/hvs_niche_set_df3 -d 3 -p mc -c 6

#----
#---- Niche metrics
#----

Rscript $srcP/scripts/hv_pairwise.r $hvjob -i 1 -u 3 -p mc -c 6
$srcP/database/load_pairwise.r $hvjob

#Calculate nestedness metric and store in database
$srcP/scripts/nestedness.r $hvjob

#----
#---- nmds analysis of niche sets
#----

#run on hpc
scp niche_sets.csv grace:~/results/$datName
ssh grace "cd ~/results/$datName && sbatch ~/projects/rsf/config/nmds_hv_mpi.sh" #run this on hpc, from appropriate results directory

#get results
scp -r grace:~/results/$datName/hvs/5axes2000pts1/nmds hvs/5axes2000pts1
scp -r grace:~/results/$datName/drom13 hvs/5axes2000pts1/nmds #get dromling 2013 which I had to run seperately

scp -r grace:~/results/$datName/hvs/5axes2000pts1/nmds1000 hvs/5axes2000pts1

#----
#---- Repeatability 
#----

$srcP/workflow/repeatability.r $hvjob $mod

#----
#---- Figures
#----


$srcP/figs/indiv_pal.r figs/ms/indiv_pal.csv #Create individual-level palette

#--------------------------------------#
#---- Final figures for manuscript ----#
#--------------------------------------#

year=2015

#---- Figure 2. SSF Plots
$srcP/figs/rsf.r loburg $year $mod $msfigP/2_rsf_loburg_${year}.png
$srcP/figs/rsf.r loburg $year $mod $msfigP/2_rsf_loburg_${year}.pdf

#---- Figure 3. AKDE, NMDS & Metrics
$srcP/figs/niches.r $hvjob $akde $year 11,10,10 $msfigP/3_niches_${year}.png

#---- Figure 4. Metrics over time
$srcP/figs/metrics_time.r $hvjob $msfigP/4_metrics_time.png

#--------------------------------------#
#---- Final figures for supplement ----#
#--------------------------------------#

msfigP=~/projects/ms1/docs/ms/submission_natcom/submission_2/v24

#---- Figure S1. SSF Plots

outP=$msfigP/supp_figs/S1_sub

mkdir -p $outP

pops=("beuster" "dromling" "loburg")
years=("2013" "2014" "2015" "2016")

for pop in "${pops[@]}"
do
  for year in "${years[@]}"
  do
    out=$outP/S1_rsf_${pop}_${year}.pdf
    $srcP/figs/rsf.r $pop $year $mod $out --mode=supp
  done
done


/Users/benc/projects/ms1/docs/ms/submission_natcom/submission_2/v24/supp_figs/S1_sub
/Users/benc/projects/ms1/docs/ms/submission_natcom/submission_2/v24/supp_figs/S1_sub/S1_rsf_*.pdf

ls /Users/benc/projects/ms1/docs/ms/submission_natcom/submission_2/v24/supp_figs/S1_sub

files=`ls $outP/S1_rsf_*.pdf`
cpdf $files -o $msfigP/supp_figs/Fig_S1.pdf

#---- Figure S2. AKDE, NMDS & Metrics

outP=$msfigP/supp/s2_niches

mkdir -p $outP

years=("2013" "2014" "2016") # 

for year in "${years[@]}"
do
  out=${outP}/S2_${year}.pdf
  $srcP/figs/niches.r $hvjob $akde $year 11,10,10 $out --mode=supp
done

#Combine all years into single report

files=`ls $outP/S2_*.pdf`
cpdf $files -o $msfigP/S2_niches.pdf
