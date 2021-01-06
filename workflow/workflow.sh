
#---- Start script init ----#

#-- parameters
datName=huj_eobs
#-- paths
srcP=~/projects/ms1/src
reportP=$srcP/reports
scriptsP=$srcP/scripts
reportOutP=~/projects/whitestork/results/stpp_models/$datName/reports


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

#Rscript ~/projects/whitestork/src/reports/report_finetune_mig/report_finetune_mig_single.r $datName

#----
#---- Set up data ----#
#----

#TODO: check to make sure this can't overwrite an existing database
$scriptsP/database/create_db.sh

#extract telemetry data from shay's dataset and save to working folder
$scriptsP/extract_inds.r

#TODO: would be nice to include avg speed, and other info for each outlier
# Also look at some distribution of speeds or something to better parameterize
$scriptsP/outliers.r

knitcopy $reportP/outliers/outliers.rnw $reportOutP/outliers.pdf

#revisit this, maybe break into multiple reports.
#Rscript $reportP/report_mvtrack/report_mvtrack_mult.r unfilt

#TODO: accept path for input file
# Note I obs.r also removes outliers; filter_outliers.r is only used in special occasions
#Rscript $scriptsP/filter_outliers.r


#Note: "breeding data" (from franzmitters) has 2012, 2013, 2014
# "stork_breeding_data" table has 2013-2015. Merge in 2012 from franzmitters?
# generate my own for breeding storks in 2016?
#any stork that does not have a nest will not have points w/in 100m of nest removed.
$scriptsP/obs.r 

knitcopy $reportP/dates/dates.rnw $reportOutP/dates.pdf #make sure input file is set to obs.csv

$scriptsP/trim_dates.r data/obs.csv data/obs_trim.csv

knitcopy $reportP/dates/dates.rnw $reportOutP/dates_trimmed.pdf #make sure input file is set to obs_trim.csv

#--- set up hpc for analyses
ssh grace "mkdir -p ~/results/$datName" #create directory on hpc
ssh grace "mkdir -p ~/results/$datName/data" #create directory on hpc
scp niches.csv grace:~/results/$datName
scp data/obs_trim.csv grace:~/results/$datName/data

#---- variogram analysis ----#

#-- test locally
Rscript $scriptsP/variograms.r data/obs_trim.csv #run sequentially
Rscript $scriptsP/variograms.r data/obsbg_anno.csv -p mc -c 5 #run doMC with 5 cores

#-- run on grace
ssh grace "cd ~/results/$datName && sbatch ~/projects/rsf/config/variograms_mpi.sh" #run this on hpc, from appropriate results directory

#-- get results
scp -r grace:~/results/$datName/ctmm .

knitcopy $reportP/ctmm/info.Rnw $reportOutP/ctmm/info.pdf

#---- akde ----#

#--test locally
Rscript $scriptsP/akde.r #run sequentially
Rscript $scriptsP/akde.r -p mc -c 3 #run using doMc

#-- run on grace
scp niches.csv grace:~/results/$datName
ssh grace "cd ~/results/$datName && sbatch ~/projects/rsf/config/akde_mpi.sh" #run this on hpc, from appropriate results directory

scp -r grace:~/results/$datName/ctmm/akde ctmm/.

#-- save akde contours as shapefiles
$scriptsP/akde2shape.r ctmm/akde ctmm/akde_contour/contours.shp

#Rscript $scriptsP/report_runcopy2.r $reportP/report_pop/report_pop.rnw $reportOutP/report_pop.pdf
#TODO: 
#  make population-level output
#  handle weird errors
#  don't now nest location map if there are no nest locations
#  add scale bars to all maps
knitcopy $reportP/pop/pop.rnw $reportOutP/pop.pdf

#TODO: update this with ctmm?
Rscript $scriptsP/report_runcopy2.r $reportP/homerange/homerange.rnw $reportOutP/homerange.pdf

#Rscript $reportP/report_mvtrack/report_mvtrack_mult.r filt #need to fix this/rework the report

$scriptsP/background_steps.r data/obs_trim.csv data/obsbg.csv

#Rscript $scriptsP/quads.r

#Rscript $scriptsP/report_runcopy2.r $reportP/quads/quads.rnw $reportOutP/quads.pdf


#---------------------------#
#---- Make local layers ----#
#---------------------------#

#TODO: I should calculate this directly instead of taking a raster and then sampling.
#NOTE: see make_dist2nest2.r. That script has a better way of calculating dist2nest.
Rscript $scriptsP/layers/make_dist2nest.r 30

#Envs reports - Not sure which one I should use.
#TODO: shouldn't this come after downloading gee layers?
Rscript $scriptsP/report_runcopy2.r $reportP/report_envs/report_envs.rnw $reportOutP/envs/dist2nest_250m.pdf #have to set .envLabel inside .rnw file
Rscript $scriptsP/report_runcopy.r report_envs

#-----------------------------#
#---- Download GEE Layers ----#
#-----------------------------#

geeDatName=${datName}_lbg_amt

#Run GEE script: layers/export_image.js
mkdir layers
gsutil -m cp -r gs://benc/layer_export/$geeDatName/* layers

#Masked pixels are set to a particular integer value (e.g. 255) in ee using unmask(255).
#But, 255 is not yet set as the nodata value on the tif

gdalinfo -mm layers/pct_tree_30m.tif #note max is 255
gdalinfo -mm layers/pct_tree_30m.tif #does not seem to have masked values

#So, need to define the nodata value
#TODO: need to make this more general
gdal_edit.py -a_nodata 255 layers/pct_tree_30m.tif
gdal_edit.py -a_nodata 255 layers/pct_bare_30m.tif

#------------------------------#
#---- GEE based annotation ----#
#------------------------------#

annoprep $dat #TODO: don't overwrite *_anno.csv if it is already there!
annoimport -d $dat -g $gcsimport -a $asset
#annotate in playground: annotate/annotate_by_static_or_ts.js
annoprocess -d $dat -g $gcsanno #TODO: in many cases NaN comes back instead of NA. Why?
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
#param2: TODO: (optional) name of output file. if not supplied, adds annotations to existing file. 
Rscript $scriptsP/anno_local.r data/obsbg_amt_anno.csv

cat data/obsbg_anno.csv | wc -l 
cat data/sav/obsbg_anno.csv | wc -l
rm data/sav/obsbg_anno.csv #clean up - remove backup file

#------------------------------#
#---- annotation reports ----#
#------------------------------#

knitcopy $reportP/anno/corr.Rnw $reportOutP/anno/correlation.pdf #run field: envs. set datPFN
knitcopy $reportP/anno/na.Rnw $reportOutP/anno/na.pdf #run field: envs. set datPFN


#[1] file with annotated data
#[2] where to copy reports
#Script assumes that these are relative path names
#TODO: could do one figure that has data for all individuals
# Also could do one figure that has individual data layered on top of each other.
#TODO: right now discrete variables are hard-coded to 'DFD_LULC'
Rscript $reportP/anno/multvar.r data/obsbg_lbg_amt_anno.csv reports/anno/lbg_amt #make sure to set envs.csv run column.

#One plot per population & env var. Note by population, not by niche_set!!
#TODO: what is the point of this report??
Rscript $scriptsP/report_runcopy2.r $reportP/anno/nicheset.rnw $reportOutP/anno_nicheset.pdf

#------------------#
#---- Modeling ----#
#------------------#

#This is old, see model_amt.r
# Rscript $scriptsP/model_prep.r #run field: envs
# 
# Rscript $scriptsP/model.r #set run field: models, envs.
# 
# Rscript $scriptsP/model_summary.r #run fields: models.
# 
# Rscript $scriptsP/predict.r
# 
# Rscript $scriptsP/eval.r

knitcopy $reportP/models/coef_over_time.Rnw $reportOutP/models/coef_over_time.pdf #run field: envs. set .modName

#TODO: !!! clip to size of pts. right now prediction is for all of background
Rscript $reportP/report_model_fit/report_model_fit_mult.r $reportOutP/models

Rscript $scriptsP/report_runcopy2.r $reportP/model_comparison/model_comparison.rnw $reportOutP/models_summary/summary_1param_30m.pdf #have to set params in .rnw file
Rscript $scriptsP/report_runcopy2.r $reportP/model_comparison/model_comparison.rnw $reportOutP/models_summary/summary_1param_250m.pdf #have to set params in .rnw file

Rscript $scriptsP/report_runcopy2.r $reportP/model_comparison/model_comparison.rnw $reportOutP/models_summary/summary_2param_30m.pdf #have to set params in .rnw file
Rscript $scriptsP/report_runcopy2.r $reportP/model_comparison/model_comparison.rnw $reportOutP/models_summary/summary_2param_250m.pdf #have to set params in .rnw file

Rscript $scriptsP/report_runcopy2.r $reportP/model_comparison/model_comparison.rnw $reportOutP/models_summary/summary_4param_30m.pdf #have to set params in .rnw file
Rscript $scriptsP/report_runcopy2.r $reportP/model_comparison/model_comparison.rnw $reportOutP/models_summary/summary_4param_250m.pdf #have to set params in .rnw file
#Rscript $scriptsP/report_runcopy2.r $reportP/model_comparison/model_comparison.rnw $reportOutP/models_summary/summary_5param_250m.pdf #have to set params in .rnw file

knitcopy $reportP/res_sel/res_sel.Rnw $reportOutP/Magnus_2013_dist2forest.pdf
#----
#---- niche analysis ----#
#----

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

#load volume data into database using scripts/database/load_indiv_vols.r

#----
#---- create population hypervolumes
#----
# I also moved this from ms1_poc/src/scripts/hv to ms1/src/workflow
Rscript $scriptsP/hv/hvs_niche_set.r hvs/$hvjob -s hvs/$hvjob/hvs_niche_set_df3 -d 3 -p mc -c 6

#Run script:
# scripts/database/load_niche_set_vols.r #load volume data into database

#----
#---- Niche metrics
#----

#Run script: 
# moved this from ms1_poc to ms1/src/workflow
# src/scripts/rini.r #Calculates individual and population level rini, updates the database

Rscript $srcP/scripts/hv_pairwise.r $hvjob -i 1 -u 3 -p mc -c 6
$srcP/database/load_pairwise.r $hvjob

#Calculate nestedness metric and store in database
$srcP/scripts/nestedness.r $hvjob

# knitcopy $reportP/niches/pop_spec_by_time.rnw $reportOutP/hv/4axes1000pts/pop_spec_by_time.pdf --params hvP=hvs/4axes1000pts
# knitcopy $reportP/niches/pop_specialization.rnw $reportOutP/hv/lbg_3axes_250pts_specialization.pdf
# knitcopy $reportP/niches/hv_plots.rnw $reportOutP/hv/$hvjob/centroids.pdf --params hvP=hvs/$hvjob,centroids=T,contours=T,popCont=F
# knitcopy $reportP/niches/hv_plots.rnw $reportOutP/hv/$hvjob/centroids_popcontour.pdf --params hvP=hvs/$hvjob,centroids=T,contours=F,popCont=T

#----
#---- nmds analysis of niche sets
#----

# run locally with minimum points
Rscript $scriptsP/nmds_hv.r hvs/5axes2000pts1 20 #run sequentially
Rscript $scriptsP/nmds_hv.r hvs/5axes2000pts1 20 --parMethod mc --cores 2

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
#chmod 744 $srcP/workflow/repeatability.r #Use to make executable

$srcP/workflow/repeatability.r $hvjob $mod

#----
#---- Figures
#----

#chmod 744 $srcP/figs/indiv_pal.r #Use to make executable

$srcP/figs/indiv_pal.r figs/ms/indiv_pal.csv #Create individual-level palette

#--
#-- EDA versions of figures ----#
#-- 

#---- Figure 2. SSF Plots
#Look at figures for all pops/years

mkdir -p figs/ms/rsf

pops=("beuster" "dromling" "loburg")
years=("2013" "2014" "2015" "2016")

#pops=("beuster"); years=("2013")

for pop in "${pops[@]}"
do
  for year in "${years[@]}"
  do
    out=figs/ms/rsf/2_rsf_${pop}_${year}.pdf
    $srcP/figs/2_rsf.r $pop $year $mod $out
  done
done

#Combine all years into single report
rm figs/ms/rsf/2_rsf_all_years.pdf
files=`ls figs/ms/rsf/2_rsf_*.pdf`
cpdf $files -o figs/ms/rsf/2_rsf_all_years.pdf

#---- Figure 3. NMDS & Metrics

mkdir -p $msfigP/alt/3_niches

years=("2013" "2014" "2015" "2016") # 
#years=('2015')
for year in "${years[@]}"
do
  out=$msfigP/alt/3_niches/3_niches_${year}.pdf
  $srcP/figs/niches.r -e $hvjob $akde $year 11,10,10 $out
done

#Combine all years into single report
rm $msfigP/alt/3_niches_all_years.pdf
files=`ls $msfigP/alt/3_niches/3_niches_*.pdf`
cpdf $files -o $msfigP/alt/3_niches_all_years.pdf

#---- Figure 4. Metrics over time

mkdir -p figs/ms

out=figs/ms/4_metrics_time.pdf
$srcP/figs/metrics_time.r $hvjob $out

#--------------------------------------#
#---- Final figures for manuscript ----#
#--------------------------------------#

year=2015
#---- Figure 2. AKDE Map
#$srcP/figs/2_akde.r ctmm/akde_contour/contours.shp $year $msfigP/2_akde_${year}.png

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

#pops=("beuster"); years=("2013")

for pop in "${pops[@]}"
do
  for year in "${years[@]}"
  do
    out=$outP/S1_rsf_${pop}_${year}.pdf
    $srcP/figs/rsf.r $pop $year $mod $out --mode=supp
  done
done

#Remove loburg 2015 because that is featured as Figure 2.
#rm $outP/S1_rsf_loburg_2015.pdf

/Users/benc/projects/ms1/docs/ms/submission_natcom/submission_2/v24/supp_figs/S1_sub
/Users/benc/projects/ms1/docs/ms/submission_natcom/submission_2/v24/supp_figs/S1_sub/S1_rsf_*.pdf

ls /Users/benc/projects/ms1/docs/ms/submission_natcom/submission_2/v24/supp_figs/S1_sub

files=`ls $outP/S1_rsf_*.pdf`
cpdf $files -o $msfigP/supp_figs/Fig_S1.pdf

#---- Figure S2. AKDE, NMDS & Metrics

outP=$msfigP/supp/s2_niches

mkdir -p $outP

years=("2013" "2014" "2016") # 
#years=('2013')
for year in "${years[@]}"
do
  out=${outP}/S2_${year}.pdf
  $srcP/figs/niches.r $hvjob $akde $year 11,10,10 $out --mode=supp
done

#Combine all years into single report

files=`ls $outP/S2_*.pdf`
cpdf $files -o $msfigP/S2_niches.pdf
