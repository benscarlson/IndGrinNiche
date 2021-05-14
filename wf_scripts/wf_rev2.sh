
#------------------#
#--- Env Distns ---#
#------------------#

wd=~/projects/ms1/analysis/huj_eobs
src=~/projects/ms1/src
panelOut=figs/env_dist
figOut=~/projects/ms1/docs/ms/submission_natcom/submission_3/v25/figs

cd $wd

#Generate distributions for environmental variables

dat=~/projects/ms1/data/derived/obs_anno.csv

envs=(dist2forest dist2urban ndvi pct_bare pct_tree)

outs=("${envs[@]/%/.pdf}")
outs=("${outs[@]/#/$panelOut/}")

for i in "${!envs[@]}"; do
  echo "Generating plot for ${envs[i]}"
  echo "Saving plot to ${outs[i]}"
  $src/figs/env_dist.r $dat ${outs[i]} ${envs[i]} -t
done

#Combine panels into a single figure
cpdf "${outs[@]}" -o $figOut/S6.pdf

#---------------------------------------------#
#---- Repeatability under null hypotheses ----#
#---------------------------------------------#

#Didn't make a formal script for this yet. See poc/rpt_null.r

#------------------#
#---- Env maps ----#
#------------------#

wd=~/projects/ms1/analysis/huj_eobs
src=~/projects/ms1/src

cd $wd

#---- Process layers ----#

gcs=gs://mol-playground/benc/layer_export/huj_eobs
out=/Volumes/WD4TB/projects/ms1/data/derived/layers

#Exported all of these using layers/export_image.js
gsutil -m cp $gcs/pct_tree.tif $out/pct_tree.tif
gsutil -m cp $gcs/pct_bare.tif $out/pct_bare.tif
gsutil -m cp $gcs/dist2forest.tif $out/dist2forest.tif
gsutil -m cp $gcs/dist2urban.tif $out/dist2urban.tif

#Exported ndvi_mean using layers/ndvi/mosiac_mean
gsutil -m cp $gcs/ndvi_mean.tif $out/ndvi_mean_utm32n.tif

#Exported ndvi_mean using layers/ndvi/mosiac_cv
gsutil -m cp $gcs/ndvi_cv.tif $out/ndvi_cv_utm32n.tif

#--- Set nodata values ---#

gdal_edit.py -a_nodata 255 $layers/pct_tree.tif
gdal_edit.py -a_nodata 255 $layers/pct_bare.tif

#--- Convert wgs84 layers to utm32n

layers=/Volumes/WD4TB/projects/ms1/data/derived/layers

gdalwarp -tr 30 30 -tap -t_srs EPSG:32632 -co COMPRESS=LZW -r near $layers/pct_tree.tif $layers/pct_tree_utm32n.tif
gdalwarp -tr 30 30 -tap -t_srs EPSG:32632 -co COMPRESS=LZW -r near $layers/pct_bare.tif $layers/pct_bare_utm32n.tif
gdalwarp -tr 30 30 -tap -t_srs EPSG:32632 -co COMPRESS=LZW -r near $layers/dist2forest.tif $layers/dist2forest_utm32n.tif
gdalwarp -tr 30 30 -tap -t_srs EPSG:32632 -co COMPRESS=LZW -r near $layers/dist2urban.tif $layers/dist2urban_utm32n.tif

#--- Make the individual panels

wd=~/projects/ms1/analysis/huj_eobs
src=~/projects/ms1/src
figout=/Users/benc/projects/ms1/docs/ms/submission_natcom/submission_3/v27/figs
cd $wd

buf=20000

$src/figs/env_maps.r 11.75705 52.95542 $buf figs/env_panels/beuster
$src/figs/env_maps.r 11.03707 52.44954 $buf figs/env_panels/dromling
$src/figs/env_maps.r 12.05762 52.13555 $buf figs/env_panels/loburg 

panels=(dist2forest.pdf dist2urban.pdf ndvi_mean.pdf ndvi_cv.pdf pct_bare.pdf pct_tree.pdf)

#--- Beuster ---#
figs=${panels[@]/#/figs/env_panels/beuster/}

cpdf $figs -o $figout/S8.pdf

#--- Dromling ---#
figs=${panels[@]/#/figs/env_panels/dromling/}

cpdf $figs -o $figout/S9.pdf

#--- Loburg ---#
figs=${panels[@]/#/figs/env_panels/loburg/}

cpdf $figs -o $figout/S10.pdf




