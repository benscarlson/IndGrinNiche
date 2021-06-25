wd=~/projects/ms1/analysis/huj_eobs
src=~/projects/ms1/src
akde=ctmm/akde_contour/contours.shp
sesid=5axes2000pts1 #name of the hypervolume result set
msfigP=~/projects/ms1/docs/ms/submission_natcom/submission_4/v28/figs

cd $wd

#---- Main figures ----#

year=2015

#---- Figure 2. AKDE, NMDS & Metrics
$srcP/figs/niches.r $sesid $akde $year 11,10,10 $msfigP/Fig_2.pdf

#---- Figure 4. Metrics over time
$srcP/figs/metrics_time.r $sesid $msfigP/Fig_3.pdf


#---- Supplementary figures ----#

$srcP/figs/sex_niche_size.r $sesid $msfigP/Fig_S5.pdf
