
#TODO: put this into howto

#---- lots of work setting up the environment ----#

#Set up the environment
module load miniconda
#source activate parallelR3
conda activate parallelR3
conda install r-rprojroot
conda install r-conflicted #Add here and below to the yml file
conda install r-here
conda install r-tidyselect

conda update r-tidyverse #already in yml

conda update --all #try to upgrade all possible packages. maybe not worth it though

conda update r-haven #might not update to latest newer version
conda update --debug r-haven #this will print out debug information as update tries to solve environment
conda search r-haven --channel conda-forge #see versions available for r-haven on conda-forge
conda install "r-haven>=2.2.0" #try to install a specific version

conda install "r-hms>=0.5.2" #try to install a specific version
conda install "r-vctrs>=0.2.1"
conda install "r-rlang>=0.4.5"
conda install "r-tidyr>=1.0.0" "r-xml2>=1.2.2"
conda install "r-tidyselect>=1.0"

#Can use this to figure out all the dependencies
R --vanilla -e "packageDescription('tidyverse')"
R --vanilla -e "packageDescription('rvest')" #Can also get version. Version: 0.3.4
R --vanilla -e "print(packageDescription('tidyselect')$Version)"

#created this command by copying dependencies from tidyverse. didn't work but I was missing a quote mark.
conda install "r-rvest>=0.3.5" "r-stringr>=1.4.0" "r-tibble>=2.1.3" "r-tidyr>=1.0.0" "r-xml2>=1.2.2"

#Can also try to install a package directly from R if can't install through conda
R
install.packages("tnet", repos="http://cran.r-project.org") #installing via conda did not work. try within R
install.packages("tidyselect", repos="http://cran.r-project.org")

#Set up the script
chmod 744 $src/workflow/bootstrap_niche_metrics.r

srun --pty -p interactive -c 1 -t 0:60:00 --mem-per-cpu=20000 bash

wd=~/projects/ms1/analysis/bootstrap
src=~/projects/ms1/src

cd $wd

module load miniconda
conda activate parallelR3

npts=50
reps=4
runid=test3
out=metrics/$runid/out.csv
axes=pct_tree,pct_bare
dat=data/obs_anno.csv

Rscript --vanilla $src/workflow/bootstrap_niche_metrics.r $dat $out $npts $reps -a $axes -r $runid -s 594 -t

#---- run the script in parallel in interactive mode
srun --pty -p interactive -n 4 bash

wd=~/projects/ms1/analysis/bootstrap
src=~/projects/ms1/src

cd $wd

module load miniconda
conda activate parallelR3

npts=50
reps=4
runid=testmpi
out=metrics/$runid/out.csv
axes=pct_tree,pct_bare
dat=data/obs_anno.csv

mpirun -n 4 Rscript --vanilla $src/workflow/bootstrap_niche_metrics.r $dat $out $npts $reps -a $axes -r $runid -s 594 -p mpi -t


WARNING: 'conda info package_name' is deprecated.
          Use 'conda search package_name --info'.

#

npts=50
reps=4
runid=test2
out=metrics/$runid/out.csv
axes=pct_tree,pct_bare
dat=data/obs_anno.csv
  
mpirun $src/workflow/bootstrap_niche_metrics.r $dat $out $npts $reps -a $axes -r $runid -p mpi -t 

#mpirun ~/projects/rsf/src/scripts/hv/hvs_indiv.r data/obsbg_anno.csv hvs/5axes2000pts1 2000 $axes -p mpi

sbatch $src/