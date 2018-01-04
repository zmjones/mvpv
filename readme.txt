please read all of this!

# general

this dataverse contains two primary tarballs, one which contains the source data files and the output of the data merging process, and another which contains all of the code for the manipulation and analysis of the data. these are named `data.tar.gz` and `R.tar.gz` respectively. both tarballs should be extracted so that there is a 'data' directory and an 'R' directory. many of the files in the 'R' directory are written to expect data to be located in this location. the analysis code was setup to be run on a torque cluster, but has been modified to use a multicore computer, wherein the number of cores is automatically detected. this can be easily modified to run on other sorts of clusters or locally in a parallel or serial manner using the `batchtools` package. please see the `batchtools` documentation for doing this. it will require changing around two lines of code per "job" (modifying the cluster type and its resources). `template.tmpl` may also have to be modified even if you are using a torque cluster. many of the jobs consume a substantial amount of memory, and, depending on your system, it may not be possible to run this. i have put together a file `replicate.R` which is in the parent directory, which "automates" the process of running things a bit, and provides even more documentation.

# uds

we create a "unified democracy score" using x-polity instead of polity. `uds.R` relies on a version of the `uds` package associated with their article which is not on CRAN, but is provided here and can be installed locally (this package is unmaintained by the authors). extract the uds tarball included herein and install from there if you are interested in reproducing this part of our data ingestion/manipulation. `uds.R` produces `uds_xpolity.csv` which is deposited in the `data` directory that should have been created by the extraction of the data tarball. if you would instead like to skip the reproduction of `x-uds` (which we recommend since we are not supporting the uds package and neither are its authors, and this is also very time consuming), just skip this entire step.

# data

data ingestion and joining is done in `data.R`. install all of the relevant packages and then run this file. if your directory structure isn't as specified in the first paragaph, this won't work. the main output of this script is two csv files, "1990_2008_rep.csv" and "1970_2008_rep.csv". the former contains variables which are unavailable over the longer 1970-2008 time frame. this script also provides a csv file for each variable which prints the country code and year of any missing values for easy inspection.

# analysis

`analysis.R` takes the aforementioned data files, estimates a number of models, and then several post-estimation interpretation methods. it is possible to skip any step of this computation since the estimated models, estimates of the partial dependence of specific covariates, etc. are saved as batchtools registry objects that can be loaded using batchtools and are located in the `R` tarball. as an example `fit_bv_registry` contains the results of fitting a multivariate random forest to each pairing of a covariate and the outcomes (jointly). `fit_registry` contains the models used for the main analysis, `pd_registry` contains partial dependence estimates, etc. these outputs are the outputs that are used directly in the paper. you have to delete or move them if you want to re-run `analysis.R`, since, by default, batchtools does not overwrite the registry directory for each batch job. if you want to instead load, say, the models (thus skipping the model fitting step and using the fit models from the paper) you would execute `fit_reg = loadRegistry("fit_registry")`. this should be clear from the batchtools documentation (which i suggest you read if you are replicating this!).

the 'figures' tarball contains... the figures. so many figures.

please also see the github repository for this, which i think is easier to navigate, since it happily accomodates a folder hierarchy. it also allows you to ask me questions via github's issues, etc. at github.com/zmjones/mvpv.
