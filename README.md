# Herring

This is an R package that contains all the data and the code
for reproduction of the results in the article "Genetic stock composition of marine bycatch reveals disproportional impacts on depleted river herring genetic stocks" published in CJFAS:
Canadian Journal of Fisheries and Aquatic Sciences, Published on the web 19 November 2015, 10.1139/cjfas-2015-0372, by Daniel J. Hasselman, Eric C Anderson, Emily E Argo, N. David Bethoney, Stephen R Gephard, David M. Post, Bradley P. Schondelmeier, Thomas F Schultz, Theodore V Willis, Eric P Palkovacs

This is available on GitHub, but also it is being prepared for archiving on Dryad.



## Terms 

As a work partially of the United States Government, the R code and the binaries in this
package are in the
public domain within the United States. Additionally, for the source code and binaries we waive
copyright and related rights in the work worldwide through the CC0 1.0
Universal public domain dedication.

The data included in this package are not works of the US Govt and are provided under
CC0 1.0.

See TERMS.md for more information.


## Replicating our results
This repository is designed so that our results can be replicated easily.  Here are the commands that you would use at an R prompt to do that:

###  to get the package from GitHub do the following in R:
```r
#  If you don't have devtools package, install it: install.packages("devtools")

# install the R package "gpiper" that is needed
devtools::install_github("eriqande/gpiper", ref = "3d5a6b5888")

# install the R-package that is this library.  This installs all the data
# and scripts for the analyses in the paper
devtools::install_github("eriqande/herring") 
```

### To install the packages from Dryad:
You need to get the files `herring_0.0.1.tgz` and `gpiper_0.1.tgz` from 
Dryad and then do:
```r
install.packages("path-to-herring_0.0.1.tgz", repos = NULL, dependencies = TRUE)
install.packages("path-to-gpiper_0.1.tgz", repos = NULL, dependencies = TRUE)
```
where you replace the paths with the paths that point to the files on your computer.

### Then do the analyses:

```r
# pdf()  # If running on a remote system without windowing, uncomment this line so the plots don't fail (Note, it won't make all of them pdf!)
```
Then do the mixture simulations:

```r
source(system.file("analysis/mixture-simulations.R", package = "herring"))
```

That should run in about 2 minutes and will produce the files: `{alewife,blueback}_{repu,pop}_mix_sim.pdf` 
 which are four fairly self-explanatory faceted scatter plots.

Finally do the rest of the analyses:

```r
source(system.file("analysis/compile-analysis.R", package = "herring"))
```

That will end up running a fairly long time (30 to 60 minutes, maybe)
because it does multiple runs of the chains 
for the mixed fishery analyses.  Read the script to see what is going on in it.
The script does write out four pdf files that will probably find their way to
the supplement.  They are:

    * `alewife_zhists.pdf`  histograms of the z-scores for fish from mixture vs from baseline for alewife 
    * `blueback_zhists.pdf` histograms of the z-scores for fish from mixture vs from baseline for blueback 
    * `alewife-pi-comp-between-runs.pdf` scatter plots showing posterior mean estimates of mixing proportions for 6 mcmc runs plotted against first run for alewife
    * `blueback-pi-comp-between-runs.pdf`  scatter plots showing posterior mean estimates of mixing proportions for 6 mcmc runs plotted against first run for blueback
    
    
Additionally it prints some results for the bycatch to CSV files.  These are pretty self
explanatory.  They were used to create the tables in the paper.
These files are:

    * `alewife_bycatch_strata_stats.csv`
    * `blueback_bycatch_strata_stats.csv`


