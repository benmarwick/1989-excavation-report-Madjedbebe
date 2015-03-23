<!-- README.md is generated from README.Rmd. Please edit that file -->
1989-excavation-report-Madjebebe
================================

### Compendium URL

<http://dx.doi.org/10.6084/m9.figshare.1297059>

### Author

Ben Marwick (<benmarwick@gmail.com>)

### Contents

This repository contains the research compendium of our work on the 1989 excavation of Malakanuja II (Madgebebe). The compendium contains all data, code, and text associated with this section of the publication (which is currently under review).

### How to use

#### Read the manuscript

See the [`vignettes`](https://github.com/benmarwick/1989-excavation-report-Madjebebe/tree/master/vignettes) directory here on GitHub for source code and data for the manuscript.

#### Install the R package

[![Build Status](https://travis-ci.org/benmarwick/1989-excavation-report-Madjebebe.svg?branch=master)](https://travis-ci.org/benmarwick/1989-excavation-report-Madjebebe)

This repository is organized as an R package, providing functions and raw data to reproduce and extend the analysis reported in the publication. Note that this package has been written explicitly for this project and may not be suitable for more general use. To download the package source as you see it here on GitHub, for offline browsing, use this line at the shell prompt:

``` r
git clone https://github.com/benmarwick/1989-excavation-report-Madjebebe.git
```

Or to install, build and use the package within R, use this line at the R prompt:

``` r
# install.packages("devtools") # which in turn needs Rtools (if Windows) or Xcode (if OSX)
devtools::install_github("benmarwick/1989-excavation-report-Madjebebe", build_vignettes = TRUE)
```

Then you can read the manuscript using this line at the R prompt:

``` r
browseVignettes("mjb1989excavationpaper")
```

This R package has several depedencies that are listed below, some of which need to be installed manually if using this package from your local R installation.

#### Run the Docker container

[![Circle CI](https://circleci.com/gh/benmarwick/1989-excavation-report-Madjebebe.svg?style=shield&circle-token=:circle-token)](https://circleci.com/gh/benmarwick/1989-excavation-report-Madjebebe)

This compendium is also available as a [Docker](https://docs.docker.com/installation) container. The advantage of this format is that it includes this package and all its dependencies already installed, so you don't have to worry about those (e.g. `devtools` Rtools, Xcode, JAGS, etc). OSX & Windows users should launch [`boot2docker`](http://boot2docker.io/) to access the Docker terminal, Linux users can just open any terminal). You can either generate the Docker container yourself using the [Dockerfile](https://github.com/benmarwick/Steele_et_al_VR003_MSA_Pigments/blob/master/vignettes/Dockerfile) included here, or for a quicker start, pull the image from the [online registry](https://registry.hub.docker.com/u/benmarwick/mjb1989excavationpaper/) and run the container using this line at the Docker prompt:

``` r
docker run -dp 8787:8787 benmarwick/mjb1989excavationpaper
```

Then you can interact with RStudio via your browser at localhost:8787 (on Linux) or <http://192.168.59.103:8787/> (on Windows/OSX, or whatever address you get from `boot2docker ip` at the shell prompt). Log in to RStudio with user: `rstudio` and password: `rstudio`. See the [rocker-org Wiki](https://github.com/rocker-org/rocker/wiki/Using-the-RStudio-image) for more details. In RStudio you'll see the `Rmd` file for the manuscript and a directory for the raw data. You can knit the `Rmd` file to produce the HTML file that reproduces the text, plots and other results of the analysis. You can also edit and run the `Rmd` interactively in RStudio to explore the analysis further.

I developed and tested the package on this Docker container, so this is the only platform that I'm confident it works on, and so recommend to anyone wanting to use this package.

### Licenses:

Text: CC-BY-4.0 <http://creativecommons.org/licenses/by/4.0/>

Code: MIT <http://opensource.org/licenses/MIT> year: 2015, copyright holder: Ben Marwick)

Data: CC0 <http://creativecommons.org/publicdomain/zero/1.0/>

### Notes and resources

-   The [issues tracker](https://github.com/benmarwick/1989-excavation-report-Madjebebes) is the place to report problems or ask questions

-   See the repository [history](https://github.com/benmarwick/1989-excavation-report-Madjebebe) for a fine-grained view of progress and changes.

-   The organisation of this compendium is based on the work of [Carl Boettiger](http://www.carlboettiger.info/)

### R Session Information

``` r
sessionInfo()
#> R version 3.1.2 (2014-10-31)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8 LC_NUMERIC=C         LC_TIME=C           
#>  [4] LC_COLLATE=C         LC_MONETARY=C        LC_MESSAGES=C       
#>  [7] LC_PAPER=C           LC_NAME=C            LC_ADDRESS=C        
#> [10] LC_TELEPHONE=C       LC_MEASUREMENT=C     LC_IDENTIFICATION=C 
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#> [1] digest_0.6.8    evaluate_0.5.5  formatR_1.0     htmltools_0.2.6
#> [5] knitr_1.9       rmarkdown_0.5.1 stringr_0.6.2   tools_3.1.2    
#> [9] yaml_2.1.13
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-pc-linux-gnu (64-bit)
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8 LC_NUMERIC=C         LC_TIME=C           
##  [4] LC_COLLATE=C         LC_MONETARY=C        LC_MESSAGES=C       
##  [7] LC_PAPER=C           LC_NAME=C            LC_ADDRESS=C        
## [10] LC_TELEPHONE=C       LC_MEASUREMENT=C     LC_IDENTIFICATION=C 
## 
## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] dplyr_0.4.1                       rioja_0.9-3                      
##  [3] analogue_0.16-0                   vegan_2.2-1                      
##  [5] lattice_0.20-29                   permute_0.8-3                    
##  [7] RColorBrewer_1.1-2                reshape2_1.4.1                   
##  [9] bcp_3.0.1                         Rcpp_0.11.4                      
## [11] iterators_1.0.7                   foreach_1.4.2                    
## [13] BEST_0.2.2                        rjags_3-14                       
## [15] MCMCpack_1.3-3                    MASS_7.3-37                      
## [17] coda_0.17-1                       scales_0.2.4                     
## [19] ggplot2_1.0.0                     mjb1989excavationpaper_0.0.0.9000
## [21] Cairo_1.5-6                       knitr_1.9                        
## 
## loaded via a namespace (and not attached):
##  [1] DBI_0.3.1        Matrix_1.1-5     assertthat_0.1   brglm_0.5-9     
##  [5] cluster_2.0.1    codetools_0.2-10 colorspace_1.2-4 compiler_3.1.2  
##  [9] devtools_1.7.0   digest_0.6.8     evaluate_0.5.5   formatR_1.0     
## [13] gdata_2.13.3     gridExtra_0.9.1  gtable_0.1.2     gtools_3.4.1    
## [17] htmltools_0.2.6  labeling_0.3     lazyeval_0.1.10  magrittr_1.5    
## [21] mgcv_1.8-4       munsell_0.4.2    nlme_3.1-119     parallel_3.1.2  
## [25] plyr_1.8.1       princurve_1.1-12 proto_0.3-10     rmarkdown_0.5.1 
## [29] stringr_0.6.2    tools_3.1.2      yaml_2.1.13
```
