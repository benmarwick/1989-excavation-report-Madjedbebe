##  ---- libraries


# libraries need to be in a .R file so packrat can find them
# to bundle them in with the repository 

# set repository to download packages from 
r <- getOption("repos")
r["CRAN"] <- "http://cran.rstudio.com/"
options(repos = r)
rm(r)

# function to download packages if they are not 
# available locally

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# The list of packages needed by this repository
pkgs <- c('knitr', 'ggplot2', 'reshape2', 'scales', 'MCMCpack', 'BEST', 'bcp', 'RColorBrewer', 'devtools', 'gridExtra', 'httr', 'memoise', 'whisker')
ipak(pkgs)

# assuming that devtools is now installed...
ipak_github <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    devtools::install_github(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

github_pkgs <- c("rmarkdown")
lapply(github_pkgs, ipak_github)

# this is for reshape2, oddly the repo name (reshape) is different from the 
# package name (reshape2)...
ipak_github_reshape <- function(pkg,pkg2){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    devtools::install_github(new.pkg, dependencies = TRUE)
  sapply(pkg2, require, character.only = TRUE)
}
ipak_github_reshape("reshape", "reshape2")

require(knitr)
require(ggplot2)
require(reshape2)
require(scales)
require(MCMCpack)
require(BEST)
require(bcp)
require(RColorBrewer)
require(gridExtra)

require(devtools)
require(httr)
require(memoise)
require(rmarkdown)
require(whisker)

