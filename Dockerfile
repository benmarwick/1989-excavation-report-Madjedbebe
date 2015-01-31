# get the base image, this one has R, RStudio, pandoc, and a bunch of R packages that I use often
FROM rocker/hadleyverse

# required
MAINTAINER Ben Marwick <benmarwick@gmail.com>

# install some packages that not in the base image, these have to be manually identified from my package's Description -> Imports list
RUN apt-get update \
  && apt-get install -y r-cran-Hmisc  r-cran-cluster r-cran-Bolstad2 r-cran-rjags \
  # install a few packages from GitHub for the most recent versions (or if they're not on CRAN)
  && installGithub.r --deps TRUE \
    # install my package that is the focus of this image
    benmarwick/mjb1989excavationpaper

# make a directory to hold the github repo for my package
RUN mkdir /home/rstudio/mjb1989excavationpaper

# Get my package source files from github and download onto Docker. The built package that we already got above is no good because it doesn't have the vignette directory in the same structure as the package source
RUN git clone https://github.com/benmarwick/mjb1989excavationpaper.git   /home/rstudio/mjb1989excavationpaper

# Keep only the vignette folder that has the Rmd file and data files. Because the package has already been loaded, the functions are already present in the environment. So we just want to present to the user the Rmd file and the data so they can knit the Rmd into the document, or work interactively with the code in the Rmd to explore the analysis and the data.
RUN cd /home/rstudio/Steele_et_al_VR003_MSA_Pigments \
  && cp -a /home/rstudio/Steele_et_al_VR003_MSA_Pigments/vignettes/. /home/rstudio/ \
  && cd /home/rstudio/ \
  && rm -r Steele_et_al_VR003_MSA_Pigments

# Set the working directory
WORKDIR /home/rstudio/



# Open the package in RStudio
# RUN rstudio ~/Steele_et_al_VR003_MSA_Pigments.Rproj

# RUN Rscript -e 'rmarkdown::render("/home/rstudio/Steele_et_al_VR003_MSA_Pigments/Marwick-WII13-ochre.Rmd")'

# to build this image:
# docker build -t benmarwick/steeleetalvr003msapigments https://raw.githubusercontent.com/benmarwick/Steele_et_al_VR003_MSA_Pigments/master/vignettes/Dockerfile

# to run this container:
# docker -dp 8787:8787 benmarwick/steeleetalvr003msapigments
# then open broswer at localhost:8787 or http://192.168.59.103:8787/

