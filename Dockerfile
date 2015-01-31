# get the base image, this one has R, RStudio, pandoc, and a bunch of R packages that I use often
FROM rocker/hadleyverse

# required
MAINTAINER Ben Marwick <benmarwick@gmail.com>

# install some packages that not in the base image, these have to be manually identified from my package's Description -> Imports list
RUN apt-get update \
  && apt-get install -y  r-cran-rjags \
  # install a few packages from GitHub for the most recent versions (or if they're not on CRAN)
  && installGithub.r --deps TRUE \
    # install my package that is the focus of this image
    benmarwick/1989-excavation-report-Madjebebe

# make a directory to hold the github repo for my package
RUN mkdir /home/rstudio/1989-excavation-report-Madjebebe

# Get my package source files from github and download onto Docker. The built package that we already got above is no good because it doesn't have the vignette directory in the same structure as the package source
RUN git clone https://github.com/benmarwick/1989-excavation-report-Madjebebe.git   /home/rstudio/1989-excavation-report-Madjebebe

# Keep only the vignette folder that has the Rmd file and data files. Because the package has already been loaded, the functions are already present in the environment. So we just want to present to the user the Rmd file and the data so they can knit the Rmd into the document, or work interactively with the code in the Rmd to explore the analysis and the data.
RUN cd /home/rstudio/1989-excavation-report-Madjebebe \
  && cp -a /home/rstudio/1989-excavation-report-Madjebebe/vignettes/. /home/rstudio/ \
  && cd /home/rstudio/ \
  && rm -r 1989-excavation-report-Madjebebe

# Set the working directory
WORKDIR /home/rstudio/


# to build this image:
# docker build -t benmarwick/mjb1989excavationpaper https://raw.githubusercontent.com/benmarwick/1989-excavation-report-Madjebebe/master/Dockerfile

# to run this container:
# docker -dp 8787:8787 benmarwick/mjb1989excavationpaper
# then open broswer at localhost:8787 or http://192.168.59.103:8787/

