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


# Get my package source files from github and download onto Docker. The built package that we already got above is no good because it doesn't have the vignette directory in the same structure as the package source
RUN git clone https://github.com/benmarwick/1989-excavation-report-Madjebebe.git  


# to build this image:
# docker build -t benmarwick/mjb1989excavationpaper https://raw.githubusercontent.com/benmarwick/1989-excavation-report-Madjebebe/master/Dockerfile

# to run this container:
# docker -dp 8787:8787 benmarwick/mjb1989excavationpaper
# then open broswer at localhost:8787 or http://192.168.59.103:8787/

