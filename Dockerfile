# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.0.0

# required
MAINTAINER Adam Sparks adamhsparks@gmail.com

COPY --chown=rstudio . /home/rstudio/rice_awd_shb

# install system-level libs
RUN apt-get update && \
    apt-get install -y libudunits2-dev libgdal-dev libglpk-dev && \
    rm -r /var/lib/apt/lists/*

# go into the repo directory
RUN . /etc/environment \
  \
 # build this compendium package
  && R -e "devtools::install('home/rstudio/rice_awd_pests', dep=TRUE)" \
  \
  && R -e "rmarkdown::render('/home/rstudio/rice_awd_pests/README.Rmd')" \
 # render the manuscript into a docx, you'll need to edit this if you've
 # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('/home/rstudio/rice_awd_pests/analysis/paper/paper.Rmd')"
