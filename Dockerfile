# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:3.4.4

# required
MAINTAINER Your Name adamhsparks@gmail.com

COPY --chown=rstudio . /home/rstudio/rice_awd_pests

# go into the repo directory
RUN . /etc/environment \
  \
 # build this compendium package
  && R -e "devtools::install('home/rstudio/rice_awd_pests', dep=TRUE)" \
  \
 # render the manuscript into a docx, you'll need to edit this if you've
 # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('/home/rstudio/rice_awd_pests/analysis/paper/paper.Rmd')"
