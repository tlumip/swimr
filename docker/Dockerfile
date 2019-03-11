## Create a docker image to run SWIMR
## Build with
##    docker build -t swimr .
## Run with
##    docker run --rm -p 8787:8787 -v /c/Users/matt.landis/shared_data/tlumip_Visum18:/home/rstudio/tlumip_Visum18 swimr

# Build dependencies first (compiling everything)
#
FROM rocker/geospatial:3.3.2

COPY --chown=rstudio /install.R /home/rstudio/swimr/

RUN apt-get update && apt-get install -y \
	libcurl4-gnutls-dev \
	libssh2-1-dev \
  	libssl-dev \
  	zlib1g-dev \
	libgit2-dev \
  	libpng-dev \
  	libtool \
	libxml2-dev

# Install R Packages
RUN Rscript /home/rstudio/swimr/install.R

CMD ["/init"]
