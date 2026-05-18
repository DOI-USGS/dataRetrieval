FROM code.chs.usgs.gov:5001/ctek/docker/r-lang/r-base:4.6

# Change the name of this environment to something which pleases you, if you
# so please. But the name will not be relevant for most cases, as reticulate
# will be pointed to the environment no matter what it is named.
ARG CONDA_ENVIRONMENT_NAME=dataretrieval

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  wget \
  && rm -rf /var/lib/apt/lists/*
  
RUN wget -O Miniforge3.sh "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh" &&  \
    bash Miniforge3.sh -b -p "${HOME}/conda" && \
    rm Miniforge3.sh

# Necessary R libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    r-cran-rcmdcheck \
    r-cran-testthat \
    r-cran-covr \
    r-cran-reticulate \
    r-cran-curl \
    r-cran-lubridate \
    r-cran-xml2 \
    r-cran-httr2 \
    r-cran-sf \
    r-cran-data.table \
    r-cran-jsonlite \
    r-cran-readr \
    && rm -rf /var/lib/apt/lists/* 

ENV RETICULATE_PYTHON=/root/conda/envs/${CONDA_ENVIRONMENT_NAME}/bin/python
